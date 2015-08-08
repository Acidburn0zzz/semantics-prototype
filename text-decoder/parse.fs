module WebAssembly.AST.Parse

open FParsec
open WebAssembly
open WebAssembly.SExpr
open WebAssembly.SExpr.Parse
open WebAssembly.AST.Module
open Microsoft.FSharp.Reflection
open System
open System.Reflection
open System.Collections.Generic


[<AllowNullLiteral>]
type SymbolScope(parent: SymbolScope) =
  class
    member val table = new Dictionary<string, int>()

    member this.TryGet(name) =
      let (exists, id) = this.table.TryGetValue(name)
      if exists then
        (true, id)
      elif not (parent = null) then
        parent.TryGet(name)
      else
        (false, 0)

    member this.GetIndexOffset() =
      if parent = null then
        this.table.Count
      else
        this.table.Count + parent.GetIndexOffset()

    member this.Add(name) =
      let newId = this.GetIndexOffset();
      this.table.Add(name, newId)
      newId

  end

let makeScope () =
  new SymbolScope(null)

let makeChildScope parent =
  new SymbolScope(parent)

let assignNumberToSymbol (scope : SymbolScope) name =
  let (exists, id) = scope.TryGet(name)
  if exists then
    id
  else
    scope.Add(name)

let existingNamedSymbolInScope (scope : SymbolScope) name =
  let (exists, id) = scope.TryGet(name)
  if exists then
    AST.Symbol.NamedSymbol(id, name)  
  else
    raise (new Exception(sprintf "No symbol named '%s' defined in this scope" name))

let newNamedSymbolInScope scope name =
  let idx = assignNumberToSymbol scope name
  AST.Symbol.NamedSymbol(idx, name)  


type ParseResult<'T> =
  | Success of 'T
  | Failure of error: string


let _expression_lookup_table = ref null
let _statement_lookup_table  = ref null


// FIXME: gross
let duFromString (cases: Dictionary<string, UnionCaseInfo>) (s:string) =
  let (found, case) = cases.TryGetValue(s)
  if found then
    Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
  else
    None

let duFromKeyword cases (kw:Value) =
  match kw with
  | Keyword s -> duFromString cases s
  | _         -> None

let duGetCases (ty:Type) =
  let result = new Dictionary<string, UnionCaseInfo>(StringComparer.InvariantCultureIgnoreCase)
  ignore (FSharpType.GetUnionCases ty |> Array.map (fun c -> result.Add(c.Name, c)))
  result


let astSymbolFromSExprSymbol scope s =
  match s with
  | SExpr.NamedSymbol     name -> 
    // Any names used inside an AST expression should have already been defined
    existingNamedSymbolInScope scope name
  | SExpr.AnonymousSymbol idx  -> AST.Symbol.AnonymousSymbol(idx)


let rec _lookupTableCaseCtor<'T> untypedCaseCtor parseArguments caseName scope sExpr =
  let caseCtorArgs = parseArguments scope sExpr

  let result = (untypedCaseCtor(caseCtorArgs) : obj)
  result :?> 'T

and     _makeArgumentParser (ty:Type) =
  if ty = typeof<AST.Expression> then
    (fun scope (v : Value) ->
      match v with
      | Expression se -> 
        (
          match expressionFromSExpr scope se with
          | Success expr -> Success (box expr)
          | Failure err  -> Failure (err)
        )
      | _             ->
        Failure (sprintf "Expected expression, got %A" v)
    )
  elif ty = typeof<AST.Symbol> then
    (fun scope (v : Value) ->
      match v with
      | Symbol s -> Success (box (astSymbolFromSExprSymbol scope s))
      | _        -> Failure (sprintf "Expected symbol, got %A" v)
    )
  elif ty = typeof<AST.NumericLiteral> then
    (fun scope (v : Value) ->
      match v with
      | Int32 i32   -> Success (box (NumericLiteral.Int32(i32)))
      | Int64 i64   -> Success (box (NumericLiteral.Int64(i64)))
      | Float f     -> Success (box (NumericLiteral.Float64(f)))
      | _           -> Failure (sprintf "Expected int or float literal, got %A" v)
    )
  elif ty = typeof<AST.Statement> then
    (fun scope (v: Value) ->
      match v with
      | Expression se -> 
        (
          match blockFromSExpr scope se with
          | Success expr -> Success (box expr)
          | Failure err  -> Failure (err)
        )
      | _             -> Failure (sprintf "Expected block or statement, got %A" v)
    )
  else
    let cases = duGetCases ty

    (fun scope (v : Value) -> 
      match duFromKeyword cases v with
      | Some du -> Success (box du)
      | _       -> Failure (sprintf "Expected keyword, got %A" v)
    )

and     _makeLookupTableCaseCtor<'T> case =
  let untypedCaseCtor = FSharpValue.PreComputeUnionConstructor case
  let caseName = case.Name.ToLowerInvariant()
  let caseFields = case.GetFields()

  let argumentParsers =
    Array.map (fun (p : PropertyInfo) -> _makeArgumentParser p.PropertyType) caseFields

  let parseArguments = (fun scope (sExpr : SExpr.Expression) ->
    if not (sExpr.arguments.Length = caseFields.Length) then
      raise (
        new ArgumentException(
          String.Format(
            "{0} expects {1} argument(s), got {2}", caseName, caseFields.Length, sExpr.arguments.Length
          )
        )
      )

    let result = Array.zeroCreate caseFields.Length
    for i = 0 to caseFields.Length - 1 do
      let parsedValue = argumentParsers.[i] scope sExpr.arguments.[i];
      match parsedValue with
      | Success v ->
        result.[i] <- v;
      | Failure err ->
        raise (
          new ArgumentException(
            String.Format("Error while parsing argument {0} of {1}: {2}", caseFields.[i].Name, caseName, err)
          )
        )

    result
  )

  let ctor = (_lookupTableCaseCtor<'T> untypedCaseCtor parseArguments caseName)
  (caseName, ctor)

and     _makeLookupTable<'T> () =
  let table = new Dictionary<string, Func<SymbolScope, SExpr.Expression, 'T>>()
  let cases = FSharpType.GetUnionCases(typeof<'T>)

  for case in cases do
    let (caseName, ctor) = _makeLookupTableCaseCtor<'T> case
    table.Add(caseName, Func<SymbolScope, SExpr.Expression, 'T>(ctor))

  table


and     getExpressionLookupTable () =
  if _expression_lookup_table.Value = null then
    (_expression_lookup_table := _makeLookupTable<AST.Expression> ())
    _expression_lookup_table.Value
  else
    _expression_lookup_table.Value

and     expressionFromSExpr scope sExpr =
  let nameParts = sExpr.keyword.ToLowerInvariant().Split([| '.' |], 2)
  let name = nameParts.[nameParts.Length - 1]

  let table = getExpressionLookupTable ()
  let (found, ctor) = table.TryGetValue(name)

  if found then
    let result = ctor.Invoke(scope, sExpr)

    if nameParts.Length > 1 then
      // FIXME: Slow
      let cases = duGetCases typeof<LocalTypes>
      let expectedType = duFromString cases nameParts.[0]
      match expectedType with
      | Some lt ->
        let assertion = TypeAssertion(lt, result)
        Success (assertion)
      | _ ->
        Failure (sprintf "Invalid type assertion '%s' in '%s'" nameParts.[0] sExpr.keyword)
    else      
      Success (result)
  else
    Failure (sprintf "No expression type named '%s'" name)

and     getStatementLookupTable () =
  if _statement_lookup_table.Value = null then
    _statement_lookup_table := _makeLookupTable<AST.Statement> ()
    _statement_lookup_table.Value
  else
    _statement_lookup_table.Value

and     statementFromSExpr scope sExpr =
  let name = sExpr.keyword.ToLowerInvariant()
  let table = getStatementLookupTable ()
  let (found, ctor) = table.TryGetValue(name)

  if found then
    let result = ctor.Invoke(scope, sExpr)
    Success (result)
  else
    let maybeExpression = expressionFromSExpr scope sExpr
    match maybeExpression with
    | Success expr ->
      Success (AST.Statement.Expression expr)
    | Failure err ->
      Failure (sprintf "No statement type named '%s'; %s" name err)

and     blockFromSExpr scope sExpr =
  let name = sExpr.keyword.ToLowerInvariant()

  if name = "block" then
    let err = ref null
    let result = Block(sExpr.arguments |> List.map (fun (elt : Value) ->
      match elt with
      | Expression expr ->
        match statementFromSExpr scope expr with
        | Success stmt ->
          stmt
        | Failure e ->
          err := e
          Statement.Void

      | _ ->
        err := sprintf "Expected expression in block, got %A" elt
        Statement.Void
      )        
    )

    if err.Value = null then
      Success result
    else
      Failure err.Value

  else
    statementFromSExpr scope sExpr

let read_numbered_symbol scope = 
  (pstring "@") >>.
  choice [
    attempt read_identifier     |>> newNamedSymbolInScope scope;
    attempt (pint32 .>> spaces) |>> AST.Symbol.AnonymousSymbol;
  ]

let read_block scope =
  read_sexpr |>>
    (fun se ->
      match blockFromSExpr scope se with
      | Success stmt -> stmt
      | Failure err  -> raise (new Exception(err))
    )

let sectionName s =
  attempt (pstring s .>> spaces)

let read_symbolTable scope =
  sectionName "symbols" >>. spaces >>.
  (
    (readMany (read_numbered_symbol scope)) |>> SymbolTable
  )

let enumerant n v =
  (pstring n) >>. (preturn v)

let read_localType =
  (pstring ":") >>. choice [
    enumerant "int32"   LocalTypes.Int32;
    enumerant "int64"   LocalTypes.Int64;
    enumerant "float32" LocalTypes.Float32;
    enumerant "float64" LocalTypes.Float64;
  ] .>> spaces

let read_variable_declaration scope =
  pipe2 
    read_localType (read_numbered_symbol scope)
    (fun t s -> { Name = s; Type = t } : LocalVariable)

let read_argument_declarations scope =
  readAbstractNamed "args" (
    spaces >>.
    readMany (read_variable_declaration scope)
  )

let read_local_declarations scope =
  readAbstractNamed "locals" (
    spaces >>.
    readMany (read_variable_declaration scope)
  )

let read_declaration scope =
  readAbstractNamed "declaration" (
    (pipe3
      // return type
      read_localType
      // name
      (read_numbered_symbol scope)
      // argument types
      (readMany read_localType)
      (fun a b c ->
        {
          ReturnType    = a;
          Name          = b;
          ArgumentTypes = c;
        } : FunctionDeclaration
      )
    )
  )

let read_declarations scope =
  sectionName "declarations" >>.
    (readMany (read_declaration scope)) |>> FunctionDeclarations

let read_definition_body scope functionName =
  let childScope = makeChildScope scope

  (pipe3
    // (args ...)
    (read_argument_declarations childScope)
    // optional (locals ...)
    (attempt (read_local_declarations childScope) <|>% [])
    // function body
    (read_block childScope)

    (fun a b c ->
      {
        Name           = functionName;
        Arguments      = a;
        LocalVariables = b;
        Body           = c;
      } : FunctionDefinition
    )
  )

let read_definition scope =
  readAbstractNamed "definition" (
    // We read the function name, then *invoke* read_definition_body
    //  this ensures that each function definition has its own child scope
    (read_numbered_symbol scope) >>= (read_definition_body scope)
  )

let read_definitions scope =
  sectionName "definitions" >>.
    (readMany (read_definition scope)) |>> FunctionDefinitions

let read_section scope =
  pstring "section." >>.
  choice [
    read_symbolTable scope;
    read_declarations scope;
    read_definitions scope;
  ]

let read_topLevel () = 
  let moduleScope = makeScope ()
  spaces >>. (readManyAbstract (read_section moduleScope)) |>> (fun sections -> { Sections = sections })

let topLevelFromString str =  
  run (read_topLevel ()) str