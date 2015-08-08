module WebAssembly.AST.FromSExpr


open WebAssembly
open WebAssembly.SExpr
open WebAssembly.AST
open Microsoft.FSharp.Reflection
open System
open System.Reflection;
open System.Collections.Generic


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
    Symbols.existingNamedSymbolInScope scope name
  | SExpr.AnonymousSymbol idx  -> AST.Symbol.AnonymousSymbol(idx)


let rec _lookupTableCaseCtor<'T> untypedCaseCtor parseArguments caseName scope sExpr =
  let caseCtorArgs = parseArguments scope sExpr

  let result = (untypedCaseCtor(caseCtorArgs) : obj)
  result :?> 'T

and     _makeArgumentParser (ty:Type) =
  if ty = typeof<AST.Expression> then
    (fun scope (v : Value) ->
      match v with
      | Value.Expression se -> 
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
      | Value.Symbol s -> Success (box (astSymbolFromSExprSymbol scope s))
      | _        -> Failure (sprintf "Expected symbol, got %A" v)
    )
  elif ty = typeof<AST.NumericLiteral> then
    (fun scope (v : Value) ->
      match v with
      | Value.Int32 i32   -> Success (box (NumericLiteral.Int32(i32)))
      | Value.Int64 i64   -> Success (box (NumericLiteral.Int64(i64)))
      | Value.Float f     -> Success (box (NumericLiteral.Float64(f)))
      | _           -> Failure (sprintf "Expected int or float literal, got %A" v)
    )
  elif ty = typeof<AST.Statement> then
    (fun scope (v: Value) ->
      match v with
      | Value.Expression se -> 
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
      let cases = duGetCases typeof<ExpressionTypes>
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
      | Value.Expression expr ->
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