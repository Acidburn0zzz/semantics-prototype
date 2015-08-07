module WebAssembly.AST.Parse

open FParsec
open WebAssembly
open WebAssembly.SExpr
open WebAssembly.SExpr.Parse
open WebAssembly.AST.Module
open Microsoft.FSharp.Reflection
open System
open System.Collections.Generic


let rec _lookupTableCaseCtor<'T> untypedCaseCtor caseFieldCount caseName sExpr =
  let caseCtorArgs = Array.zeroCreate 0
  if not (caseCtorArgs.Length = caseFieldCount) then
    raise (
      new ArgumentException(
        String.Format(
          "{0} expects {1} argument(s), got {2}", caseName, caseFieldCount, caseCtorArgs.Length
        )
      )
    )

  let result = (untypedCaseCtor(caseCtorArgs) : obj)
  result :?> 'T

let rec _makeLookupTableCaseCtor<'T> case =
  let untypedCaseCtor = FSharpValue.PreComputeUnionConstructor case
  let caseName = case.Name.ToLowerInvariant()
  let caseFields = case.GetFields()
  let ctor = (_lookupTableCaseCtor<'T> untypedCaseCtor caseFields.Length caseName)
  (caseName, ctor)

let rec _makeLookupTable<'T> () =
  let table = new Dictionary<string, Func<SExpr.Expression, 'T>>()
  let cases = FSharpType.GetUnionCases(typeof<'T>)

  for case in cases do
    let (caseName, ctor) = _makeLookupTableCaseCtor<'T> case
    table.Add(caseName, Func<SExpr.Expression, 'T>(ctor))

  table


let _expression_lookup_table = 
  ref (null : Dictionary<string, Func<SExpr.Expression, AST.Expression>>)

let rec getExpressionLookupTable () =
  if _expression_lookup_table.Value = null then
    (_expression_lookup_table := _makeLookupTable<AST.Expression> ())
    _expression_lookup_table.Value
  else
    _expression_lookup_table.Value

let expressionFromSExpr sExpr =
  let name = sExpr.keyword.ToLowerInvariant()
  let table = getExpressionLookupTable ()
  let (found, ctor) = table.TryGetValue(name)

  if found then
    let result = ctor.Invoke(sExpr)
    Some result
  else
    printfn "No expression type named '%s'" name
    None    

let _statement_lookup_table =
  ref (null : Dictionary<string, Func<SExpr.Expression, AST.Statement>>)

let rec getStatementLookupTable () =
  if _statement_lookup_table.Value = null then
    _statement_lookup_table := _makeLookupTable<AST.Statement> ()
    _statement_lookup_table.Value
  else
    _statement_lookup_table.Value

let statementFromSExpr sExpr =
  let name = sExpr.keyword.ToLowerInvariant()
  let table = getStatementLookupTable ()
  let (found, ctor) = table.TryGetValue(name)

  if found then
    let result = ctor.Invoke(sExpr)
    Some result
  else
    let maybeExpression = expressionFromSExpr sExpr
    match maybeExpression with
    | Some e ->
      Some (AST.Statement.Expression e)
    | None ->
      printfn "No statement type named '%s'" name
      None

let read_block =
  readAbstractNamed "block" (
    (readMany read_sexpr) |>>
    (fun sExprs ->
      printfn "(block %A)" sExprs;
      ({
        Statements = List.choose statementFromSExpr sExprs
      } : Block)
    )
  )


let sectionName s =
  attempt (pstring s .>> spaces)

let read_symbolTable =
  sectionName "symbols" >>. spaces >>.
  (
    (readMany read_symbol) |>> SymbolTable
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

let read_local_declaration = 
  read_localType .>> (opt read_symbol)

let read_argument_types =
  readAbstractNamed "args" (
    spaces >>.
    readMany read_local_declaration
  )

let read_local_types =
  readAbstractNamed "locals" (
    spaces >>.
    readMany read_local_declaration
  )

let read_declaration =
  readAbstractNamed "declaration" (
    (pipe3 
      read_symbol
      read_localType 
      read_argument_types
      (fun a b c ->
        {
          Name          = a;
          ReturnType    = b;
          ArgumentTypes = c;
        } : FunctionDeclaration
      )
    )
  )

let read_declarations =
  sectionName "declarations" >>.
    (readMany read_declaration) |>> FunctionDeclarations

let read_definition =
  readAbstractNamed "definition" (
    (pipe3 
      read_symbol
      read_local_types
      read_block
      (fun a b c ->
        {
          Name          = a;
          VariableTypes = b;
          Body          = c;
        } : FunctionDefinition
      )
    )
  )

let read_definitions =
  sectionName "definitions" >>.
    (readMany read_definition) |>> FunctionDefinitions

let read_section =
  pstring "section:" >>. choice [
    read_symbolTable;
    read_declarations;
    read_definitions;
  ]

let read_topLevel = 
  spaces >>. (readManyAbstract read_section) |>> (fun sections -> { Sections = sections })

let topLevelFromString str =  
  run read_topLevel str