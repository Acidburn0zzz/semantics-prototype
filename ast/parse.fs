module WebAssembly.AST.Parse

open FParsec
open WebAssembly
open WebAssembly.SExpr
open WebAssembly.SExpr.Parse
open WebAssembly.AST.Module
open Microsoft.FSharp.Reflection


let symbolFromValue v =
  match v with
  | Symbol s -> Some s
  | _ -> None

let functionDeclarationFromValue v =
  match v with
  | Value.Expression s ->
    None
  | _ -> None

let functionDefinitionFromValue v =
  match v with
  | Value.Expression s ->
    None
  | _ -> None

let sectionFromSExpr sExpr =
  let sectionName = sExpr.keyword.ToLowerInvariant();

  match sectionName with
  | "symboltable" ->
    SymbolTable(sExpr.arguments |> List.choose symbolFromValue)

  | "functiondeclarations" ->
    FunctionDeclarations(sExpr.arguments |> List.choose functionDeclarationFromValue)

  | "functiondefinitions" ->
    FunctionDefinitions(sExpr.arguments |> List.choose functionDefinitionFromValue)

  | _ ->
    Unknown(sExpr.keyword)

let sectionName s =
  attempt (pstring s)

let read_symbol_table =
  sectionName "symbols" >>. spaces >>.
  (
    (readMany read_symbol) |>> SymbolTable
  )

let read_localType =
  read_keyword |>> (fun kw ->
    match kw with
    | "int32"   -> LocalTypes.Int32
    | "int64"   -> LocalTypes.Int64
    | "float32" -> LocalTypes.Float32
    | "float64" -> LocalTypes.Float64
  )

let read_argument_declaration = 
  read_localType .>> (opt read_symbol)

let read_argument_types =
  readAbstractNamed "args" (
    spaces >>.
    readMany read_argument_declaration
  )

let read_declaration =
  readAbstractNamed "declaration" (
    (pipe3 
      read_symbol
      read_localType
      read_argument_types
      (fun name returnType argumentTypes ->
        printfn "argumentTypes %A" argumentTypes;
        {
          Name          = name
          ReturnType    = returnType;
          ArgumentTypes = argumentTypes;
        } : FunctionDeclaration
      )
    )
  )

let read_declarations =
  sectionName "declarations" >>. spaces >>. 
    (readMany read_declaration) |>> FunctionDeclarations

let read_definitions =
  sectionName "definitions" >>. spaces |>> (fun s -> FunctionDefinitions([]))

let read_section =
  pstring "section:" >>. 
    choice [
      read_symbol_table;
      read_declarations;
      read_definitions;
    ]

let read_toplevel = 
  spaces >>. (readManyAbstract read_section) |>> (fun sections -> { Sections = sections })

let topLevelFromString str =  
  run read_toplevel str