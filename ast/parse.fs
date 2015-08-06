module WebAssembly.AST.Parse

open FParsec
open WebAssembly
open WebAssembly.SExpr
open WebAssembly.SExpr.Parse
open WebAssembly.AST.Module
open Microsoft.FSharp.Reflection


let read_block =
  readAbstractNamed "block" (
    (readMany read_sexpr) |>>
    (fun sexprs ->
      printfn "(block %A)" sexprs;
      ({
        Statements = []
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