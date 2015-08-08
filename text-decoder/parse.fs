module WebAssembly.AST.Parse

open FParsec
open WebAssembly
open WebAssembly.SExpr
open WebAssembly.SExpr.Parse
open WebAssembly.AST
open WebAssembly.AST.Module
open System

let read_numbered_symbol scope = 
  (pstring "@") >>.
  choice [
    attempt read_identifier     |>> Symbols.newNamedSymbolInScope scope;
    attempt (pint32 .>> spaces) |>> AST.Symbol.AnonymousSymbol;
  ]

let read_block scope =
  read_sexpr |>>
    (fun se ->
      match WebAssembly.AST.FromSExpr.blockFromSExpr scope se with
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

let read_expressionType =
  (pstring ":") >>. choice [
    enumerant "void"    ExpressionTypes.Void;
    enumerant "int32"   ExpressionTypes.Int32;
    enumerant "int64"   ExpressionTypes.Int64;
    enumerant "float32" ExpressionTypes.Float32;
    enumerant "float64" ExpressionTypes.Float64;
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
      // name
      (read_numbered_symbol scope)
      // return type
      read_expressionType
      // argument types
      (readMany read_localType)
      (fun a b c ->
        {
          Name          = a;
          ReturnType    = b;
          ArgumentTypes = c;
        } : FunctionDeclaration
      )
    )
  )

let read_declarations scope =
  sectionName "declarations" >>.
    (readMany (read_declaration scope)) |>> FunctionDeclarations

let read_definition_body scope functionName =
  let childScope = Symbols.makeChildScope scope

  (pipe4
    // return type
    read_expressionType
    // (args ...)
    (read_argument_declarations childScope)
    // optional (locals ...)
    (attempt (read_local_declarations childScope) <|>% [])
    // function body
    (read_block childScope)

    (fun a b c d ->
      {
        Name           = functionName;
        ReturnType     = a;
        Arguments      = b;
        LocalVariables = c;
        Body           = d;
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
  let moduleScope = Symbols.makeScope ()
  spaces >>. (readManyAbstract (read_section moduleScope)) |>> (fun sections -> { Sections = sections })

let topLevelFromString str =  
  run (read_topLevel ()) str