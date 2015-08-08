module WebAssembly.AST.Validator

open WebAssembly.AST
open WebAssembly.AST.Module


let _void  = ExpressionTypes.Void
let _bool  = ExpressionTypes.Int32
let _int32 = ExpressionTypes.Int32


type ValidationResult =
  | Ok
  | Error of string


let failedValidation vr =
  match vr with
  | Error e -> Some e
  | Ok      -> None

let allOk results =
  match results |> Seq.tryPick failedValidation with
  | Some e -> Error e
  | None   -> Ok

let checkedValidate validator =
  fun v -> failedValidation (validator v)

let validateList validator list =
  match list |> List.tryPick (checkedValidate validator) with
  | Some e -> Error e
  | None   -> Ok


let checkType expected actual =
  if expected = actual then
    Ok
  else
    Error (sprintf "Expected type %A but type was %A" expected actual)


let getReturnType fd =
  fd.ReturnType

let getArgumentType fd argName =
  fd.Arguments |> List.pick (fun a -> 
    if a.Name.Equals(argName) then
      Some (ExpressionTypes.fromLocal a.Type)
    else
      None
  )

let getLocalType fd localName =
  fd.LocalVariables |> List.pick (fun l -> 
    if l.Name.Equals(localName) then
      Some (ExpressionTypes.fromLocal l.Type)
    else
      None
  )

  
let rec validateExpressionExpecting expectedType expr fd =
  match expr with

  | Get_local l ->
    checkType expectedType (getLocalType fd l)

  | Get_argument a ->
    checkType expectedType (getArgumentType fd a)

  | Set_local(l, e) ->
    fd |> validateExpressionExpecting (getLocalType fd l) e

  | TypeAssertion(t, e) ->
    allOk [
      checkType expectedType t
      (fd |> validateExpressionExpecting t e)
    ]

  | _ ->
    Error (sprintf "unhandled expression type %A" expr)


let rec validateStatement stmt (fd : WebAssembly.AST.Module.FunctionDefinition) =
  match stmt with
  | Expression e ->
    fd |> validateExpressionExpecting _void e

  | If(c, t) ->
    allOk [
      fd |> validateExpressionExpecting _bool c
      fd |> validateStatement t
    ]

  | If_else(c, t, f) ->
    allOk [
      fd |> validateExpressionExpecting _bool c
      fd |> validateStatement t
      fd |> validateStatement f
    ]

  | Do_while(c, b) ->
    allOk [
      fd |> validateExpressionExpecting _bool c
      fd |> validateStatement b
    ]

  | Forever b ->
    fd |> validateStatement b

  | Block b ->
    b  |> validateList (fun s -> validateStatement s fd)

  | Return v ->
    fd |> validateExpressionExpecting (getReturnType fd) v

  | _ ->
    Error (sprintf "unhandled statement type %A" stmt)


let validateFunctionDefinition (fd : WebAssembly.AST.Module.FunctionDefinition) =
  fd |> validateStatement fd.Body

let validateSection (section : WebAssembly.AST.Module.Section) =
  match section with
  | SymbolTable st ->
    Ok

  | FunctionDeclarations d ->
    Ok

  | FunctionDefinitions fd ->
    fd |> validateList validateFunctionDefinition

  | _ ->
    Error (sprintf "unhandled section type %A" section)

let validateTopLevel (topLevel : WebAssembly.AST.Module.TopLevel) =
  (topLevel.Sections |> validateList validateSection)