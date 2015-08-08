module WebAssembly.AST.Validator

open WebAssembly.AST
open WebAssembly.AST.Module


let _void    = ExpressionTypes.Void
let _address = ExpressionTypes.Int32
let _bool    = ExpressionTypes.Int32
let _int32   = ExpressionTypes.Int32


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

let checkIntegral et =
  match et with
  | ExpressionTypes.Int32
  | ExpressionTypes.Int64 ->
    Ok
  | ExpressionTypes.Void
  | ExpressionTypes.Float32
  | ExpressionTypes.Float64 ->
    Error "Expected integral type"


let getReturnType fd =
  fd.ReturnType

let isMatchingVariable name =
  fun (v : LocalVariable) ->
    if v.Name.Equals(name) then
      Some (ExpressionTypes.fromLocal v.Type)
    else
      None

let getArgumentType fd argName =
  fd.Arguments |> List.pick (isMatchingVariable argName)

let getLocalType fd localName =
  fd.LocalVariables |> List.pick (isMatchingVariable localName)

  
let rec validateExpressionExpecting expectedType expr fd =
  match expr with
  | Get_argument arg ->
    checkType expectedType (getArgumentType fd arg)

  | Get_local loc ->
    checkType expectedType (getLocalType fd loc)

  | Set_local(loc, value) ->
    fd |> validateExpressionExpecting (getLocalType fd loc) value

  | Load(memoryType, address)
  | Load_extended(memoryType, address, _) ->
    // FIXME: Check memory type against expected type
    fd |> validateExpressionExpecting _address address

  | Store(memoryType, address, localType, value) ->
    let et = ExpressionTypes.fromLocal localType
    // FIXME: Check memory type against value type
    allOk [
      fd |> validateExpressionExpecting _address address
      fd |> validateExpressionExpecting et value   
    ]

  | Immediate(localType, value) ->
    let et = ExpressionTypes.fromLocal localType
    checkType expectedType et

  // TODO: Call, addressof

  | Comma(a, b) ->
    allOk [
      fd |> validateExpressionExpecting _void a
      fd |> validateExpressionExpecting expectedType b
    ]

  | Conditional(cond, t, f) ->
    allOk [
      fd |> validateExpressionExpecting _bool cond
      fd |> validateExpressionExpecting expectedType t
      fd |> validateExpressionExpecting expectedType f
    ]

  | Eq (localType, lhs, rhs)
  | Slt(localType, lhs, rhs)
  | Sle(localType, lhs, rhs)
  | Sgt(localType, lhs, rhs)
  | Sge(localType, lhs, rhs) ->
    let et = ExpressionTypes.fromLocal localType
    allOk [
      checkType expectedType _bool
      fd |> validateExpressionExpecting et lhs
      fd |> validateExpressionExpecting et rhs
    ]

  | Ult(localType, lhs, rhs)
  | Ule(localType, lhs, rhs)
  | Ugt(localType, lhs, rhs)
  | Uge(localType, lhs, rhs) ->
    let et = ExpressionTypes.fromLocal localType
    allOk [
      checkType expectedType _bool
      checkIntegral et
      fd |> validateExpressionExpecting et lhs
      fd |> validateExpressionExpecting et rhs
    ]

  | Add (lhs, rhs)
  | Mul (lhs, rhs)
  | Sub (lhs, rhs)
  | Sdiv(lhs, rhs)
  | Udiv(lhs, rhs)
  | Srem(lhs, rhs)
  | Urem(lhs, rhs)
  | And (lhs, rhs)
  | Ior (lhs, rhs)
  | Xor (lhs, rhs)
  | Shl (lhs, rhs)
  | Shr (lhs, rhs)
  | Sar (lhs, rhs) ->
    allOk [
      checkIntegral expectedType
      fd |> validateExpressionExpecting expectedType lhs
      fd |> validateExpressionExpecting expectedType rhs
    ]

  | Assert_type(assertedType, value) ->
    allOk [
      checkType expectedType assertedType
      fd |> validateExpressionExpecting assertedType value
    ]

  | _ ->
    Error (sprintf "unhandled expression type %A" expr)


let rec validateStatement stmt (fd : WebAssembly.AST.Module.FunctionDefinition) =
  let result =
    match stmt with
    | Expression e ->
      fd |> validateExpressionExpecting _void e

    | If(cond, t) ->
      allOk [
        fd |> validateExpressionExpecting _bool cond
        fd |> validateStatement t
      ]

    | If_else(cond, t, f) ->
      allOk [
        fd |> validateExpressionExpecting _bool cond
        fd |> validateStatement t
        fd |> validateStatement f
      ]

    | Do_while(cond, b) ->
      allOk [
        fd |> validateExpressionExpecting _bool cond
        fd |> validateStatement b
      ]

    | Forever b ->
      fd |> validateStatement b

    | Block b ->
      b  |> validateList (fun s -> validateStatement s fd)

    | Return v ->
      fd |> validateExpressionExpecting (getReturnType fd) v

    | _ ->
      Error (sprintf "unhandled statement type: %A" stmt)

  match result with
  | Ok ->
    result
  | Error e ->
    Error (sprintf "Validation failed for statement: %A\n%s" stmt e)

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