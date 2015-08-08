#I "libs/FParsec"
#r "FParsec.dll"
#r "FParsecCS.dll"
#load "sexpr/sexpr.fs" 
      "ast/expressions.fs" "ast/module.fs"
      "text-decoder/symbols.fs" "text-decoder/sexpr-to-ast.fs" "text-decoder/parse.fs"
      "semantics/validate.fs"

open FParsec
open WebAssembly
open WebAssembly.AST.Validator

let parseSExpr str = 
  printfn "// \"%s\" \n" str
  match (SExpr.fromString str) with
  | Success(expr, _, _)         -> 
    printfn "%s\n" (SExpr.toString expr)
    printfn "%A\n" expr
  | Failure(errorMessage, _, _) -> printfn "Failed: %s" errorMessage

let validateTopLevel topLevel =
  match (WebAssembly.AST.Validator.validateTopLevel topLevel) with
  | Ok        ->
    printfn "Validation successful\n"
    printfn "%A\n" topLevel
  | Error(msg, location) ->
    printfn "Module validation failed: %s\n  at location: %A\n" msg location

let parseModule str =
  printfn "// \"\"\"%s\"\"\" \n" str
  match (AST.Parse.topLevelFromString str) with
  | Success(topLevel, _, _)         ->
    validateTopLevel topLevel
  | Failure(errorMessage, _, _) -> printfn "Failed: %s" errorMessage
 
parseModule """
(section.symbols
  @add
)
(section.declarations
  (declaration @add :int32 :int32 :int32)
  (declaration @write_into :void :int32 :int32 :int32)
)
(section.definitions
  (definition
    @add :int32
    (args
      :int32 @lhs
      :int32 @rhs
    )
    (locals
      :int32 @result
    )
    (block
      (set_local
        @result
        (int32.add (int32.get_argument @lhs) (int32.get_argument @rhs))
      )
      (return (int32.get_local @result))
    )
  )
  (definition
    @write_into :void
    (args
      :int32 @destination
      :float32 @lhs
      :int32 @rhs
    )
    (block
      (if_else 
        (immediate :int32 1)
        (store 
          :int32 (int32.get_argument @destination)
          :int32 (int32.add (int32.get_argument @lhs) (int32.get_argument @rhs))
        )
        (block
          (store 
            :int32 (int32.get_argument @destination)
            :int32 (int32.sub (int32.get_argument @lhs) (int32.get_argument @rhs))
          )
        )
      )
    )
  )
)"""

(*
parseSExpr "(setlocal @localVar (add 5.0 3))"
parseSExpr "(section:functions (function @six (return (add 5.0 1))))"
parseSExpr "(numbers 0x12AF 0xFFEEDDCCBBAA 3.71)"
parseSExpr "(store 0x0010 :int16 (immediate :int32 123))"

parseSExpr "(test"
parseSExpr "(a (b (c 37 @a) @6) (@foo"
parseSExpr "@w"
parseSExpr "(a @()))"
*)