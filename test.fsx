#I "libs/FParsec"
#r "FParsec.dll"
#r "FParsecCS.dll"
#load "ast/sexpr.fs" "ast/expressions.fs" "ast/module.fs" "ast/parse.fs"

open FParsec
open WebAssembly

let parseSExpr str = 
  printfn "// \"%s\" \n" str
  match (SExpr.fromString str) with
  | Success(expr, _, _)         -> 
    printfn "%s\n" (SExpr.toString expr)
    printfn "%A\n" expr
  | Failure(errorMessage, _, _) -> printfn "Failed: %s" errorMessage

let parseModule str =
  printfn "// \"\"\"%s\"\"\" \n" str
  match (AST.Parse.topLevelFromString str) with
  | Success(expr, _, _)         -> 
    printfn "%A\n" expr
  | Failure(errorMessage, _, _) -> printfn "Failed: %s" errorMessage
 
parseModule """
(section:symbols
  @add
)
(section:declarations
  (declaration 
    @add 
    :int32 
    (args
      :int32 @lhs
      :int32 @rhs
    )
  )
)
(section:definitions
  (definition
    @add
    (locals
      :int32 @result
    )
    (block
      (set_local
        @result
        (add (get_argument @lhs) (get_argument @rhs))
      )
      (return (get_local @result))
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