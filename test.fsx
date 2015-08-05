#I "libs/FParsec"
#r "FParsec.dll"
#r "FParsecCS.dll"
#load "ast/sexpr.fs" "ast/types.fs"

open FParsec

let parseSExpr str = 
  printfn "// '%s'\n" str
  match WebAssembly.SExpr.fromString str with
  | Success(expr, _, _)         -> 
    printfn "%s\n" (expr.ToString())
    printfn "%A\n" expr
  | Failure(errorMessage, _, _) -> printfn "Failed: %s" errorMessage

parseSExpr "(setlocal @localVar (add 5.0 3))"
parseSExpr "(section:functions (function @six (return (add 5.0 1))))"
parseSExpr "(numbers 0x12AF 0xFFEEDDCCBBAA 3.71)"

parseSExpr "(test"
parseSExpr "(a (b (c 37 @a) @6) (@foo"
parseSExpr "@w"
parseSExpr "(a @()))"