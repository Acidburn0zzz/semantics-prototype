#r "FParsec.dll"
#load "ast/sexpr.fs" "ast/types.fs"

open FParsec

let parseSExpr str = 
  printfn "// '%s'\n" str
  match WebAssembly.SExpr.fromString str with
  | Success(expr, _, _)         -> printfn "%A\n" expr
  | Failure(errorMessage, _, _) -> printfn "Failed: %s" errorMessage

parseSExpr "(setlocal @localVar (add 5.0 3))"
parseSExpr "(section:functions (function (return (add 5.0 1))))"

parseSExpr "(test"
parseSExpr "(test (test (test 37 @a) @6) (@foo"