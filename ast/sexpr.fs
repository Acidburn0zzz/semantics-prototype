module WebAssembly.SExpr

open FParsec
open System
open System.Text

// What I really want here is something along the lines of
//  variant<Expression, Symbol, int, float> but I guess that's not possible in ML?
type Value =
  | Expression of Expression
  | Symbol     of Symbol
  | Int32      of int32
  | Int64      of int64
  | Float      of float

and Symbol =
  | NamedSymbol     of string
  | AnonymousSymbol of int

and Expression =
  {
    keyword: string;
    arguments: Value list
  }


module Parse =
  let LPAREN = pstring "("
  let RPAREN = pstring ")"
  let AT     = pstring "@"

  let read_identifier =
    let isIdentifierFirstChar c = 
      isLetter c || 
      c = '_'

    let isIdentifierChar c = 
      isLetter c || 
      isDigit c ||
      isAnyOf "_.:" c

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> spaces

  let read_symbol = 
    AT >>.
    choice [
      attempt read_identifier |>> Symbol.NamedSymbol;
      attempt pint32          |>> Symbol.AnonymousSymbol;
    ]

  // deal with recursive parser definition
  let read_sexpr, _read_sexpr = 
    createParserForwardedToRef<Expression, unit>()

  let read_int32_literal =
    pint32 .>> notFollowedBy (pstring ".")

  let read_int64_literal =
    pint64 .>> notFollowedBy (pstring ".")

  let read_float_literal =
    pfloat

  let read_sexpr_argument = 
    choice [
      read_sexpr                 |>> Value.Expression;
      read_symbol                |>> Value.Symbol;
      attempt read_int32_literal |>> Value.Int32;
      attempt read_int64_literal |>> Value.Int64;
      attempt read_float_literal |>> Value.Float;
    ]

  let read_sexpr_arguments =
    many (read_sexpr_argument .>> spaces)

  let read_sexpr_body =
    pipe2 (read_identifier) 
      (read_sexpr_arguments)
      (fun keyword arguments -> { keyword = keyword; arguments = arguments })

  do _read_sexpr := (
      LPAREN >>. read_sexpr_body .>> spaces .>> RPAREN
    )


let     symbolToStringInto (sb:StringBuilder) s =
  match s with
  | NamedSymbol n     -> sb.AppendFormat("@{0}", n)
  | AnonymousSymbol i -> sb.AppendFormat("@{0}", i)

let rec valueToStringInto (sb:StringBuilder) v =
  ignore (sb.Append(" "))

  match v with
  | Expression e      -> toStringInto sb e
  | Symbol s          -> symbolToStringInto sb s
  | Int32  i32        -> sb.Append(i32)
  | Int64  i64        -> sb.Append(i64)
  | Float  f          -> sb.AppendFormat("{0}f", f)

and     toStringInto sb e =
  ignore (
    sb.Append("(")
      .Append(e.keyword)
  );

  (e.arguments |> Seq.fold valueToStringInto sb)
    .Append(")")

let toString e =
  (toStringInto (StringBuilder()) e)
    .ToString();


let fromString str =
  run Parse.read_sexpr str