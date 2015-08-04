module WebAssembly.SExpr

open FParsec
open System.Text

// What I really want here is something along the lines of
//  variant<Expression, Symbol, int, float> but I guess that's not possible in ML?
type Value =
  | Expression of Expression
  | Symbol     of Symbol
  | Int32      of int32
  | Int64      of int64
  | Float      of float
  with
    override this.ToString() =
      match this with
      | Expression e      -> e.ToString()
      | Symbol s          -> s.ToString()
      | Int32  i32        -> i32.ToString()
      | Int64  i64        -> i64.ToString()
      | Float  f          -> f.ToString()
  end

and Symbol =
  | NamedSymbol     of string
  | AnonymousSymbol of int
  with
    override this.ToString() =
      match this with
      | NamedSymbol s     -> sprintf "@%s" s
      | AnonymousSymbol i -> sprintf "@%i" i
  end

and Expression =
  {
    keyword: string;
    arguments: Value list
  }
  with
    override this.ToString() =
      let result = 
        StringBuilder()
          .AppendFormat("({0}", this.keyword.ToString());

      ignore (this.arguments |>
        Seq.fold (fun (sb:System.Text.StringBuilder) (v:Value) -> sb.AppendFormat(" {0}", v.ToString())) result);

      result.Append(")")
        .ToString();
  end


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


let fromString str =
  run Parse.read_sexpr str