module WebAssembly.SExpr

open FParsec

// What I really want here is something along the lines of
//  variant<Expression, Symbol, int, float> but I guess that's not possible in ML?
type Value =
  | Expression of Expression
  | Symbol     of Symbol
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
  with
    override this.ToString() = sprintf "(%s %A)" this.keyword this.arguments
  end


module Parse =
  let LPAREN = pstring "("
  let RPAREN = pstring ")"
  let AT     = pstring "@"

  let read_identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '.' || c = ':'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> spaces

  let read_named_symbol =
    AT >>. read_identifier

  let read_anonymous_symbol =
    AT >>. pint32

  let read_symbol = 
    choice [
      read_named_symbol     |>> Symbol.NamedSymbol;
      read_anonymous_symbol |>> Symbol.AnonymousSymbol;
    ]

  // deal with recursive parser definition
  let read_sexpr, _read_sexpr = 
    createParserForwardedToRef<Expression, unit>()

  let read_int_literal =
    pint64 .>> notFollowedBy (pstring ".")

  let read_float_literal =
    pfloat

  let read_sexpr_argument = 
    choice [
      read_sexpr                 |>> Value.Expression;
      read_symbol                |>> Value.Symbol;
      attempt read_int_literal   |>> Value.Int64;
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