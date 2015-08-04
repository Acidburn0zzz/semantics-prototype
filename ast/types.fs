module WebAssembly.AST

type Symbol =
  | NamedSymbol of string * int
  | AnonymousSymbol of int


type LocalTypes =
  | Int32
  | Int64
  | Float32
  | Float64

and ExpressionTypes =
  | Void
  | LocalTypes

and MemoryTypes =
  | Int8
  | Int16
  | LocalTypes


type Expression =
  | Int32'load_sx'int8    of Expression
  | Int32'load_sx'int16   of Expression
  | Int32'load_zx'int8    of Expression
  | Int32'load_zx'int16   of Expression
  | Int32'load'int32      of Expression

  | Int64'load_sx'int8    of Expression
  | Int64'load_sx'int16   of Expression
  | Int64'load_sx'int32   of Expression
  | Int64'load_zx'int8    of Expression
  | Int64'load_zx'int16   of Expression
  | Int64'load_zx'int32   of Expression
  | Int64'load'int64      of Expression

  | Float32'load'float32  of Expression
  | Float64'load'float64  of Expression

  | Get_local             of Symbol
  | Get_global            of Symbol

  | Int32'store'int8      of Expression * Expression
  | Int32'store'int16     of Expression * Expression
  | Int32'store'int32     of Expression * Expression

  | Int64'store'int8      of Expression * Expression
  | Int64'store'int16     of Expression * Expression
  | Int64'store'int32     of Expression * Expression
  | Int64'store'int64     of Expression * Expression

  | Float32'store'float32 of Expression * Expression
  | Float64'store'float64 of Expression * Expression

  | Set_local             of Symbol * Expression
  | Set_global            of Symbol * Expression

  | Int32'imm             of System.Int32
  | Int64'imm             of System.Int64
  | Float32'imm           of System.Single
  | Float64'imm           of System.Double

  | Call_direct           of Symbol * Expression list
  | Call_indirect         of Expression * FunctionSignature * Expression list
  | Addressof             of Symbol

  | Comma                 of Expression * Expression
  | Conditional           of Expression * Expression * Expression

and FunctionSignature =
  {
    ReturnType: ExpressionTypes;
    ArgumentTypes: LocalTypes list;
  }

and Block =
  {
    Statements: Statement list;
  }

and Statement =
  | Block
  | Expression
  | If       of Expression * Block
  | Do_while of Expression * Block
  | Forever  of Block
  | Continue
  | Break
  | Return   of Expression
  // FIXME
  | Switch

and Function =
  {
    Name: Symbol;
    Signature: FunctionSignature;
    Body: Block;
  }


let test () =
  let loc = NamedSymbol("loc", 0)
  Float32'store'float32(Get_local(loc), Float32'imm(3.5f))
