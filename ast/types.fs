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

  | Int32'add             of Expression * Expression
  | Int32'sub             of Expression * Expression
  | Int32'mul             of Expression * Expression
  | Int32'sdiv            of Expression * Expression
  | Int32'udiv            of Expression * Expression
  | Int32'srem            of Expression * Expression
  | Int32'urem            of Expression * Expression
  | Int32'and             of Expression * Expression
  | Int32'ior             of Expression * Expression
  | Int32'xor             of Expression * Expression
  | Int32'shl             of Expression * Expression
  | Int32'shr             of Expression * Expression
  | Int32'sar             of Expression * Expression
  | Int32'eq              of Expression * Expression
  | Int32'slt             of Expression * Expression
  | Int32'sle             of Expression * Expression
  | Int32'ult             of Expression * Expression
  | Int32'ule             of Expression * Expression
  | Int32'sgt             of Expression * Expression
  | Int32'sge             of Expression * Expression
  | Int32'ugt             of Expression * Expression
  | Int32'uge             of Expression * Expression
  | Int32'clz             of Expression
  | Int32'ctz             of Expression
  | Int32'popcnt          of Expression

  | Int64'add             of Expression * Expression
  | Int64'sub             of Expression * Expression
  | Int64'mul             of Expression * Expression
  | Int64'sdiv            of Expression * Expression
  | Int64'udiv            of Expression * Expression
  | Int64'srem            of Expression * Expression
  | Int64'urem            of Expression * Expression
  | Int64'and             of Expression * Expression
  | Int64'ior             of Expression * Expression
  | Int64'xor             of Expression * Expression
  | Int64'shl             of Expression * Expression
  | Int64'shr             of Expression * Expression
  | Int64'sar             of Expression * Expression
  | Int64'eq              of Expression * Expression
  | Int64'slt             of Expression * Expression
  | Int64'sle             of Expression * Expression
  | Int64'ult             of Expression * Expression
  | Int64'ule             of Expression * Expression
  | Int64'sgt             of Expression * Expression
  | Int64'sge             of Expression * Expression
  | Int64'ugt             of Expression * Expression
  | Int64'uge             of Expression * Expression
  | Int64'clz             of Expression
  | Int64'ctz             of Expression
  | Int64'popcnt          of Expression

  | Float32'add           of Expression * Expression
  | Float32'sub           of Expression * Expression
  | Float32'mul           of Expression * Expression
  | Float32'div           of Expression * Expression
  | Float32'abs           of Expression
  | Float32'neg           of Expression
  | Float32'copysign      of Expression * Expression
  | Float32'ceil          of Expression
  | Float32'floor         of Expression
  | Float32'trunc         of Expression
  | Float32'nearestint    of Expression
  | Float32'eq            of Expression * Expression
  | Float32'lt            of Expression * Expression
  | Float32'le            of Expression * Expression
  | Float32'gt            of Expression * Expression
  | Float32'ge            of Expression * Expression
  | Float32'sqrt          of Expression
  | Float32'min           of Expression * Expression
  | Float32'max           of Expression * Expression

  | Float64'add           of Expression * Expression
  | Float64'sub           of Expression * Expression
  | Float64'mul           of Expression * Expression
  | Float64'div           of Expression * Expression
  | Float64'abs           of Expression
  | Float64'neg           of Expression
  | Float64'copysign      of Expression * Expression
  | Float64'ceil          of Expression
  | Float64'floor         of Expression
  | Float64'trunc         of Expression
  | Float64'nearestint    of Expression
  | Float64'eq            of Expression * Expression
  | Float64'lt            of Expression * Expression
  | Float64'le            of Expression * Expression
  | Float64'gt            of Expression * Expression
  | Float64'ge            of Expression * Expression
  | Float64'sqrt          of Expression
  | Float64'min           of Expression * Expression
  | Float64'max           of Expression * Expression

  | Int32'wrap'int64             of Expression
  | Int32'trunc_signed'float32   of Expression
  | Int32'trunc_signed'float64   of Expression
  | Int32'trunc_unsigned'float32 of Expression
  | Int32'trunc_unsigned'float64 of Expression
  | Int32'reinterpret'float32    of Expression

  | Int64'extend_signed'int32    of Expression
  | Int64'extend_unsigned'int32  of Expression
  | Int64'trunc_signed'float32   of Expression
  | Int64'trunc_signed'float64   of Expression
  | Int64'trunc_unsigned'float32 of Expression
  | Int64'trunc_unsigned'float64 of Expression
  | Int64'reinterpret'float64    of Expression

  | Float32'demote'float64       of Expression
  | Float32'cvt_signed'int32     of Expression
  | Float32'cvt_signed'int64     of Expression
  | Float32'cvt_unsigned'int32   of Expression
  | Float32'cvt_unsigned'int64   of Expression
  | Float32'reinterpret'int32    of Expression

  | Float64'promote'float32      of Expression
  | Float64'cvt_signed'int32     of Expression
  | Float64'cvt_signed'int64     of Expression
  | Float64'cvt_unsigned'int32   of Expression
  | Float64'cvt_unsigned'int64   of Expression
  | Float64'reinterpret'int64    of Expression

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
