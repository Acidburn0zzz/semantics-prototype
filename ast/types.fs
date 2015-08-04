module WebAssembly.AST

type Symbol =
  | NamedSymbol of string
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


type NumericLiteral =
  | Int32   of System.Int32
  | Int64   of System.Int64
  | Float32 of System.Single
  | Float64 of System.Double


type Expression =
  | Load_sx               of Expression
  | Load_zx               of Expression
  | Load                  of Expression

  | Get_local             of Symbol
  | Get_global            of Symbol

  | Store                 of Expression * Expression

  | Set_local             of Symbol * Expression
  | Set_global            of Symbol * Expression

  | Immediate             of NumericLiteral

  | Call_direct           of Symbol * Expression list
  | Call_indirect         of Expression * FunctionSignature * Expression list
  | Addressof             of Symbol

  | Comma                 of Expression * Expression
  | Conditional           of Expression * Expression * Expression

  | Add                   of Expression * Expression
  | Sub                   of Expression * Expression
  | Mul                   of Expression * Expression
  | Sdiv                  of Expression * Expression
  | Udiv                  of Expression * Expression
  | Srem                  of Expression * Expression
  | Urem                  of Expression * Expression
  | And                   of Expression * Expression
  | Ior                   of Expression * Expression
  | Xor                   of Expression * Expression
  | Shl                   of Expression * Expression
  | Shr                   of Expression * Expression
  | Sar                   of Expression * Expression
  | Eq                    of Expression * Expression
  | Slt                   of Expression * Expression
  | Sle                   of Expression * Expression
  | Ult                   of Expression * Expression
  | Ule                   of Expression * Expression
  | Sgt                   of Expression * Expression
  | Sge                   of Expression * Expression
  | Ugt                   of Expression * Expression
  | Uge                   of Expression * Expression
  | Clz                   of Expression
  | Ctz                   of Expression
  | Popcnt                of Expression

  | Fdiv                  of Expression * Expression
  | Fabs                  of Expression
  | Fneg                  of Expression
  | Copysign              of Expression * Expression
  | Ceil                  of Expression
  | Floor                 of Expression
  | Trunc                 of Expression
  | Nearestint            of Expression
  | Sqrt                  of Expression
  | Fmin                  of Expression * Expression
  | Fmax                  of Expression * Expression

  | Cvt_signed            of Expression
  | Cvt_unsigned          of Expression
  | Reinterpret           of Expression

  | Int32'wrap'int64      of Expression

  | Float32'demote'float64      of Expression
  | Float64'promote'float32     of Expression

  | Int64'extend_signed'int32   of Expression
  | Int64'extend_unsigned'int32 of Expression

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
  let loc  = NamedSymbol("loc")
  let glob = NamedSymbol("global")
  let f    = Immediate(Float32(3.5f))
  let f2   = Immediate(Float32(2.7f))
  [
    Set_local(loc, f);
    Set_global(
      glob,
      Add(Get_local(loc), f2)
    )
  ]
