module WebAssembly.AST

type Symbol =
  | NamedSymbol of string
  | AnonymousSymbol of int
and  LocalVariable     = Symbol
and  GlobalVariable    = Symbol
and  FunctionReference = Symbol

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


type LoadExtensionTypes =
  | NoExtend
  | SignExtend
  | ZeroExtend


type Address   = Expression
and  Condition = Expression

and  Expression =
  | Get_local             of LocalVariable
  | Set_local             of LocalVariable  * Expression

  | Get_global            of GlobalVariable
  | Set_global            of GlobalVariable * Expression

  | Load                  of MemoryTypes * Address * LoadExtensionTypes
  | Store                 of MemoryTypes * Address * Expression

  | Immediate             of NumericLiteral

  | Call_direct           of FunctionReference * Expression list
  | Call_indirect         of Expression * FunctionSignature * Expression list
  | Addressof             of FunctionReference

  | Comma                 of Expression * Expression
  | Conditional           of Condition  * Expression * Expression

  // Int or float operations
  | Add                   of Expression * Expression
  | Mul                   of Expression * Expression
  | Sub                   of Expression * Expression
  | Eq                    of Expression * Expression
  | Slt                   of Expression * Expression
  | Sle                   of Expression * Expression
  | Sgt                   of Expression * Expression
  | Sge                   of Expression * Expression
  | Div                   of Expression * Expression

  // Int-only operations
  | Udiv                  of Expression * Expression
  | Srem                  of Expression * Expression
  | Urem                  of Expression * Expression
  | And                   of Expression * Expression
  | Ior                   of Expression * Expression
  | Xor                   of Expression * Expression
  | Shl                   of Expression * Expression
  | Shr                   of Expression * Expression
  | Sar                   of Expression * Expression
  | Ult                   of Expression * Expression
  | Ule                   of Expression * Expression
  | Ugt                   of Expression * Expression
  | Uge                   of Expression * Expression
  | Clz                   of Expression
  | Ctz                   of Expression
  | Popcnt                of Expression

  // Float-only operations
  | Abs                   of Expression
  | Neg                   of Expression
  | Copysign              of Expression * Expression
  | Ceil                  of Expression
  | Floor                 of Expression
  | Trunc                 of Expression
  | Nearestint            of Expression
  | Sqrt                  of Expression
  | Min                   of Expression * Expression
  | Max                   of Expression * Expression

  | Cvt_signed            of Expression
  | Cvt_unsigned          of Expression
  | Reinterpret           of Expression

  | Wrap                  of Expression
  | Demote                of Expression
  | Promote               of Expression

  | Extend_signed         of Expression
  | Extend_unsigned       of Expression

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
  | If       of Condition  * Block
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