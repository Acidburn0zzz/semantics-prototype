namespace WebAssembly.AST

// FIXME: Named symbols should be assigned an index by the parser
type Symbol =
  | AnonymousSymbol of int
  | NamedSymbol     of int * string

and  LocalVariable     = Symbol
and  GlobalVariable    = Symbol

type LocalTypes =
  | Int32
  | Int64
  | Float32
  | Float64

and  FromLocalType = LocalTypes
and  ToLocalTypes  = LocalTypes

and  ExpressionTypes =
  | Void
  | Int32
  | Int64
  | Float32
  | Float64

and  MemoryTypes =
  | Int8
  | Int16
  | Int32
  | Int64
  | Float32
  | Float64

and  FromMemoryType = MemoryTypes
and  ToMemoryType   = MemoryTypes


type NumericLiteral =
  | Int32   of System.Int32
  | Int64   of System.Int64
  | Float32 of System.Single
  | Float64 of System.Double


type LengthExtensionTypes =
  | SignExtend
  | ZeroExtend


type Address   = Expression
and  Condition = Expression

and  Expression =
  | Get_argument          of Symbol

  | Get_local             of LocalVariable
  | Set_local             of LocalVariable  * Expression

  | Get_global            of GlobalVariable
  | Set_global            of GlobalVariable * Expression

  | Load                  of Address    * FromMemoryType
  | LoadExtended          of Address    * FromMemoryType * LengthExtensionTypes

  // This seems wrong. Should Store accept the target type in LocalType? Gross asymmetry
  | Store                 of Address    * ToMemoryType * Expression
  | StoreConverted        of Address    * ToMemoryType * Expression * FromLocalType

  | Immediate             of LocalTypes * NumericLiteral

  | Call_direct           of Symbol     * arguments: (Expression list)
  | Call_indirect         of Expression * returnType: LocalTypes * argumentTypes: (LocalTypes list) * arguments: (Expression list)
  | Addressof             of Symbol

  | Comma                 of Expression * Expression
  | Conditional           of Condition  * Expression * Expression

  // Int or float operations
  | Eq                    of Expression * Expression
  | Slt                   of Expression * Expression
  | Sle                   of Expression * Expression
  | Sgt                   of Expression * Expression
  | Sge                   of Expression * Expression

  // Int-only operations
  | Add                   of Expression * Expression
  | Mul                   of Expression * Expression
  | Sub                   of Expression * Expression
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
  | Ult                   of Expression * Expression
  | Ule                   of Expression * Expression
  | Ugt                   of Expression * Expression
  | Uge                   of Expression * Expression
  | Clz                   of Expression
  | Ctz                   of Expression
  | Popcnt                of Expression

  // Float-only operations
  | Fadd                  of Expression * Expression
  | Fmul                  of Expression * Expression
  | Fsub                  of Expression * Expression
  | Fdiv                  of Expression * Expression
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

  // Mixed int/float operations
  | Trunc_signed          of FromLocalType * Expression
  | Trunc_unsigned        of FromLocalType * Expression
  | Cvt_signed            of FromLocalType * Expression
  | Cvt_unsigned          of FromLocalType * Expression
  | Reinterpret           of FromLocalType * Expression

  // These are only valid for a single type so they're unambiguous
  | Wrap                  of Expression
  | Demote                of Expression
  | Promote               of Expression
  | Extend                of Expression * LengthExtensionTypes

  // Compile-time type assertion
  | TypeAssertion         of LocalTypes * Expression

and Statement =
  | Block      of Statement list
  | Expression of Expression
  | If         of Condition * Statement
  | If_else    of Condition * Statement * Statement
  | Do_while   of Condition * Statement
  | Forever    of Statement
  | Return     of Expression
  | Void
  // FIXME
  | Continue
  | Break
  | Switch
