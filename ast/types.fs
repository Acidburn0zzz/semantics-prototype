module WebAssembly.AST

type Symbol =
  | NamedSymbol of string
  | AnonymousSymbol of int


type Expression = interface end

type Expression< 'T > = interface 
  inherit Expression
  end


type Int8 = System.SByte

type Int16 = System.Int16

type Int32 = System.Int32

type Int64 = System.Int64

type Float32 = System.Single

type Float64 = System.Double


type LocalTypes =
  | Int32
  | Int64
  | Float32
  | Float64

type ExpressionTypes =
  | Void
  | LocalTypes

type MemoryTypes =
  | Int8
  | Int16
  | LocalTypes


type LocalVariable<'T>(symbol: Symbol) =
  class end

type GlobalVariable<'T>(symbol: Symbol) =
  class end


type Statement =
  | Block
  | Expression

type Block =
  {
    Statements: Statement list;
  }


type FunctionSignature =
  {
    ReturnType: ExpressionTypes;
    ArgumentTypes: LocalTypes list;
  }

type Function =
  {
    Name: Symbol;
    Signature: FunctionSignature;
    Body: Block;
  }


module Abstract =
  type load<'R, 'M> =
    {
      Address: Expression<Int32>;
    }
    interface Expression<'R>

  type store<'M, 'V> =
    {
      Address: Expression<Int32>;
      Value: Expression<'V>;
    }
    interface Expression

  type imm<'V>(Value: 'V) =
    interface Expression<'V>    


type int32'load_sx'int8 =
  Abstract.load<Int32, Int8>
type int32'load_sx'int16 =
  Abstract.load<Int32, Int16>  
type int32'load_zx'int8 =
  Abstract.load<Int32, Int8>
type int32'load_zx'int16 =
  Abstract.load<Int32, Int16>
type int32'load'int32 =
  Abstract.load<Int32, Int32>

type int64'load_sx'int8 =
  Abstract.load<Int64, Int8>
type int64'load_sx'int16 =
  Abstract.load<Int64, Int16>  
type int64'load_sx'int32 =
  Abstract.load<Int64, Int16>  
type int64'load_zx'int8 =
  Abstract.load<Int64, Int8>
type int64'load_zx'int16 =
  Abstract.load<Int64, Int16>  
type int64'load_zx'int32 =
  Abstract.load<Int64, Int16>  
type int64'load'int64 =
  Abstract.load<Int64, Int64>

type float32'load'float32 =
  Abstract.load<Float32, Float32>  
type float64'load'float64 =
  Abstract.load<Float64, Float64>  

type int32'store'int8 =
  Abstract.store<Int32, Int8>
type int32'store'int16 =
  Abstract.store<Int32, Int16>
type int32'store'int32 =
  Abstract.store<Int32, Int32>

type int64'store'int8 =
  Abstract.store<Int64, Int8>
type int64'store'int16 =
  Abstract.store<Int64, Int16>
type int64'store'int32 =
  Abstract.store<Int64, Int32>
type int64'store'int64 =
  Abstract.store<Int64, Int64>

type float32'store'float32 =
  Abstract.store<Float32, Float32>
type float64'store'float64 =
  Abstract.store<Float64, Float64>

type int32'imm =
  Abstract.imm<Int32>
type int64'imm =
  Abstract.imm<Int64>
type float32'imm =
  Abstract.imm<Float32>
type float64'imm =
  Abstract.imm<Float64>

type get_local<'T>(Variable: LocalVariable<'T>) =
  interface Expression<'T>

type set_local<'T> =
  {
    Variable: LocalVariable<'T>;
    Value: Expression<'T>;
  }

type get_global<'T>(Variable: GlobalVariable<'T>) =
  interface Expression<'T>

type store_global<'T> =
  {
    Variable: GlobalVariable<'T>;
    Value: Expression<'T>;
  }


let test () =
  let loc = LocalVariable<Int32>(NamedSymbol("loc"))
  ({ 
    Address = get_local<_>(loc);
    Value = float32'imm(3.5f);
  } : float32'store'float32)
