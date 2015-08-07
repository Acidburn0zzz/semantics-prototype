namespace WebAssembly.AST.Module

open WebAssembly.AST

type LocalVariable =
  {
    Type: LocalTypes;
    Name: Symbol;
  }

type TopLevel =
  {
    Sections: Section list;
  }

and  Section =
  | SymbolTable          of Symbol list
  | FunctionDeclarations of FunctionDeclaration list
  | FunctionDefinitions  of FunctionDefinition  list
  | Unknown              of name: string

and  FunctionDeclaration =
  {
    Name: Symbol;
    ReturnType: LocalTypes;
    ArgumentTypes: LocalVariable list;
  }

and  FunctionDefinition =
  {
    Name: Symbol;
    LocalVariables: LocalVariable list;
    Body: Block;
  }