namespace WebAssembly.AST.Module

open WebAssembly.AST

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
    ArgumentTypes: LocalTypes list;
  }

and  FunctionDefinition =
  {
    Name: Symbol;
    VariableTypes: LocalTypes list;
    Body: Block;
  }