namespace WebAssembly.AST

open WebAssembly
open System
open System.Collections.Generic


type ParseResult<'T> =
  | Success of 'T
  | Failure of error: string


[<AllowNullLiteral>]
type SymbolScope(parent: SymbolScope) =
  class
    member val table = new Dictionary<string, int>()

    member this.TryGet(name) =
      let (exists, id) = this.table.TryGetValue(name)
      if exists then
        (true, id)
      elif not (parent = null) then
        parent.TryGet(name)
      else
        (false, 0)

    member this.GetIndexOffset() =
      if parent = null then
        this.table.Count
      else
        this.table.Count + parent.GetIndexOffset()

    member this.Add(name) =
      let newId = this.GetIndexOffset();
      this.table.Add(name, newId)
      newId

  end

module Symbols =
  let makeScope () =
    new SymbolScope(null)

  let makeChildScope parent =
    new SymbolScope(parent)

  let assignNumberToSymbol (scope : SymbolScope) name =
    let (exists, id) = scope.TryGet(name)
    if exists then
      id
    else
      scope.Add(name)

  let existingNamedSymbolInScope (scope : SymbolScope) name =
    let (exists, id) = scope.TryGet(name)
    if exists then
      AST.Symbol.NamedSymbol(id, name)  
    else
      raise (new Exception(sprintf "No symbol named '%s' defined in this scope" name))

  let newNamedSymbolInScope scope name =
    let idx = assignNumberToSymbol scope name
    AST.Symbol.NamedSymbol(idx, name)  