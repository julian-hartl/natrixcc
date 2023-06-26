# Fusion-Lang

### Today

- [x] Implement Fix for binary operator associativity (as explained in this [PR](https://github.com/julian-hartl/fusion-lang/pull/1) by [@martinkauppinen](https://github.com/martinkauppinen)) 
- [x] Remove AST Prefix from ast components
- [x] Use IndexVec<Symbol> instead of HashMap<String, Symbol>
- [x] Refactor scoping mechanism to use SymbolIdx instead of cloned Symbol value
- [x] Refactor ast to use IndexVec
- Give every ast node a type (Statement will just be void)
- [x] Use Item instead of Statement for top level ast nodes
  - An item could be one of the following:
    - Function declaration
    - Statement
- [x] Make everything an expression
- Gather function symbols during parsing (we have all the information we need) instead of in a separate pass
- Add tests for evaluator

