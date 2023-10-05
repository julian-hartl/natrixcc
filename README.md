# Fusion-Lang

### Stream #7

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

### Stream #8
- [x] Make functions expressions
  ```
    let add = func (a: int, b: int) -> int {
      return a + b
    }
    let sum = add(1, 2)
    let test = func (anotherFunc: (int, int) -> int) -> int {
      return anotherFunc(1, 2)
    }
    sum
  ```


### Stream #9
- Rethink language design
  - I'd like it not to become too functional - Therefore we should reconsider the current way of function declarations
Instead of
```
let add = func(a: int, b: int) -> int {
  return a + b
}
```
We could do
```
func add(a: int, b: int) -> int {
  return a + b
}
```
- Refactor symbol resolving => Move to parsing stage because we have all the information (except for types) at that point

