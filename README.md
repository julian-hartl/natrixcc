# Fusion-Lang

## Roadmap

### 07.04.2023

- [x] Add support for basic arithmetic operations
  ```rust
  7 - (30 + 7) * 8 / 2
  ```
### 08.04.2023

- [x] Add support for `let` statements

 ```
 let x = 30 * (8 - 1)
 let y = 30
 let z = x + y
 ```

- [x] Add error reporting

### 09.04.2023

- [ ] Add if statements

 ```
 let x = 30 
 let b = if x > 10 {
     x = 10
     10
 } else {
     x = 0
     2
 }
 ```

What do we have to consider?

- New type: boolean
- Conditional executing of statements
- Scoping

- [ ] Add while loops

 ```
 let x = 0
 while x < 10 {
     x = x + 1
 }
 ```

- [ ] Add scoping

 ```
 let x = 0
 {
     let x = 10
 }
 ```

### Next stream

- [ ] Add types & type checking

 ```
 let x: int = 10
 let y: bool = false
 let z: string = "Hello World"
 let a = 10 // type inference => will be int
 ```

- [ ] IR Lowering

 ```
 let x = 10
 let y = 20
 if x > y {
     x = 20
 } else {
     x = 10
 }
 ...
 ```

 ```
 func main() {
     x = 10
     y = 20
     gotoIfFalse x > y else
     x = 20
     goto end
     else: 
     x = 10
     end:
 }
 ...
 ```

- [ ] Add strings

 ```
 let hello_world = "Hello world\""
 ```
