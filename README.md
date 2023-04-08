# Fusion-Lang

## Roadmap

### 07.04.2023

- [x] Add support for basic arithmetic operations

```
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

### Next stream

- [ ] Add if statements
    ```
    let x = 30 
    if x > 10 {
        x = 10
    } else {
        x = 0
    }
    ```

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