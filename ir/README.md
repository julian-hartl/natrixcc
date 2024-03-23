# Natrix

The general purpose codegen backend of the natrix compiler collection.

[//]: # (## Building)

[//]: # ()
[//]: # (You need unicorn installed: https://www.unicorn-engine.org/docs/)

[//]: # ()
[//]: # (```shell)

[//]: # (brew install unicorn)

[//]: # (```)

[//]: # ()
[//]: # (```shell)

[//]: # (export DYLD_LIBRARY_PATH=/opt/homebrew/Cellar/unicorn/2.0.1.post1/lib:$DYLD_LIBRARY_PATH)

[//]: # (```)

## Intermediate representation reference

### Modules

A module is the top-level container for all other entities.

### Functions

A function is a container for basic blocks and instructions.

### Basic blocks

A basic block is a container for instructions.
Every basic block must end with a terminator instruction.

Basic block identifiers are represented like this:

```
    bb<id>
```

where `<id>` is a unique unsigned 32-bit integer. Note that skipping ids is allowed, but undesired as it can lead to wasted memory in a lot of the internal data structures.

### Instructions

An instruction is usually represented like this:

```
    <vreg> = <opcode> <type> <arg1>, <arg2>, ...;
```

Note the semicolon at the end.

The type can be omitted if an instruction can only produce one type (e.g. `cmp`).
The arguments can be virtual registers, immediate values or additional information for the instruction.

### Virtual registers

Virtual registers are represented like this:

```
    v<id>
```

where `<id>` is a unique unsigned 32-bit integer. Note that skipping ids is allowed, but undesired as it can lead to wasted memory in a lot of the internal data structures.

### Types

The following types are supported:

- `i8` (8-bit signed integer)
- `i16` (16-bit signed integer)
- `i32` (32-bit signed integer)
- `i64` (64-bit signed integer)
- `u8` (8-bit unsigned integer)
- `u16` (16-bit unsigned integer)
- `u32` (32-bit unsigned integer)
- `u64` (64-bit unsigned integer)
- `bool` (boolean)

### Opcodes

#### Arithmetic

##### `add`

Add two values.

```
    <vreg> = add <type> <arg1>, <arg2>
```

Example:

```
    v1 = add i32 v2, v3
```

##### `sub`

Subtract two values.

```
    <vreg> = sub <type> <arg1>, <arg2>
```

Example:

```
    v1 = sub i32 v2, v3
```

#### Comparison

##### `icmp`

Compare two integer values.

```
    <vreg> = icmp <predicate> <type> <arg1>, <arg2>
```

Example:

```
    v1 = icmp eq i32 v2, v3
```

The following predicates are supported:

- `eq` (equal)
- `gt` (greater than)

#### Control flow

##### `br`

Unconditional branch.

```
    br bb<id>
```

Example:

```
    br bb1
```

##### `condbr`

Conditional branch.

```
    condbr <vreg>, bb<id>, bb<id>
```

Example:

```
    condbr v1, bb1, bb2
```

##### `ret`

Return from the current function.

```
    ret <type> <vreg>
```

Example:

```
    ret i32 v1
```

### Examples

You can find examples of the intermediate representation in the [examples](./examples) directory.
