The control flow based representation of a function.

<!-- TOC -->
  * [What is a control flow graph?](#what-is-a-control-flow-graph)
    * [Example](#example)
  * [SSA Form](#ssa-form)
    * [Example](#example-1)
<!-- TOC -->

## What is a control flow graph?

A control flow graph is a directed graph, which represents the control flow of
a function by putting sequentially executed statements into nodes - called
basic blocks - and connecting them with edges, which represent the possible
transitions between the basic blocks.

Therefore, control flow is modeled explicitly, which allows for a more
detailed analysis of the function's behavior.

### Example

```c
int main() {
  int a = 0;
  int b = 1;
  int c = 2;
  if (a == 0) {
    b = 3;
  } else {
    c = 4;
  }
  return a + b + c;
}
```

The control flow graph of this function would look like this:

```text
            +-------------------+
            |      Block 1      |
            | int a = 0;        |
            | int b = 1;        |
            | int c = 2;        |
            | goto Block 2;     |
            +-------------------+
                      |
                      v
            +-------------------+
            |      Block 2      |
            | if (a == 0)       |
            |   goto Block 3;   |
            | else              |
            |   goto Block 4;   |
            +-------------------+
                   /     \
                  /       \
                 v         v
+-------------------+   +-------------------+
|      Block 3      |   |      Block 4      |
| b = 3;            |   | c = 4;            |
| goto Block 5;     |   | goto Block 5;     |
+-------------------+   +-------------------+
              |               |
              v               v
            +-------------------+
            |      Block 5      |
            | return a + b + c; |
            +-------------------+
                 
```

## SSA Form

We represent instructions in the control flow graph in [SSA form](https://en.wikipedia.org/wiki/Static_single_assignment_form),
meaning that each variable is assigned exactly once. This solves the problem of
needing to check whether a variable is reassigned between two uses of the
variable and therefore makes it easier to reason about the control flow of the program.

### Example

```c
int main() {
  int a = 0;
  int b = 1;
  a = 2;
  return a + b;
}
```

Here you can clearly see, that the first value of `a` is never used.

Without SSA the compiler
would need walk through every possible path in the control flow graph to check
whether the first value of `a` is used or not.

In SSA form the control flow graph would look like this:

```text
            +-------------------+
            |      Block 1      |
            | int a.1 = 0;      |
            | int b = 1;        |
            | int a.2 = 2;      |
            | return a.2 + b;   |
            +-------------------+
```

This makes it trivial for the compiler to see that the first value of `a` is never used, because `a.1` is never referenced.

Of course, as anything else in life, SSA comes with its own set of problems. 
For example, think of a simple loop:

```c
int main() {
  int a = 0;
  while (a < 10) {
    a = a + 1;
  }
  return a;
}
```

In Non-SSA form the translation into a CFG is straight forward:

```text
            +-------------------+
            |      Block 1      |
            | int a = 0;        |
            | goto Block 2;     |
            +-------------------+
                      |
                      v
            +-------------------+
            |      Block 2      |
            | if (a < 10)       |
            |   goto Block 3;   |
            | else              |
            |   goto Block 4;   |
            +-------------------+
               ʌ  |     \
               |  |      \
               |  v       v
+-------------------+   +-------------------+
|      Block 3      |   |      Block 4      |
| a = a + 1;        |   | return a;        |
| goto Block 2;     |   +-------------------+
+-------------------+
```
However, in SSA form, the direct translation is not actually correct.

```text
            +-------------------+
            |      Block 1      |
            | int a.1 = 0;      |
            | goto Block 2;     |
            +-------------------+
                      |
                      v
            +-------------------+
            |      Block 2      |
            | if (a.1 < 10)     |
            |   goto Block 3;   |
            | else              |
            |   goto Block 4;   |
            +-------------------+
               ʌ  |     \
               |  |      \
               |  v       v
+-------------------+   +-------------------+
|      Block 3      |   |      Block 4      |
| int a.2 = a.1 + 1;|   | return a.2;      |
| goto Block 2;     |   +-------------------+
+-------------------+
```

You can see the value of `a` used in the loop condition is always `a.1` and therefore the condition is always true.

To solve this problem, whenever we want to read from a variable `v`, we need to check the last declared version of `v`
in the current block `b` and its predecessors. This is called the reaching definition of `v`. We distinguish:
- If `v` is defined in `b`, we use the last version of `v` in `b`. 
- If `v` is not defined in `b`, we have to check the predecessors of `b`.
  - Case 1: `b` only has one predecessor `p`. We use the reaching definition of `v` in `p`.
  - Case 2: `b` has multiple predecessors. We need to combine the reaching definitions of `v` in all predecessors of `b`.
    which we do using a phi-function. A phi-function (typically notated with Φ) is a function that takes multiple versions of a variable and magically returns the
    correct version of the variable based on the control flow.

Now, the control flow graph in SSA form looks like this:

```text
            +-------------------+
            |      Block 1      |
            | int a.1 = 0;      |
            | goto Block 2;     |
            +-------------------+
                      |
                      v
            +-----------------------+
            |      Block 2          |
            | int a.2 = Φ(a.1, a.3);|
            | if (a.2 < 10)         |
            |   goto Block 3;       |
            | else                  |
            |   goto Block 4;       |
            +-----------------------+
               ʌ  |     \
               |  |      \
               |  v       v
+-------------------+   +-------------------+
|      Block 3      |   |      Block 4      |
| int a.3 = a.2 + 1;|   | return a.3;      |
| goto Block 2;     |   +-------------------+
+-------------------+
```

To see a list of optimizations that can be performed on a control flow graph, see the [optimization](`crate::optimization`) module.
