# Natrix Compiler Collection

A collection of common compiler tooling designed and developed for educational purposes.

*Note: The compiler collection is currently work in progress and far away from being finsihed. It is missing any kind of documentation and is not tested very well yet.*

<!-- TOC -->
* [Natrix compiler collection](#natrix-compiler-collection)
  * [General Purpose Codegen Backend](#general-purpose-codegen-backend)
  * [Example C-Like Language Frontend](#natrix-language-frontend)
  * [Example Package Manager](#example-package-manager)
<!-- TOC -->

## Educational Resources

## General Purpose Codegen Backend

See [here](./ir/README.md) for more information.

## Natrix Language Frontend

See [here](./lang/README.md) for more information.

## Example Package Manager

Yet to be implemented.

## Project setup

### Setup git commit hooks

To manage git hooks, we use [Rusty Hook](https://github.com/swellaby/rusty-hook).

To install Rusty Hook, run the following command:

```bash
cargo install rusty-hook
```

To install the git hooks, run the following command:

```bash
rusty-hook init
```
