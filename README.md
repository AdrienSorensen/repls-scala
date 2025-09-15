| __ \ | |
| |) | | | ___
| _ // _ | |/ _ \
| | \ \ () | | __/
|| __/|_|_|
Scala REPLs: Int & MultiSet

# Scala REPLs

A modular Scala REPL framework that supports **integers** and **multisets**, with parsing, evaluation, variable assignment, and expression simplification.

## Features
- Interactive REPL for two domains:
  - **IntREPL**: integer arithmetic with `+`, `-`, `*`
  - **MultiSetREPL**: multiset operations (`+` union, `-` subtraction, `*` intersection)
- Variable assignment (`x = 3 * 4`)
- Simplification rules (e.g. `a - a = 0`, `{}` as empty multiset, identities with 0 and 1)
- Parsing expressions with normal operator precedence
- Extensible design via a common abstract base class

## Run
Clone the repo and compile:

```bash
scalac *.scala
