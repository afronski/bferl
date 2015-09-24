# TODO

## Next phase - Compiler

- [x] Compiler facility.
- [x] Helpers for showing transpiled representation and type of the program.
- [ ] Lexer, Parser and Code Generation.
  - [ ] Verify that loops are closed during compilation.
- [ ] Compiling source to the *Core Erlang* representation.
  - [ ] Compile it to the single function inside module with *fixed* structure.
- [ ] Fixing `dialyzer` errors.

## Next phase - Virtual Machine

- [ ] *Virtual Machine* facility.
  - [ ] Server and internal representation.
  - [ ] *Heuristic* optimizations.
    - [ ] Rolling up increments / decrements.
    - [ ] Rolling up pointer movements.
    - [ ] Building up jump table first.
    - [ ] *Hot Code* detection.
      - [ ] Loops unwinding and body compilation.

## Next phase - *Brainfork*

- [ ] Adding support for `Y` (*Brainfork*) - it make sense only in the compiler and VM.
  - [x] Do a detection based on the file extension.
  - [x] Code provided as a string should be always interpreted as a *Brainfuck*.
  - [ ] For VM - fork uses new *VM* process with cloned state.
  - [ ] For Compiler - fork should `spawn_link` a new process with logic and its own memory.
