# TODO

## Next phase - REPL improvements

- [x] Run only properly balanced code fragments.
  - [x] If only part of the loop delivered, wait until it will be complete.
  - [x] Use different prompt for that.
- [x] Handle infinite loops.
- [x] Help command.
- [ ] *Interactive mode with stepping by one instruction at a time*:
  - [x] Change the old interactive mode to *auto print* mode.
  - [x] It is a toggle and you have to wait for complete loops as well.
  - [ ] You can execute one instruction at a time.

## Next phase - Compiler

- [ ] Compiler:
  - [ ] Verify that loops are closed before compilation.
  - [ ] Compiling source to the *BEAM* representation.
    - [ ] Compile it to the single function inside module with *fixed* structure.
    - [ ] Helpers for showing transpiled representation and type of the program.

## Next phase - Virtual Machine

- [ ] *Heuristic* optimizations (e.g. rolling up increments / decrements, building up jump table first).
- [ ] *Hot Code* detection.
  - [ ] Basic optimizations based on pattern and usage.

## Next phase - *Brainfork*

- [ ] Adding support for `Y` (*Brainfork*) - it make sense only in the compiler and VM.
  - [ ] Do a detection based on the file extension.
    - [ ] Code provided as a string should be always interpreted as a *Brainfuck*.
  - [ ] For VM - fork uses new *VM* process with cloned state.
  - [ ] For Compiler - fork should `spawn_link` a new process with logic and its own memory.
