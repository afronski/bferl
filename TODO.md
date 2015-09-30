# TODO

## Next phase - Compiler

- [x] Compiler facility.
- [x] Helpers for showing transpiled representation and type of the program.
- [x] Lexer and Parser.
  - [x] Verify that loops are closed during compilation.
- [x] Code Generation
  - [x] Compiling source to the *Core Erlang* representation.
    - [x] Compile it to the single function inside module with *fixed* structure.
  - [x] Compiling code in `debug` and `release` modes.
  - [x] Change execution mechanism to `lists:foldl/3`.
  - [x] Create state tuple in `build_state/1` and return `IC` afterwards.
  - [ ] Language logic implemented in *Core Erlang*.

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

## Next phase - Documentation

- [ ] Documentation:
  - [x] *Architecture* and overview documentation.
  - [x] *REPL* and *Interpreter*.
  - [ ] *Compiler* internals and *Core Erlang* description.
  - [x] *Brainfork* annotations.
  - [ ] *Virtual Machine* and implemented optimizations.
  - [ ] *Brainfork* internals.

## Next phase - Improvements

- [ ] *Dialyzer*:
  - [ ] Find way to exclude Lexer / Parser includes from *dialyzer* analysis.
  - [ ] Fix all *dialyzer* errors related to project.
- [ ] Improvements:
  - [ ] *REPL* and *Interpreter* - Detecting infinite loop based on IC observation.
  - [ ] *Compiler* - Optimization stage.
