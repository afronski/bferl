# TODO

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

- [x] *README* review.
- [x] *Architecture* overview.
- [ ] *REPL* and *Interpreter* overview.
- [ ] *REPL* and *Interpreter* internals.
  - [ ] Example session
- [ ] *Compiler* overview.
- [ ] *Compiler* internals.
  - [ ] Example session
- [ ] *Virtual Machine* overview.
- [ ] *Virtual Machine* internals.
  - [ ] Example session
- [x] *Brainfork* overview.
- [ ] *Brainfork* internals.
  - [ ] Example session

## Next phase - Improvements

- [ ] *Dialyzer*:
  - [ ] Find way to exclude Lexer / Parser includes from *dialyzer* analysis.
  - [ ] Fix all *dialyzer* errors related to project.
- [ ] Improvements:
  - [ ] *REPL* and *Interpreter* - Detecting infinite loop based on *IC* observation.
  - [ ] *Compiler* - Safe pointers operations in `debug` mode.
  - [ ] *Compiler* - Pretty print *Core Erlang* to file.
  - [ ] *Compiler* - Optimization stages.
    - [ ] Removing unused code.
    - [ ] Rolling up increments/decrements, pointer movements.
