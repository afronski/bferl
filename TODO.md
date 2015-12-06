# TODO

## Next phase - Virtual Machine

- [ ] *Virtual Machine* facility.
  - [x] Server and helpers API.
  - [x] Internal representation, preprocessing and stages.
    - [x] Translating opcodes to IR (*Internal representation*).
    - [x] Different supervisor (`simple_one_for_one`) which manages VM threads.
    - [x] Different process for representing VM thread.
    - [x] Internal structure for the VM (different than interpreter one).
    - [x] Preparing main logic of execution.
    - [x] Implementation of basic opcodes (all besides loops and calls).
    - [ ] Implementation for looping constructs.
    - [ ] Implementation for call constructs.
    - [ ] Introducing concept of stages in the VM tool.
  - [ ] *Heuristic* optimizations.
    - [x] Building up jump table first.
    - [ ] Rolling up increments / decrements.
    - [ ] Rolling up pointer movements.
    - [ ] Replacing reading from input with constant loading when tape provided.
    - [ ] Using `jnze` instead of plain `jmp` in optimization stage.

## Next phase - *Brainfork*

- [ ] Adding support for `Y` (*Brainfork*) - it make sense only in the compiler and VM.
  - [x] Do a detection based on the file extension.
  - [x] Code provided as a string should be always interpreted as a *Brainfuck*.
  - [ ] For VM - fork uses new *VM* process with cloned state.
  - [ ] For Compiler - fork should `spawn_link` a new process with logic and its own memory.

## Next phase - Documentation

- [ ] *Virtual Machine* overview.
- [ ] *Virtual Machine* internals.
  - [ ] Example session
- [ ] *Brainfork* internals.
  - [ ] Example session

## Improvements

- [ ] Improvements:
  - [ ] *REPL* and *Interpreter* - Detecting infinite loop based on *IC* observation.
  - [ ] *Compiler* - Safe pointers operations in `debug` mode.
  - [ ] *Compiler* - Pretty print *Core Erlang* to file.
  - [ ] *Compiler* - Optimization stage.
    - [ ] Removing unused code (based on used instructions).
    - [ ] Rolling up increments/decrements, pointer movements.
