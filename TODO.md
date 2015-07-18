# TODO

## Phase 1 - Foundation and Interpreter

- [x] Bootstrapped basic structure and `rebar3` configuration.
- [x] Tokenizer and Parser.
  - [x] Removing comments.
  - [x] Handling set of keywords (e.g. enhancement for *Brainfork*)
- [x] Basic structure for interpreter.
  - [x] Records vs. Maps?
  - [x] Implement all *Brainfuck* opcodes.
- [x] Setting up `common_test` framework.
  - [x] Test-Driven Development.
  - [x] Basic intergration tests.
- [x] Setting up *QuickCheck-like* tool e.g. *PropER*.
  - [x] Basic tests for the interpreter.
  
## Phase 2 - *OTP*

- [ ] Add property based tests for:
  - [ ] Tokenizer.
  - [ ] I/O operations.
- [ ] Prepare it as an *OTP* system.
  - [ ] Interpreter as a `gen_server`.
  - [ ] *VM* as a `gen_server`.
  - [ ] Proper *OTP* application.
- [ ] Setting up `dialyzer` and preparing *type specifications*. 
- [ ] Updating documentation (and *screenshots*).

## Phase 3 - *Brainfork* and Compiler

- [ ] Adding support for `Y` (*Brainfork*).
  - [ ] Fork uses new process with cloned *VM* state. 
- [ ] Compiling source to the *BEAM* representation.
  - [ ] Compile it to the single function inside module with *fixed* structure.

## Phase 4 - Virtual Machine

- [ ] *Heuristic* optimizations (e.g. rolling up incrementations / decrementations).
- [ ] *Hot Code* detection.
  - [ ] Basic optimizations based on pattern and usage.
