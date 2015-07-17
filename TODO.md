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
  - [ ] Basic intergration tests.
- [ ] Setting up *QuickCheck-like* tool e.g. *PropER*.
  - [ ] Basic tests for the parser / tokenizer.
  
## Phase 2 - *OTP*

- [ ] Updating documentation.
- [ ] Interpreter as a `gen_server`.
- [ ] *VM* as a `gen_server`.
- [ ] Prepare it as an *OTP* application.

## Phase 3 - *Brainfork* and Compiler

- [ ] Adding support for `Y` (*Brainfork*).
  - [ ] Fork uses new process with cloned *VM* state. 
- [ ] Compiling source to the *BEAM* representation.
  - [ ] Compile it to the single function inside module with *fixed* structure.

## Phase 4 - Virtual Machine

- [ ] *Heuristic* optimizations (e.g. rolling up incrementations / decrementations).
- [ ] *Hot Code* detection.
  - [ ] Basic optimizations based on pattern and usage.
