# TODO

## Phase 1

- [x] Bootstrapped basic structure and `rebar3` configuration.
- [x] Tokenizer and Parser.
  - [x] Removing comments.
  - [x] Handling set of keywords (e.g. enhancement for *Brainfork*)
- [x] Basic structure for interpreter.
  - [x] Records vs. Maps?
  - [ ] Implement all *Brainfuck* opcodes.
- [x] Setting up `common_test` framework.
  - [x] Test-Driven Development.
  - [ ] Basic intergration tests.
- [ ] Setting up *QuickCheck-like* tool e.g. *PropER*.
  - [ ] Basic tests for the parser / tokenizer.
  
## Phase 2 

- [ ] Updating documentation.
- [ ] Parser as a `gen_server`.
- [ ] Interpreter *VM* as a `gen_server`.

## Phase 3

- [ ] Adding support for `Y` (*Brainfork*).
  - [ ] Fork uses new process with cloned *VM* state. 
- [ ] Compiling source to the *BEAM* representation.
  - [ ] Running it as a standard module with fixed structure and *exports*.

## Phase 4

- [ ] *Heuristic* optimizations (e.g. rolling up incrementations / decrementations).
- [ ] *Hot Code* detection.
  - [ ] Basic optimizations based on pattern and usage.
