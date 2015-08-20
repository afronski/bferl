# TODO
  
## Next phase - *OTP*

- [x] Prepare it as an *OTP* system.
  - [x] Proper *OTP* application.
  - [x] Interpreter as a `gen_server`.
  - [x] *VM* as a `gen_server`.
- [x] Updating documentation (and *screenshots*).
- [ ] Implementing interpreter logic.
  - [ ] REPL.
  - [ ] Interactive visualization.
  - [ ] Attaching tape.
- [ ] Setting up `dialyzer` and preparing *type specifications*.
- [ ] Add property based tests for:
  - [ ] Tokenizer
  - [ ] I/O operations

## Next phase - *Brainfork* and Compiler

- [ ] Adding support for `Y` (*Brainfork*).
  - [ ] Fork uses new process with cloned *VM* state. 
- [ ] Compiling source to the *BEAM* representation.
  - [ ] Compile it to the single function inside module with *fixed* structure.

## Next phase - Virtual Machine

- [ ] *Heuristic* optimizations (e.g. rolling up incrementations / decrementations).
- [ ] *Hot Code* detection.
  - [ ] Basic optimizations based on pattern and usage.
- [ ] Visualizations - *API*, *introspection*, *profiler* (like http://privateeye.io).
  - [ ] Graphic visualization in *d3.js*.
