# TODO
  
## Next phase - *OTP*

- [ ] Add property based tests for:
  - [ ] Tokenizer
  - [ ] I/O operations
- [ ] Prepare it as an *OTP* system.
  - [ ] Interpreter as a `gen_server`.
  - [ ] *VM* as a `gen_server`.
  - [ ] Proper *OTP* application.
- [ ] Setting up `dialyzer` and preparing *type specifications*. 
- [ ] Updating documentation (and *screenshots*).

## Next phase - *Brainfork* and Compiler

- [ ] Adding support for `Y` (*Brainfork*).
  - [ ] Fork uses new process with cloned *VM* state. 
- [ ] Compiling source to the *BEAM* representation.
  - [ ] Compile it to the single function inside module with *fixed* structure.

## Next phase - Virtual Machine

- [ ] *Heuristic* optimizations (e.g. rolling up incrementations / decrementations).
- [ ] *Hot Code* detection.
  - [ ] Basic optimizations based on pattern and usage.
- [ ] Visualizations - *API*, *introspection* (like http://privateeye.io).
  - [ ] Graphic visualization in *d3.js*.
