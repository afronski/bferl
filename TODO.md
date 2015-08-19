# TODO
  
## Next phase - *OTP*

- [ ] Prepare it as an *OTP* system.
  - [ ] Proper *OTP* application.
  - [ ] Interpreter as a `gen_server`.
  - [ ] *VM* as a `gen_server`.
- [ ] Updating documentation (and *screenshots*).
- [ ] Add property based tests for:
  - [ ] Tokenizer
  - [ ] I/O operations 
- [ ] Setting up `dialyzer` and preparing *type specifications*.

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
