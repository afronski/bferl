# TODO

## Next phase - Virtual Machine

- [ ] *Virtual Machine* facility.
  - [ ] Introducing concept of stages in the VM tool.
  - [ ] New stage with *heuristic* optimizations.
    - [ ] Rolling up increments / decrements.
    - [ ] Rolling up pointer movements.
    - [ ] Replacing reading from input with constant loading when tape provided.
    - [ ] Using `jnze` in `jit` optimization stage instead of plain `jmp` and `jze`.
      - It has to remove `jze`, insert `jnze` with the same index and rewrite
        jump table prepared at the beginning. It should optimize it after
        several rounds (e.g. at least 5 iterations).

## Next phase - *Brainfork*

- [ ] Adding support for `Y` (*Brainfork*) - it make sense only in the compiler and VM.
  - [ ] For VM - fork uses new *VM* process with cloned state.
  - [ ] For Compiler - fork should `spawn_link` a new process with logic and its own memory.

## Next phase - Documentation

- [ ] *Virtual Machine* overview.
- [ ] *Virtual Machine* internals.
  - [ ] Example session
- [ ] *Brainfork* internals.
  - [ ] Example session
