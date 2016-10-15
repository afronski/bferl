# Virtual Machine

## Internals

Another part of that project is *Virtual Machine*. It translates *Brainfuck* opcodes to intermediate language, similar to assembly. Underneath it is *register*-based *virtual machine*, supervision tree (`supervisor` and `gen_server`) for taking care of individual execution threads. As in previous cases - it is a **toy**. At the moment it does not do any additional optimizations (like *rolling up* increments / decrements etc.).

### Translation and Execution

There are two modules responsible for translating *Brainfuck* / *Brainfork* opcodes to the intermediate language and executing it (correspondingly they are called `bferl_vm_ir_translator` and `bferl_vm_ir_executor`). Intermediate language (called also `IR`) is similar to the assembly, because we have implemented *register*-based virtual machine. It contains following mnemonics:

- `{add, ir0 | r0, INTEGER}` - add literal value to a following register.
- `{sub, ir0 | r0, INTEGER}` - subtract literal value from a following register.
- `{jze, POSITIVE_INTEGER}` - conditional jump to following instruction encoded as literal, if `zf` is set.
- `{jmp, POSITIVE_INTEGER}` - jump unconditionally to following instruction encoded as literal.
- `{jnze, POSITIVE_INTEGER}` - conditional jump to following instruction encoded as literal, if `zf` is not set.
- `{const, ir0, NOT_NEGATIVE_INTEGER}` - insert literal value to `ir0` register.
- `{const, r0, INTEGER}` - insert lieteral value to `r0` register.
- `{load, ir0, r0}` - load number from memory cell indexed with register `ir0` to register `r0`.
- `{store, r0, ir0}` - store value from register `r0` to memory cell indexed with register `ir0`.
- `{call, in | out | fork}` - call one of three built-in procedures (either *get character*, *put character* or *fork*).

From those operations, we can deduce that internally we have available following modifiable registers:

- `ir0` - index register for indexing memory cells.
- `r0` - accumulator, register used for all value-related operations.

Beside those we have also *flags* and not modifiable registers:

- `zf` - zero flag, it is set when `0` value is inside `r0` register.
- `ic` - usual *instruction counter* (number of executed instructions), this is increased after each opcode.
- `ip` - usual *instruction pointer* (index of currently executed instruction), this is set by jumps.

At last and not least, we have usual structures there:

- We have three pointers for *I/O* subsystem (*get character*, *put character* and *put new line*).
- We have *30000* words of memory (where *word* is a usual number, with no range defined).
- We have list with our current program (it contains `IR` opcodes).

A new structure is a jump table (`jmp_table`), built during in the translation stage. It is necessary for using various jumps defined above (we could use 2 types of jumps, but third is used as an optimization).

All of mentioned structures are defined inside a record called `register_based_virtual_machine`.

### Execution threads

`bferl_vm_threads_sup`

`bferl_vm_thread`

### API

`bferl_tools_virtual_machine`

### Helpers

Inside `bferl_app` we have couple of helpers available:

- *TODO*

## Example Session
