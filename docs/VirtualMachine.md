# Virtual Machine

## Internals

Another part of that project is *Virtual Machine*. It translates *Brainfuck* opcodes to intermediate language, similar to assembly. Underneath it is *register*-based *virtual machine*, supervision tree (`supervisor` and `gen_server`) for taking care of individual execution threads. As in previous cases - it is a **toy**. At the moment it does not do any additional optimizations (like *rolling up* increments / decrements etc.).

### Translation and Execution

There are two modules responsible for translating *Brainfuck* / *Brainfork* opcodes to the intermediate language and executing it (correspondingly they are called `bferl_vm_ir_translator` and `bferl_vm_ir_executor`). Intermediate language (called also `IR`) is similar to the assembly, because we have implemented *register*-based virtual machine. It contains following mnemonics:

- `{add, ir0 | r0, INTEGER}` - Add literal value to a following register.
- `{sub, ir0 | r0, INTEGER}` - Subtract literal value from a following register.
- `{jze, POSITIVE_INTEGER}` - Conditional jump to following instruction encoded as literal, if `zf` is set.
- `{jmp, POSITIVE_INTEGER}` - Jump unconditionally to following instruction encoded as literal.
- `{jnze, POSITIVE_INTEGER}` - Conditional jump to following instruction encoded as literal, if `zf` is not set.
- `{const, ir0, NOT_NEGATIVE_INTEGER}` - Insert literal value to `ir0` register.
- `{const, r0, INTEGER}` - Insert literal value to `r0` register.
- `{load, ir0, r0}` - Load number from memory cell indexed with register `ir0` to register `r0`.
- `{store, r0, ir0}` - Store value from register `r0` to memory cell indexed with register `ir0`.
- `{call, in | out | fork}` - Call one of three built-in procedures (either *get character*, *put character* or *fork*).

From those operations, we can deduce that internally we have available following modifiable registers:

- `ir0` - Index register for indexing memory cells.
- `r0` - Accumulator, register used for all value-related operations.

Beside those we have also *flags* and not modifiable registers:

- `zf` - Zero flag, it is set when `0` value is inside `r0` register.
- `ic` - Usual *instruction counter* (number of executed instructions), this is increased after each opcode.
- `ip` - Usual *instruction pointer* (index of currently executed instruction), this is set by jumps.

At last and not least, we have usual structures there:

- We have three pointers for *I/O* subsystem (*get character*, *put character* and *put new line*).
- We have *30000* words of memory (where *word* is a usual number, with no range defined).
- We have list with our current program (it contains `IR` opcodes).

A new structure is a jump table (`jmp_table`), built during in the translation stage. It is necessary for using various jumps defined above (we could use 2 types of jumps, but third is used as an optimization).

All of mentioned structures are defined inside a record called `register_based_virtual_machine`.

### Execution threads

Execution single thread is a piece of cake, but if you want to execute multiple programs and you want to have a facility in place for *Brainfork* you need to tackle it differently. We have used *OTP* built-in behaviors: `supervisor` (inside `bferl_vm_threads_sup`) and `gen_server` (inside `bferl_vm_thread`) to wrap translation, execution into separate and isolate threads.

It is a standard usage of `simple_one_for_one` strategy, without any additional fireworks. Each thread can be in one of couple states:

- After `prepare` we have thread with translated content and initial virtual machine context loaded (with additional things e.g. *tape* or other settings).
- Inside `running` it executes previously prepared program.
- In `finished` it stopped, gathered last state (called in this phase a *result*) and reported it back to parent which stored it inside process memory.

Basically everything is hidden from end-user and wrapped in an API described below.

### Subsystem *API*

Orchestration and parent (aka *starter*) for all threads is inside `bferl_tools_virtual_machine` module. We have couple of *API* calls defined for cover protocols, spawning children, properly feeding them with initial data and recording final results (it also covers `PID` management). We have following calls available for the *API* user:

- `start_vm_thread/3` - It prepares (but not start yet) new thread based on all three arguments passed there. It will return a tuple `{ok, PID_FOR_NEW_THREAD}`:
  - `Program` contains textual representation of *Brainfuck* / *Brainfork* code.
  - `Type` contains type of the program (either `"Brainfuck"` or `"Brainfork"`).
  - `Flags` contains flags for *virtual machine*. It can be either:
    - `debug` - It will *pretty-print* initial state, *virtual machine* settings and translated program representation.
    - `interactive` - It will allow to step through the program (with *pretty-printing* state of *virtual machine* for each), executing instructions *step by step* - not all at once.
    - `optimize` - It will enable first stage of optimizations (`IR` opcodes rewrite).
    - `jit` - It will enable more sophisticated optimizations (enabled after multiple runs, like in *JIT* compilers).
- `run_program/1` - It will start program based on the passed `PID`.
- `get_result_for_thread/1` - It will gather final results for the passed `PID` or it will return that there is no result available yet.
- `step/1` - If *virtual machine* is `interactive` mode, this will allow you to step through code, instruction by instruction, based on the passed `PID`.

All functions are taking care of non-existing process identifiers. Just a remark - internally module is a `gen_server` implementation, it lives on the same level as `bferl_vm_threads_sup`.

### Helpers

Inside `bferl_app` we have couple of helpers available:

- `run_code_on_vm(Code)` - It reads code directly from passed string literal, and executes it as *Brainfuck*, with all possible optimizations.
- `run_code_on_vm(Code, debug)` - It reads code directly from passed string literal, and executes it as *Brainfuck*, *step-by-step* with printing *debug* information upfront.
- `run_code_on_vm(Code, Tape)` - It reads code directly from passed string literal, and executes it as *Brainfuck*, with all possible optimizations. Additionally it attaches the passed string literal as *input tape*, *output tape* will be printed after finishing the whole program.
- `run_code_on_vm(Code, Tape, debug)` - It reads code directly from passed string literal, and executes it as *Brainfuck*, *step-by-step* with printing *debug* information upfront. Additionally it attaches the passed string literal as *input tape*, *output tape* will be printed after finishing the whole program.
- `run_file_on_vm(Filename)` - It reads file, based on passed filename and extension and executes it, will all possible optimizations.
- `run_file_on_vm(Filename, debug)` - It reads file, based on passed filename and extension and executes it *step-by-step* with printing *debug* information upfront.
- `run_file_on_vm(Filename, Tape)` - It reads file, based on passed filename and extension and executes it, with all possible optimizations. Additionally it attaches the passed string literal as *input tape*, *output tape* will be printed after finishing the whole program.
- `run_file_on_vm(Filename, Tape, debug)` - It reads file, based on passed filename and extension and executes it *step-by-step* with printing *debug* information upfront. Additionally it attaches the passed string literal as *input tape*, *output tape* will be printed after finishing the whole program.

## Example Session

*TODO*
