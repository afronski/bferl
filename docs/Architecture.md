# Architecture

## Overview

`bferl` is a standard *OTP* application, with very simple supervision hierarchy:

![Supervision Tree for `bferl`](/docs/images/supervision-tree.png)

Each service (*interpreter* without *REPL*, *compiler* and *virtual machine*) are gathered into one logical group called *tools*. Basically, each one is isolated from the others and crashing one of them do not affect others, so they can restart individually without any problems. Each facility in this logical group is an implementation of `gen_server`.

Tools and *I/O* subsystem have a different supervisors in order differentiate restart strategies.

## Common Facilities

Besides subsystems and *OTP* specific processes, we have also other modules with different responsibilities:

- `bferl_types` which contains predefined, internal type specifications.
- `bferl_tokenizer` which is responsible for splitting *Brainfuck* / *Brainfork* code into single tokens.
- `bferl_programming_language_logic` which is responsible for interpreting and representing language logic.

#### Internal Data Structure

Last module from the previous section, introduces a new data structure which is an *internal state* representation of *interpreter*. In our case it is defined as a record with name `interpreter`. It is defined in `include/interpreter_definitions.hrl`.

```erlang
-record(
 interpreter,

 { io = {undefined, undefined, undefined} :: bferl_types:io_callbacks(),
   stack = []                             :: list(pos_integer()),
   instructions_counter = 0               :: non_neg_integer(),
   instructions = undefined               :: undefined | bferl_types:instructions(),
   instructions_pointer = 1               :: pos_integer(),
   memory_pointer = 0                     :: non_neg_integer(),
   memory = ?EMPTY_MEMORY                 :: array:array(integer())
 }).
```

Inside this record we have:

- registered *I/O* subsystem references (inside `io` field),
- a *stack*, a reference for looping constructs (inside `stack` field),
- an *IC* (inside `instruction_counter` field),
- program content (inside `instructions` field),
- an *IP* (inside `instruction_pointer` field),
- actual position in memory (inside `memory_pointer` field),
- and memory representation (inside `memory` field).

Types attached to the record will help you deduce which values are valid for each field. By default `interpreter` memory is initialized with zeros and it has maximum size of 30000 memory cells.

Below you can see *pretty printed* record representation:

```erlang
1> application:ensure_all_started(bferl).
...
2> bferl_app:attach_console().
...
3> bferl_app:run_code(",>++++++[<-------->-],[<+>-]<.").
> 2
> 3
5
{interpreter,{#Fun<bferl_io.get_character_from_console.0>,
              #Fun<bferl_io.put_character_to_console.1>,
              #Fun<bferl_io.new_line_on_console.0>},
             [],397,
             [",",">","+","+","+","+","+","+","[","<","-","-","-","-",
              "-","-","-","-",">","-","]",",","[",
              [...]|...],
             31,0,
             {array,30000,0,0,
                    {{{{{53,0,0,0,0,0,0,0,0,0},10,10,10,10,10,10,10,10,10,10},
                       100,100,100,100,100,100,100,100,100,100},
                      1000,1000,1000,1000,1000,1000,1000,1000,1000,1000},
                     10000,10000,10000,10000,10000,10000,10000,10000,10000,
                     10000}}}
```

Trained eye will spot an *Erlang* `array` representation in the last field.

## Subsystems

As it is stated above, whole application is split into multiple subsystems. We will describe them one by one in this section.

### Interpreter and REPL

First tool delivered with the `bferl_app` is the *interpreter*, together with a *REPL* functionality.

You can evaluate code delivered as a file or string, with these commands:

- `bferl_app:run_file("./hello_world.bf").` or `bferl_app:run_code("[-]").`.
  - Interpreter does not support *Brainfork* code.

Also, you can start a REPL by invoking the following command:

- `bferl_app:repl().`
  - After that your prompt will change and you can start playing with *REPL* abilities.
  - *REPL* has built-in help functionality, which is available by invoking `?help` in it.
  - It does not support *Brainfork* code.

Details about implementation and usage documentation is hosted [here](/docs/REPL.md).

### Compiler

One of the tools delivered with the application is a *compiler*. It compiles *Brainfuck* / *Brainfork* code to the *Core Erlang* representation. If you are interested, you can find more details [here](/docs/Compiler.md).

How to compile a program? You can do it by invoking these commands in the shell:

- `bferl_app:compile_file("./hello_world.bf").` or `bferl_app:compile_code("[-]").`.
  - If you want to compile your code in `debug` mode, you can add aforementioned atom as a second argument of both functions.
  - If you would like to compile a *Brainfork* code, you can do it only by delivering a file with an extension `.bfo`.

After invoking those commands (with a valid program) it will produce a *BEAM* module which will be automatically loaded after successful compilation into the current shell session. Prepared module has two exported functions which are the *starting points*:

- `MODULE_NAME:start/0` - which starts program with console attached as an I/O subsystem. As a result it will return amount of instructions executed during the program run.
- `MODULE_NAME:start/1` - which starts program with I/O subsystem represented as a tape (you are providing an input tape as an argument). The output tape, together with counter described above, will be returned as a result of the program execution.

Now you can invoke your program with one of the starting points.

### Virtual Machine

This module is a very simple implementation of a *VM* which does very crude *JIT* optimizations and has different internal representation than the *interpreter* or *compiled programs*. If you are interested in details, you can find them [here](/docs/VirtualMachine.md).

You can run code on it, with these commands:

- `bferl_app:run_file_on_vm("./hello_world.bf").` or `bferl_app:run_code_on_vm("[-]").`
  - Both methods have ability to run programs in *debug* mode.
    - Add `debug` atom to run it with special annotations, e.g.:
      - `bferl_app:run_code_on_vm("[-]", debug).`
      - `bferl_app:run_file_on_vm("./hello_world.bf", debug).`
  - If you would like to run a *Brainfork* code, you can do it only by delivering a file with an extension `.bfo`.

### I/O

*Interpreter* and *virtual machine* are relying additionally on the *I/O* subsystem. In described application it is represented by `bferl_io`.

It is a `gen_event` behavior with ability to attach different mechanisms - a standard console or a *tape*-like mechanism. *Tape* in that context has a predefined input (delivered as a list of characters) and output (result of printing statements inside the executed program).
