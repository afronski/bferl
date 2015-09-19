# bferl

## Description

**This is a pet project**.

*Brainfuck* and *Brainfork* are well-known [*esoteric programming languages*](https://en.wikipedia.org/wiki/Esoteric_programming_language). While both are fully *turing complete*, they are not intened to practical use. :wink:

But each of them is an ideal candidate to learn how to write by yourself an *interpreter*, `REPL`, *compiler* or *virtual machine* and in overall - *play by doing it*. It is also an interesting place to learn and verify various concepts like e.g. *property based testing* (with [manopapad/proper](https://github.com/manopapad/proper)) or working with an amazing tracing support in *Erlang* (and equally terrific [ferd/recon](https://github.com/ferd/recon) library for detecting common problems).

## How to build and run it?

1. Grab `rebar3` binaries from [here](https://github.com/rebar/rebar3).
2. `rebar3 compile`
3. `rebar3 shell`
4. Invoke following commands in the *Erlang* shell:
  - `application:ensure_all_started(bferl).`
  - `bferl_app:attach_console().`
  - Now you can play with *Brainfuck* programs by built-in:
    - Interpreter:
      - Loading code from file - `bferl_app:run_file("hello_world.bf").`
      - Evaluating code - `bferl_app:run_code(",+.").`
      - Working with `REPL` - `bferl_app:repl().`
    - Compiler to *BEAM*:
      - Compiling code from file - `bferl_app:compile_file("hello_world.bf").`
      - Compile code delivered as a string - `bferl_app:compile_code(",+.")`
      - Both commands will produce new module with `start` command.
        - You can start it by typing `MODULE_NAME_AFTER_COMPILATION:start().`
        - If you would like to provide a *tape* with input characters you can do it by:
          - `MODULE_NAME_AFTER_COMPILATION:start("This is input tape content.").`
    - Virtual Machine:
      - Loading code from file - `bferl_app:run_file_on_vm("hello_world.bf").`
      - Evaluating code - `bferl_app:run_code_on_vm(",+.").`
      - Both methods have ability to run program in debug mode.
        - Add `debug_mode` flag to run it with debugging annotations:
        - `bferl_app:run_code_on_vm(",+.", [ debug_mode ]).`
   - *Brainfork* variation can be delivered to the *compiler* and *virtual
     machine* facilities only via functions that read files.
     - That particular file should have extension `.bfo`, otherwise it will be
       interpreted as a *Brainfuck*.
5. If you would like to run tests, invoke command: `rebar3 ct`.

## Architecture

`bferl` is a standard *OTP* application, with very simple supervision hierarchy:

![Supervision Tree for `bferl`](/docs/supervision-tree.png)

Each service (*interpreter* with `REPL`, *compiler* and *virtual machine*) are gathered into one group called *tools*. Basically, each one is isolated from the others and crashing one of them do not affect others, so they can restart individually without any problems.

*Interpreter* and *virtual machine* are relying additionally on the *I/O* subsystem - `bferl_io`. It is a `gen_event` behavior with ability to attach a standard console or a *tape*-like mechanism. Such tape has a predefined input (delivered as a list of characters) and output (result of printing statements inside a program).

*Compiler* produces a *BEAM* file which is automatically loaded after successfull compilation into the current shell session for you. Such module have two exported functions, which are the *starting points*:

- `MODULE_NAME:start/0` - which starts program with console attached as an I/O subsystem.
- `MODULE_NAME:start/1` - which starts program with I/O subsystem represented as a tape (you are providing input tape as an argument, and the output tape will be returned as a result of the program execution).

## Example

Here is the execution of example program which adds two digits (represented as an `ASCII`) together and display result of that:

```erlang
1> bferl_app:run_code(",>++++++[<-------->-],[<+>-]<.").
> 2
> 3
5
{interpreter,{#Fun<bferl_io.get_character_from_console.0>,
              #Fun<bferl_io.put_character.1>,#Fun<bferl_io.new_line.0>},
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

As a result we returned the internal state of the *interpreter* after the program execution.

**Note**: You have probably noticed that it will not produce proper results if the result is greater than '9'. :wink:
