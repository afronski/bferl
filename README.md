# bferl

## Description

**This is a pet project**.

*Brainfuck* and *Brainfork* are well-known [*esoteric programming languages*](https://en.wikipedia.org/wiki/Esoteric_programming_language). While both are fully *turing complete*, they are not intened to practical use. :wink:

But each of them is an ideal candidate to learn how to write by yourself an *interpreter*, *REPL*, *compiler* or *virtual machine* and in overall - *play by doing it*. It is also an interesting place to learn and verify various concepts like e.g. *property based testing* (with [manopapad/proper](https://github.com/manopapad/proper)) or working with an amazing tracing support in *Erlang* (and equally terrific [ferd/recon](https://github.com/ferd/recon) library with facilities for *analyzing*/*detecting* common problems in the systems built on top of *Erlang VM*).

## How to build and run it?

1. Grab `rebar3` binaries from [here](https://github.com/rebar/rebar3).
2. `rebar3 compile`
3. `rebar3 shell`
4. Invoke following commands in the *Erlang* shell:
  - `application:ensure_all_started(bferl).`
  - `bferl_app:attach_console().`
  - Now you can play with all kind of *Brainfuck* / *Brainfork* programs!
    - See the documentation for more examples.
5. During development, you will probably need:
  - To run tests, by invoking command: `rebar3 ct`
  - To perform a *Dialyzer* analysis, by invoking command: `rebar3 dialyzer`

### Support

Supported and tested on:

- *Erlang 17.5*
- *Erlang 18.0*, *18.1*, *18.2*, *18.2.1* and *18.3*
- *Erlang 19.0*

## Quick Example

If you want to start playing without any hassle, here you have an example program which adds two digits (represented as `ASCII` characters) and display result of that operation:

```erlang
1> bferl_app:run_code(",>++++++[<-------->-],[<+>-]<.").
> 2
> 3
5
{interpreter,...}
```

As a result of the invocation we returned the internal state of the *interpreter* after the program execution.

**Note**: You have probably noticed that it will not produce proper results if the result will be greater than '9'. :wink:

## Documentation

Interested in more? Dive into one of those pages:

- [Architecture](/docs/Architecture.md) - General overview and *architecture* documentation.
- [REPL](/docs/REPL.md) - Description of *REPL* and *Interpreter* subsystems.
- [Compiler](/docs/Compiler.md) - Description of the *Compiler* subsystem (compilation to *Core Erlang* representation).
- [Virtual Machine](/docs/VirtualMachine.md) - Description of the *Virtual Machine* subsystem.

## Improvements

- Further improvements:
  - [ ] *REPL* and *Interpreter* - Detecting infinite loop based on *IC* observation (increasing trend).
  - [ ] *Compiler* - Safe pointer operations in `debug` mode.
  - [ ] *Compiler* - Dump pretty printed *Core Erlang* to a file.
  - [ ] *Compiler* - Additional stage for various optimizations.
    - [ ] Removing unused code (scan generated code and remove implementation of unused instructions).
    - [ ] Rolling up increments, decrements and pointer movements.
  - [ ] *Virtual Machine* - Additional stage for various optimizations.
    - [ ] Rolling up increments / decrements.
    - [ ] Rolling up pointer movements.
    - [ ] Replacing reading from input with constant loading when tape provided.
    - [ ] Using `jnze` in `jit` optimization stage instead of plain `jmp` and `jze`.
      - *How*? We have to remove `jze`, insert `jnze` with the same index and
        rewrite jump table prepared at the beginning. It should be optimized
        after several rounds (e.g. at least 5 iterations).
