# *REPL* and *Interpreter*

This project contains not only a simple interpreter of *Brainfuck* programming
language, but also an interactive *REPL* with many debugging facilities. It does
not have support for *Brainfork* deliberately.

This document contains details about the internal implementation and available
API.

## Internals

### `bferl_repl`

It is a wrapper for `bferl_tools_interpreter` and `bferl_tokenizer` with
additional functionality related with invoking commands, *CLI* interface,
prompts and debugging facilities (like pretty printing, interactivity etc.).

### `bferl_tools_interpreter`

It is an implementation of the interpreter process, exposed as a `gen_server`
behavior implementation. It evaluates provided code in a separate process in
order to avoid unresponsiveness of main `gen_server`.

It uses for that a pair - *process* with *monitor*. If no response will be sent
in *5 seconds*, it assumes that provided code contains an infinite loop.

Besides that this module use internally `bferl_programming_language_logic`
module, which encodes whole logic. Details of it are explain [here](/docs/Architecture.md).

## API

### Helpers

Application level helpers and shortcuts for ease of use.

- `bferl_app:run_file/1` - It runs file on top a fresh instance of interpreter
  and returns state after execution.
  - Provided argument is a path to a file with *Brainfuck* code.
  - Since there is no support for *Brainfork* here, extension of that file does
    not matter.
- `bferl_app:run_code/1` - It runs a code sample delivered as a first argument
  on top a fresh instance of interpreter.
- `bferl_app:repl/0` - It starts interactive *REPL* environment.
  - Available commands are described in the next section.

**Keep in mind that if your program requires *I/O*, all functions above require
access to the console, so it should be invoked in the *Erlang* shell session
after connecting console to the I/O subsystem (via `bferl_app:attach_console().`)
or it should use tape subsystem as follows**:

```
1> application:ensure_all_started(bferl).
...
2> bferl_io:tape("ABCD").
ok
3> bferl_app:run_code("++++[>,+.<-]").
...
4> bferl_io:get_output_tape().
"BCDE"
```

### *REPL*

- `bferl_repl:start_loop/0` - Internal function used by an application level
  facility, which start the *Read, Evaluate, Print, Loop*.
  - **Keep in mind that it requires access to the console, so it should be invoked in the *Erlang* shell session**.

Available *REPL* commands:

- `?h`, `?help` - Prints help messages.
- `?e`, `?exit` - Exits the *REPL*.
- `?i`, `?interactive` - Enables the interactive mode.
  - It means that after typing a bunch of *Brainfuck* commands and hitting
   *Enter*, *REPL* will evaluate only one instruction and will wait for next
    commands or next *Enter* key.
- `?a`, `?autoprint` - Pretty-prints automatically *REPL* state, after
  evaluating commands.
  - Combined with the previous (`?i`, `?interactive`) command, it shows a *REPL*
    state after single command execution.
- `?c`, `?clear` - Clears *REPL* state.
- `?r`, `?reset` - Resets only *REPL* pointers, but it will not touch the
  program or tapes.
- `?s`, `?state` - Immediately prints *REPL* state at the moment of the
  invocation of the command.
- `?t[T]`, `?tape[T]` - Attaches tape to the *REPL*, which is provided as `T`
  argument of a command (a *string*).

### `bferl_tools_interpreter

- `bferl_tools_interpreter:start_link/0`
- `bferl_tools_interpreter:get_state/0`
- `bferl_tools_interpreter:restore/1`
- `bferl_tools_interpreter:clear/0`
- `bferl_tools_interpreter:reset/0`
- `bferl_tools_interpreter:tape_attached/0`
- `bferl_tools_interpreter:evaluate_code/1`
- `bferl_tools_interpreter:debug_mode/0`
- `bferl_tools_interpreter:validate/1`

## Example Session

*TODO*
