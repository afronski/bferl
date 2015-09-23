# Architecture

## Overview

`bferl` is a standard *OTP* application, with very simple supervision hierarchy:

![Supervision Tree for `bferl`](/docs/supervision-tree.png)

Each service (*interpreter* with *REPL*, *compiler* and *virtual machine*) are gathered into one group called *tools*. Basically, each one is isolated from the others and crashing one of them do not affect others, so they can restart individually without any problems.

## Internal Data Structure

## Subsystems

### Interpreter and REPL

### Compiler

*Compiler* produces a *BEAM* file which is automatically loaded after successfull compilation into the current shell session for you. Such module have two exported functions, which are the *starting points*:

- `MODULE_NAME:start/0` - which starts program with console attached as an I/O subsystem.
- `MODULE_NAME:start/1` - which starts program with I/O subsystem represented as a tape (you are providing input tape as an argument, and the output tape will be returned as a result of the program execution).

### Virtual Machine

### I/O

*Interpreter* and *virtual machine* are relying additionally on the *I/O* subsystem - `bferl_io`. It is a `gen_event` behavior with ability to attach a standard console or a *tape*-like mechanism. Such tape has a predefined input (delivered as a list of characters) and output (result of printing statements inside a program).
