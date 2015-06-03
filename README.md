# bferl

## Description

*TODO*

## How to build and run it?

1. Grab `rebar3` binaries from [here](https://github.com/rebar/rebar3).
2. `rebar3 compile`
3. `rebar3 shell`
4. Invoke following commands in the *Erlang* shell:
  - `application:start(sasl).`
  - `application:start(bferl).`
  - `bferl:run("./examples/hello_world.bf")`
5. If you would like to run tests, invoke command: `rebar3 ct`.

## Architecture

*TODO*
