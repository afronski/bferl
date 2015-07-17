-module(brainfuck_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/interpreter_definitions.hrl").

-export([ all/0 ]).
-export([ increment_value_in_the_cell/1, decrement_value_in_the_cell/1,
          moving_pointer_left/1, moving_pointer_right/1,
          edge_cases_for_moving_pointer/1 ]).

all() ->
    [ increment_value_in_the_cell, decrement_value_in_the_cell,
      moving_pointer_left, moving_pointer_right,
      edge_cases_for_moving_pointer ].

increment_value_in_the_cell(_Context) ->
    State = bferl_interpreter:init(["+"]),
    Output = bferl_interpreter:run(State),

    ?assertEqual(1, bferl_interpreter:get_memory_cell(0, Output)).

decrement_value_in_the_cell(_Context) ->
    State = bferl_interpreter:init(["-"]),
    Output = bferl_interpreter:run(State),

    ?assertEqual(-1, bferl_interpreter:get_memory_cell(0, Output)).

moving_pointer_left(_Context) ->
    State = bferl_interpreter:init(["<"]),
    NextState = State#interpreter_state{memory_pointer = 1},
    Output = bferl_interpreter:run(NextState),

    ?assertEqual(0, Output#interpreter_state.memory_pointer).

moving_pointer_right(_Context) ->
    State = bferl_interpreter:init([">"]),
    Output = bferl_interpreter:run(State),

    ?assertEqual(1, Output#interpreter_state.memory_pointer).

edge_cases_for_moving_pointer(_Context) ->
    InvalidLeft = bferl_interpreter:init(["<"]),
    LeftOutput = bferl_interpreter:run(InvalidLeft),

    ?assertEqual(0, LeftOutput#interpreter_state.memory_pointer),

    InvalidRight = bferl_interpreter:init([">"]),
    NextInvalidRight = InvalidRight#interpreter_state{memory_pointer = ?MEMORY_SIZE},
    RightOutput = bferl_interpreter:run(NextInvalidRight),

    ?assertEqual(?MEMORY_SIZE, RightOutput#interpreter_state.memory_pointer).
