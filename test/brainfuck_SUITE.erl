-module(brainfuck_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/interpreter_definitions.hrl").

-export([ all/0 ]).
-export([ increment_value_in_the_cell/1, decrement_value_in_the_cell/1,
          moving_pointer_left/1, moving_pointer_right/1,
          edge_cases_for_moving_pointer/1,
          testing_loop/1, testing_nested_loops/1 ]).

all() ->
    [ increment_value_in_the_cell, decrement_value_in_the_cell,
      moving_pointer_left, moving_pointer_right,
      edge_cases_for_moving_pointer,
      testing_loop, testing_nested_loops ].


increment_value_in_the_cell(_Context) ->
    State = bferl_programming_language_logic:new(["+"]),
    Output = bferl_programming_language_logic:run(State),

    ?assertEqual(1, bferl_programming_language_logic:get_memory_cell(0, Output)).

decrement_value_in_the_cell(_Context) ->
    State = bferl_programming_language_logic:new(["-"]),
    Output = bferl_programming_language_logic:run(State),

    ?assertEqual(-1, bferl_programming_language_logic:get_memory_cell(0, Output)).

moving_pointer_left(_Context) ->
    State = bferl_programming_language_logic:new(["<"]),
    NextState = State#interpreter{memory_pointer = 1},
    Output = bferl_programming_language_logic:run(NextState),

    ?assertEqual(0, Output#interpreter.memory_pointer).

moving_pointer_right(_Context) ->
    State = bferl_programming_language_logic:new([">"]),
    Output = bferl_programming_language_logic:run(State),

    ?assertEqual(1, Output#interpreter.memory_pointer).

edge_cases_for_moving_pointer(_Context) ->
    InvalidLeft = bferl_programming_language_logic:new(["<"]),
    LeftOutput = bferl_programming_language_logic:run(InvalidLeft),

    ?assertEqual(0, LeftOutput#interpreter.memory_pointer),

    InvalidRight = bferl_programming_language_logic:new([">"]),
    NextInvalidRight = InvalidRight#interpreter{memory_pointer = ?MEMORY_SIZE},
    RightOutput = bferl_programming_language_logic:run(NextInvalidRight),

    ?assertEqual(?MEMORY_SIZE, RightOutput#interpreter.memory_pointer).

testing_loop(_Context) ->
    State = bferl_programming_language_logic:new(["+", "+", "+", "[", "-", "]", "-"]),
    Output = bferl_programming_language_logic:run(State),

    % Calculation: 3x '+', 3x '[-]', 1x '-' and +1 for `end_of_program`.
    ?assertEqual(3 + 3 * 3 + 1 + 1, Output#interpreter.instructions_counter),
    ?assertEqual(length(Output#interpreter.instructions) + 1, Output#interpreter.instructions_pointer),

    ?assertEqual(-1, bferl_programming_language_logic:get_memory_cell(0, Output)).

testing_nested_loops(_Context) ->
    State = bferl_programming_language_logic:new(["[", "[", "-", "]", ">", "]"]),

    State0 = State#interpreter{memory = array:set(0, 5, State#interpreter.memory)},
    State1 = State0#interpreter{memory = array:set(1, 4, State0#interpreter.memory)},
    State2 = State1#interpreter{memory = array:set(2, 3, State1#interpreter.memory)},
    State3 = State2#interpreter{memory = array:set(3, 2, State2#interpreter.memory)},
    StateWithModifiedMemory = State3#interpreter{memory = array:set(4, 1, State3#interpreter.memory)},

    ?assertEqual(5, bferl_programming_language_logic:get_memory_cell(0, StateWithModifiedMemory)),
    ?assertEqual(4, bferl_programming_language_logic:get_memory_cell(1, StateWithModifiedMemory)),
    ?assertEqual(3, bferl_programming_language_logic:get_memory_cell(2, StateWithModifiedMemory)),
    ?assertEqual(2, bferl_programming_language_logic:get_memory_cell(3, StateWithModifiedMemory)),
    ?assertEqual(1, bferl_programming_language_logic:get_memory_cell(4, StateWithModifiedMemory)),

    Output = bferl_programming_language_logic:run(StateWithModifiedMemory),

    ?assertEqual(0, bferl_programming_language_logic:get_memory_cell(0, Output)),
    ?assertEqual(0, bferl_programming_language_logic:get_memory_cell(1, Output)),
    ?assertEqual(0, bferl_programming_language_logic:get_memory_cell(2, Output)),
    ?assertEqual(0, bferl_programming_language_logic:get_memory_cell(3, Output)),
    ?assertEqual(0, bferl_programming_language_logic:get_memory_cell(4, Output)),

    % Calculation: (5+4+3+2+1)x '[-]' (45), 5x '[>]' (15), 5x '[' and +1 for `end_of_program`.
    ?assertEqual(66, Output#interpreter.instructions_counter).
