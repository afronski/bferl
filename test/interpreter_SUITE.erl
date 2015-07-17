-module(interpreter_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/interpreter_definitions.hrl").

-define(TO_STRING(Code), string:join(Code, "")).

-export([ all/0 ]).
-export([ empty_state_should_have_fixed_memory_size/1,
          pointers_should_be_set_at_the_beginning_after_init/1,
          after_loading_program_it_should_be_available_in_state/1,
          you_should_be_able_run_program/1,
          you_should_be_able_step_through_program/1,
          stepping_out_of_the_program_should_not_be_a_problem/1,
          running_partially_stepped_program_should_finish_program_execution/1 ]).

all() -> [ empty_state_should_have_fixed_memory_size,
           pointers_should_be_set_at_the_beginning_after_init,
           after_loading_program_it_should_be_available_in_state,
           you_should_be_able_run_program,
           you_should_be_able_step_through_program,
           stepping_out_of_the_program_should_not_be_a_problem,
           running_partially_stepped_program_should_finish_program_execution ].

empty_state_should_have_fixed_memory_size(_Context) ->
    State = bferl_interpreter:init(),
    ?assertEqual(?MEMORY_SIZE, array:size(State#interpreter.memory)).

pointers_should_be_set_at_the_beginning_after_init(_Context) ->
    State = bferl_interpreter:init(),

    ?assertEqual(undefined, State#interpreter.instructions),

    ?assertEqual(0, State#interpreter.instructions_pointer),
    ?assertEqual(0, State#interpreter.memory_pointer).

after_loading_program_it_should_be_available_in_state(_Context) ->
    State = bferl_interpreter:load(["+"], bferl_interpreter:init()),
    ?assertEqual("+", ?TO_STRING(State#interpreter.instructions)),

    DifferentState = bferl_interpreter:init(["-"]),
    ?assertEqual("-", ?TO_STRING(DifferentState#interpreter.instructions)).

you_should_be_able_run_program(_Context) ->
    State = bferl_interpreter:init(["+", "+", "+"]),
    Output = bferl_interpreter:run(State),

    ?assertEqual(3, Output#interpreter.instructions_pointer),
    ?assertEqual(3, bferl_interpreter:get_memory_cell(0, Output)).

you_should_be_able_step_through_program(_Context) ->
    State = bferl_interpreter:init(["+", "+", "+"]),
    OutputAfterFirstStep = bferl_interpreter:step(State),

    ?assertEqual(1, OutputAfterFirstStep#interpreter.instructions_pointer),
    ?assertEqual(1, bferl_interpreter:get_memory_cell(0, OutputAfterFirstStep)).

stepping_out_of_the_program_should_not_be_a_problem(_Context) ->
    EmptyState = bferl_interpreter:init(),
    no_program_loaded = bferl_interpreter:step(EmptyState),

    State = bferl_interpreter:init([]),
    end_of_program = bferl_interpreter:step(State),

    StateWithNegativeInstructionsPointer = State#interpreter{instructions_pointer = -1},
    end_of_program = bferl_interpreter:step(StateWithNegativeInstructionsPointer).

running_partially_stepped_program_should_finish_program_execution(_Context) ->
    State = bferl_interpreter:init(["+", "+", "+"]),

    StateAfterFirstStep = bferl_interpreter:step(State),
    ?assertEqual(1, StateAfterFirstStep#interpreter.instructions_pointer),

    Output = bferl_interpreter:run(StateAfterFirstStep),
io:format("~p~n", [Output]),
    ?assertEqual(3, Output#interpreter.instructions_pointer).
