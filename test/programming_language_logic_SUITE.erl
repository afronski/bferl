-module(programming_language_logic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/interpreter_definitions.hrl").

-define(TO_STRING(Code), string:join(Code, "")).

-export([ all/0 ]).
-export([ empty_state_should_have_fixed_memory_size/1,
          other_parameters_should_be_set_to_proper_value/1,
          after_registering_io_process_state_should_update_that_field/1,
          pointers_should_be_set_at_the_beginning_after_new/1,
          after_loading_program_it_should_be_available_in_state/1,
          you_should_be_able_run_program/1,
          you_should_be_able_step_through_program/1,
          stepping_out_of_the_program_should_not_be_a_problem/1,
          running_partially_stepped_program_should_finish_program_execution/1 ]).

all() -> [ empty_state_should_have_fixed_memory_size,
           other_parameters_should_be_set_to_proper_value,
           after_registering_io_process_state_should_update_that_field,
           pointers_should_be_set_at_the_beginning_after_new,
           after_loading_program_it_should_be_available_in_state,
           you_should_be_able_run_program,
           you_should_be_able_step_through_program,
           stepping_out_of_the_program_should_not_be_a_problem,
           running_partially_stepped_program_should_finish_program_execution ].

empty_state_should_have_fixed_memory_size(_Context) ->
    State = bferl_programming_language_logic:new(),
    ?assertEqual(?MEMORY_SIZE, array:size(State#interpreter.memory)).

other_parameters_should_be_set_to_proper_value(_Context) ->
    State = bferl_programming_language_logic:new(),

    ?assertEqual([], State#interpreter.stack),

    ?assertEqual(undefined, State#interpreter.instructions),
    ?assertEqual({undefined, undefined, undefined}, State#interpreter.io).

after_registering_io_process_state_should_update_that_field(_Context) ->
    State = bferl_programming_language_logic:new(),

    StateWithTape = bferl_programming_language_logic:register_tape(State),
    ?assertEqual({fun bferl_io:get_character_from_tape/0,
                  fun bferl_io:put_character_to_tape/1,
                  fun bferl_io:new_line_on_tape/0}, StateWithTape#interpreter.io),

    StateWithConsole = bferl_programming_language_logic:register_console(State),
    ?assertEqual({fun bferl_io:get_character_from_console/0,
                  fun bferl_io:put_character_to_console/1,
                  fun bferl_io:new_line_on_console/0}, StateWithConsole#interpreter.io).

pointers_should_be_set_at_the_beginning_after_new(_Context) ->
    State = bferl_programming_language_logic:new(),

    ?assertEqual(0, State#interpreter.instructions_counter),

    ?assertEqual(1, State#interpreter.instructions_pointer),
    ?assertEqual(0, State#interpreter.memory_pointer).

after_loading_program_it_should_be_available_in_state(_Context) ->
    State = bferl_programming_language_logic:load(["+"], bferl_programming_language_logic:new()),
    ?assertEqual("+", ?TO_STRING(State#interpreter.instructions)),

    DifferentState = bferl_programming_language_logic:new(["-"]),
    ?assertEqual("-", ?TO_STRING(DifferentState#interpreter.instructions)).

you_should_be_able_run_program(_Context) ->
    State = bferl_programming_language_logic:new(["+", "+", "+"]),
    Output = bferl_programming_language_logic:run(State),

    ?assertEqual(length(Output#interpreter.instructions) + 1, Output#interpreter.instructions_pointer),
    ?assertEqual(length(Output#interpreter.instructions), Output#interpreter.instructions_counter),

    ?assertEqual("+", bferl_programming_language_logic:get_opcode(1, Output)),
    ?assertEqual("+", bferl_programming_language_logic:get_opcode(2, Output)),
    ?assertEqual("+", bferl_programming_language_logic:get_opcode(3, Output)),

    ?assertEqual(3, bferl_programming_language_logic:get_memory_cell(0, Output)).

you_should_be_able_step_through_program(_Context) ->
    State = bferl_programming_language_logic:new(["+", "+", "+"]),
    OutputAfterFirstStep = bferl_programming_language_logic:step(State),

    ?assertEqual(2, OutputAfterFirstStep#interpreter.instructions_pointer),
    ?assertEqual(1, OutputAfterFirstStep#interpreter.instructions_counter),

    ?assertEqual(1, bferl_programming_language_logic:get_memory_cell(0, OutputAfterFirstStep)).

stepping_out_of_the_program_should_not_be_a_problem(_Context) ->
    EmptyState = bferl_programming_language_logic:new(),
    no_program_loaded = bferl_programming_language_logic:step(EmptyState),

    State = bferl_programming_language_logic:new([]),
    end_of_program = bferl_programming_language_logic:step(State),

    StateWithZeroedPointer = State#interpreter{instructions_pointer = 0},
    end_of_program = bferl_programming_language_logic:step(StateWithZeroedPointer),

    StateWithNegativeInstructionsPointer = State#interpreter{instructions_pointer = -1},
    end_of_program = bferl_programming_language_logic:step(StateWithNegativeInstructionsPointer).

running_partially_stepped_program_should_finish_program_execution(_Context) ->
    State = bferl_programming_language_logic:new(["+", "+", "+"]),

    StateAfterFirstStep = bferl_programming_language_logic:step(State),
    ?assertEqual(2, StateAfterFirstStep#interpreter.instructions_pointer),

    Output = bferl_programming_language_logic:run(StateAfterFirstStep),

    ?assertEqual(length(Output#interpreter.instructions) + 1, Output#interpreter.instructions_pointer),
    ?assertEqual(length(Output#interpreter.instructions), Output#interpreter.instructions_counter).
