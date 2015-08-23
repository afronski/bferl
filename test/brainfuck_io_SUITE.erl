-module(brainfuck_io_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/interpreter_definitions.hrl").

-export([ all/0, init_per_testcase/2, end_per_testcase/2 ]).
-export([ testing_input_and_output/1, testing_hello_world/1,
          testing_loop_with_input_and_output/1,
          testing_nontrivial_programs_adding_two_digits_and_displaying_result_if_it_is_a_digit/1 ]).

all() ->
    [ testing_input_and_output,
      testing_hello_world,
      testing_loop_with_input_and_output,
      testing_nontrivial_programs_adding_two_digits_and_displaying_result_if_it_is_a_digit ].

init_per_testcase(_TestCase, Config) ->
    {ok, Pid} = bferl_io:start_link(),
    [ {bferl_io, Pid} | Config ].

end_per_testcase(_TestCase, Config) ->
    Pid = proplists:get_value(bferl_io, Config),
    exit(Pid, normal),
    ok.

testing_input_and_output(_Context) ->
    State = bferl_programming_language_logic:new([",", "+", "."]),

    bferl_io:tape("A"),
    StateWithIO = bferl_programming_language_logic:register_tape(State),

    Output = bferl_programming_language_logic:run(StateWithIO),

    Tape = bferl_io:get_output_tape(),

    ?assertEqual("B", Tape),
    ?assertEqual(length(Output#interpreter.instructions), Output#interpreter.instructions_counter).

testing_hello_world(_Context) ->
    Program = bferl_tokenizer:from_file("../../../../test/assets/hello_world.bf"),
    State = bferl_programming_language_logic:new(Program),

    bferl_io:tape(""),
    StateWithIO = bferl_programming_language_logic:register_tape(State),

    bferl_programming_language_logic:run(StateWithIO),
    Tape = bferl_io:get_output_tape(),

    ?assertEqual("Hello World!\n", Tape).

testing_loop_with_input_and_output(_Context) ->
    State = bferl_programming_language_logic:new([",", "[", ".", ",", "]"]),

    bferl_io:tape("ABC"),
    StateWithIO = bferl_programming_language_logic:register_tape(State),

    Output = bferl_programming_language_logic:run(StateWithIO),
    Tape = bferl_io:get_output_tape(),

    ?assertEqual("ABC", Tape),

    % Calculation: 1x ',', 3x '[.,]' and +1 for `end_of_program`.
    ?assertEqual(1 + 3 * 4 + 1, Output#interpreter.instructions_counter).

testing_nontrivial_programs_adding_two_digits_and_displaying_result_if_it_is_a_digit(_Context) ->
    State = bferl_programming_language_logic:new(bferl_tokenizer:from_string(",>++++++[<-------->-],[<+>-]<.")),

    bferl_io:tape("45"),
    StateWithIO = bferl_programming_language_logic:register_tape(State),

    bferl_programming_language_logic:run(StateWithIO),

    ?assertEqual("9", bferl_io:get_output_tape()).
