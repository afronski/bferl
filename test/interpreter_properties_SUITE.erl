-module(interpreter_properties_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ all/0 ]).
-export([ programs_without_loops_should_be_always_valid/1,
          programs_with_proper_loops_should_be_always_valid/1 ]).

all() ->
    [ programs_without_loops_should_be_always_valid,
      programs_with_proper_loops_should_be_always_valid ].

programs_without_loops_should_be_always_valid(_Context) ->
    ?assertEqual(true,
                 proper:quickcheck(interpreter_properties:prop_programs_without_loops_should_have_IC_and_IP_equal_to_program_length(),
                                   [ {to_file, user}, {numtests, 100} ])).

programs_with_proper_loops_should_be_always_valid(_Context) ->
    ?assertEqual(true,
                 proper:quickcheck(interpreter_properties:prop_programs_with_proper_loops_should_execute_properly(),
                                   [ {to_file, user}, {numtests, 100}, {constraint_tries, 100} ])).
