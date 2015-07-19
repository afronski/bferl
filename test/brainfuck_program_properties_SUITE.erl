-module(brainfuck_program_properties_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ all/0 ]).
-export([ properties_of_programs_without_loops/1,
          properties_of_programs_with_proper_loops/1 ]).

all() ->
    [ properties_of_programs_without_loops,
      properties_of_programs_with_proper_loops ].

properties_of_programs_without_loops(_Context) ->
    ?assertEqual(true,
                 proper:quickcheck(brainfuck_program_model:prop_programs_without_loops_should_have_IC_and_IP_equal_to_program_length(),
                                   [ {to_file, user}, {numtests, 100} ])).

properties_of_programs_with_proper_loops(_Context) ->
    ?assertEqual(true,
                 proper:quickcheck(brainfuck_program_model:prop_programs_with_proper_loops_should_finish_in_finite_time(),
                                   [ {to_file, user}, {numtests, 100}, {constraint_tries, 100} ])).
