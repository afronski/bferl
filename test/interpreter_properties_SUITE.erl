-module(interpreter_properties_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ all/0 ]).
-export([ programs_without_loops_should_be_always_valid/1 ]).

all() ->
    [ programs_without_loops_should_be_always_valid ].

programs_without_loops_should_be_always_valid(_Context) ->
    ?assertEqual(true, proper:quickcheck(interpreter_properties:prop_test(), [ {to_file, user} ])).
