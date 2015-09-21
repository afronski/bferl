-module(tools_interpreter_validation_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/interpreter_definitions.hrl").

-export([ all/0 ]).
-export([ proper_programs_should_be_validated_successfully/1,
          programs_with_opened_loops_should_be_validated_conditionally/1,
          programs_with_invalid_loops_should_not_be_validated/1 ]).

all() -> [ proper_programs_should_be_validated_successfully,
           programs_with_opened_loops_should_be_validated_conditionally,
           programs_with_invalid_loops_should_not_be_validated ].

proper_programs_should_be_validated_successfully(_Context) ->
    ?assertEqual(valid, bferl_tools_interpreter:validate([])),
    ?assertEqual(valid, bferl_tools_interpreter:validate(["."])),
    ?assertEqual(valid, bferl_tools_interpreter:validate(["+", "+"])),
    ?assertEqual(valid, bferl_tools_interpreter:validate([".", "<", ">"])),
    ?assertEqual(valid, bferl_tools_interpreter:validate(["[", "]"])),
    ?assertEqual(valid, bferl_tools_interpreter:validate(["[", "[", "]", "]"])),
    ?assertEqual(valid, bferl_tools_interpreter:validate(["[", "]", "[", "]"])),
    ?assertEqual(valid, bferl_tools_interpreter:validate(["[", "-", "]", ">", "[", ".", "-", "+", "<", "]"])).

programs_with_opened_loops_should_be_validated_conditionally(_Context) ->
    ?assertEqual(more_tokens, bferl_tools_interpreter:validate(["["])),
    ?assertEqual(more_tokens, bferl_tools_interpreter:validate(["[", "["])),
    ?assertEqual(more_tokens, bferl_tools_interpreter:validate(["[", ".", "["])),
    ?assertEqual(more_tokens, bferl_tools_interpreter:validate(["[", "]", "["])),
    ?assertEqual(more_tokens, bferl_tools_interpreter:validate(["[", "[", "]", "["])),
    ?assertEqual(more_tokens, bferl_tools_interpreter:validate(["[", "[", "]", "]", "["])).

programs_with_invalid_loops_should_not_be_validated(_Context) ->
    ?assertEqual(not_valid, bferl_tools_interpreter:validate(["]"])),
    ?assertEqual(not_valid, bferl_tools_interpreter:validate(["]", "[", "]", "[", "["])),
    ?assertEqual(not_valid, bferl_tools_interpreter:validate(["[", "]", "[", "]", "]"])),
    ?assertEqual(not_valid, bferl_tools_interpreter:validate(["[", "]", "]"])),
    ?assertEqual(not_valid, bferl_tools_interpreter:validate(["[", "[", "[", "]", "]", "]", "]"])),
    ?assertEqual(not_valid, bferl_tools_interpreter:validate([".", "]", ".", "]", ".", "]", ".", "]"])).
