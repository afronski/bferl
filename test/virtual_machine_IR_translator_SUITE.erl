-module(virtual_machine_IR_translator_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ all/0 ]).
-export([ individual_opcodes_should_be_translated_properly/1,
          loop_opcodes_should_contain_proper_index/1,
          programs_with_valid_loops_should_be_translated/1,
          programs_without_loops_should_be_translated/1,
          programs_with_invalid_loops_should_not_be_translated/1 ]).

all() ->
    [ individual_opcodes_should_be_translated_properly,
      loop_opcodes_should_contain_proper_index,
      programs_with_valid_loops_should_be_translated,
      programs_without_loops_should_be_translated,
      programs_with_invalid_loops_should_not_be_translated ].

individual_opcodes_should_be_translated_properly(_Context) ->
    ?assertEqual({translation_suceeded, [ {add, r0, 1} ]}, bferl_vm_ir_translator:translate(["+"])),
    ?assertEqual({translation_suceeded, [ {add, r0, -1} ]}, bferl_vm_ir_translator:translate(["-"])),

    ?assertEqual({translation_suceeded, [ {add, ir0, 1} ]}, bferl_vm_ir_translator:translate(["<"])),
    ?assertEqual({translation_suceeded, [ {add, ir0, -1} ]}, bferl_vm_ir_translator:translate([">"])),

    ?assertEqual({translation_suceeded, [ {call, 1} ]}, bferl_vm_ir_translator:translate([","])),
    ?assertEqual({translation_suceeded, [ {call, 2} ]}, bferl_vm_ir_translator:translate(["."])).

loop_opcodes_should_contain_proper_index(_Context) ->
    ?assertEqual({translation_suceeded, [ {test, r0, 2}, {jmp, 1} ]},
                 bferl_vm_ir_translator:translate(["[", "]"])),

    ?assertEqual({translation_suceeded, [ {test, r0, 2}, {jmp, 1}, {test, r0, 4}, {jmp, 3} ]},
                 bferl_vm_ir_translator:translate(["[", "]", "[", "]"])),

    ?assertEqual({translation_suceeded, [ {test, r0, 4}, {test, r0, 3}, {jmp, 2}, {jmp, 1} ]},
                 bferl_vm_ir_translator:translate(["[", "[", "]", "]"])),

    ?assertEqual({translation_suceeded, [ {test, r0, 4}, {test, r0, 3}, {jmp, 2}, {jmp, 1}, {test, r0, 6}, {jmp, 5} ]},
                 bferl_vm_ir_translator:translate(["[", "[", "]", "]", "[", "]"])).

programs_with_valid_loops_should_be_translated(_Context) ->
    ?assertEqual(true,
                 proper:quickcheck(brainfuck_program_model:prop_programs_with_proper_loops_should_be_translated(),
                                   [ {to_file, user}, {numtests, 1000}, {constraint_tries, 200} ])).

programs_without_loops_should_be_translated(_Context) ->
    ?assertEqual(true,
                 proper:quickcheck(brainfuck_program_model:prop_programs_without_loops_should_be_translated(),
                                   [ {to_file, user}, {numtests, 1000}, {constraint_tries, 200} ])).

programs_with_invalid_loops_should_not_be_translated(_Context) ->
    ?assertEqual(true,
                 proper:quickcheck(brainfuck_program_model:prop_programs_with_improper_loops_should_not_be_translated(),
                                   [ {to_file, user}, {numtests, 1000}, {constraint_tries, 200} ])).
