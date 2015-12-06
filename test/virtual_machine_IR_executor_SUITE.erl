-module(virtual_machine_IR_executor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/virtual_machine_definitions.hrl").

-export([ all/0 ]).
-export([ virtual_machine_state_should_be_returned_after_start_up/1,
          virtual_machine_state_should_contain_delivered_program/1,
          jump_table_should_be_built_automatically_after_starting_up_machine/1,
          values_in_jump_table_should_correspond_to_initial_pass/1 ]).

all() ->
    [ virtual_machine_state_should_be_returned_after_start_up,
      virtual_machine_state_should_contain_delivered_program,
      jump_table_should_be_built_automatically_after_starting_up_machine,
      values_in_jump_table_should_correspond_to_initial_pass ].

virtual_machine_state_should_be_returned_after_start_up(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([]),

    ?assertEqual(true, is_record(Machine, register_based_virtual_machine)).

virtual_machine_state_should_contain_delivered_program(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {add, r0, 3} ]),

    ?assertEqual(1, length(Machine#register_based_virtual_machine.ir_code)),
    ?assertEqual({add, r0, 3}, hd(Machine#register_based_virtual_machine.ir_code)).

jump_table_should_be_built_automatically_after_starting_up_machine(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([]),

    ?assertEqual(true, is_map(Machine#register_based_virtual_machine.jmp_table)).

values_in_jump_table_should_correspond_to_initial_pass(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {test, r0, 2}, {jmp, 1},
                                                   {test, r0, 5}, {add, ir0, 0}, {jmp, 3} ]),

    ?assertEqual(2, maps:get(1, Machine#register_based_virtual_machine.jmp_table)),
    ?assertEqual(1, maps:get(2, Machine#register_based_virtual_machine.jmp_table)),

    ?assertEqual(5, maps:get(3, Machine#register_based_virtual_machine.jmp_table)),
    ?assertEqual(3, maps:get(5, Machine#register_based_virtual_machine.jmp_table)),

    %% There should not be key '4' in jump table (it is an `add` opcode).
    ?assertEqual(-1, maps:get(4, Machine#register_based_virtual_machine.jmp_table, -1)).
