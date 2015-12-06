-module(virtual_machine_IR_executor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/virtual_machine_definitions.hrl").

-export([ all/0 ]).
-export([ virtual_machine_state_should_be_returned_after_start_up/1,
          virtual_machine_state_should_contain_delivered_program/1,
          virtual_machine_state_should_have_fixed_memory_size/1,
          virtual_machine_state_should_have_two_registers/1,
          virtual_machine_state_should_have_intruction_counter/1,
          virtual_machine_state_should_have_intruction_pointer/1,
          virtual_machine_state_should_have_zero_flag/1,
          jump_table_should_be_built_automatically_after_starting_up_machine/1,
          values_in_jump_table_should_correspond_to_initial_pass/1,
          instruction_pointer_and_counter_should_be_increased_after_step/1,
          instruction_pointer_and_counter_should_be_increased_after_run/1,
          address_should_be_updated_when_we_add_offset_to_it/1,
          address_should_be_updated_when_we_perform_more_complicated_operations/1,
          value_should_be_loaded_from_corresponding_memory_address/1,
          zero_flag_should_be_set_when_value_loaded_to_r0_is_equal_to_zero/1,
          zero_flag_should_not_be_set_when_value_loaded_to_r0_is_not_equal_to_zero/1 ]).

all() ->
    [ virtual_machine_state_should_be_returned_after_start_up,
      virtual_machine_state_should_contain_delivered_program,
      virtual_machine_state_should_have_fixed_memory_size,
      virtual_machine_state_should_have_two_registers,
      virtual_machine_state_should_have_intruction_counter,
      virtual_machine_state_should_have_intruction_pointer,
      virtual_machine_state_should_have_zero_flag,
      jump_table_should_be_built_automatically_after_starting_up_machine,
      instruction_pointer_and_counter_should_be_increased_after_step,
      instruction_pointer_and_counter_should_be_increased_after_run,
      values_in_jump_table_should_correspond_to_initial_pass,
      address_should_be_updated_when_we_add_offset_to_it,
      address_should_be_updated_when_we_perform_more_complicated_operations,
      value_should_be_loaded_from_corresponding_memory_address,
      zero_flag_should_be_set_when_value_loaded_to_r0_is_equal_to_zero,
      zero_flag_should_not_be_set_when_value_loaded_to_r0_is_not_equal_to_zero ].

virtual_machine_state_should_be_returned_after_start_up(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([]),

    ?assertEqual(true, is_record(Machine, register_based_virtual_machine)).

virtual_machine_state_should_contain_delivered_program(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {add, ir0, 0} ]),

    ?assertEqual(1, length(Machine#register_based_virtual_machine.ir_code)),
    ?assertEqual({add, ir0, 0}, hd(Machine#register_based_virtual_machine.ir_code)).

virtual_machine_state_should_have_fixed_memory_size(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([]),

    ?assertEqual(?VM_MEMORY_SIZE, array:size(Machine#register_based_virtual_machine.memory)).

virtual_machine_state_should_have_two_registers(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([]),

    ?assertEqual(0, Machine#register_based_virtual_machine.r0),
    ?assertEqual(0, Machine#register_based_virtual_machine.ir0).

virtual_machine_state_should_have_intruction_counter(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([]),

    ?assertEqual(0, Machine#register_based_virtual_machine.ic).

virtual_machine_state_should_have_intruction_pointer(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([]),

    ?assertEqual(1, Machine#register_based_virtual_machine.ip).

virtual_machine_state_should_have_zero_flag(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([]),

    ?assertEqual(0, Machine#register_based_virtual_machine.zf).

jump_table_should_be_built_automatically_after_starting_up_machine(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([]),

    ?assertEqual(true, is_map(Machine#register_based_virtual_machine.jmp_table)).

values_in_jump_table_should_correspond_to_initial_pass(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {load, ir0, r0}, {jze, 4},
                                                   {jmp, 1},
                                                   {load, ir0, r0}, {jze, 8},
                                                   {add, ir0, 0},
                                                   {jmp, 4} ]),

    ?assertEqual(4, maps:get(2, Machine#register_based_virtual_machine.jmp_table)),
    ?assertEqual(1, maps:get(3, Machine#register_based_virtual_machine.jmp_table)),

    ?assertEqual(8, maps:get(5, Machine#register_based_virtual_machine.jmp_table)),
    ?assertEqual(4, maps:get(7, Machine#register_based_virtual_machine.jmp_table)),

    %% There should not be key '1' in jump table (it is an `add` opcode).
    ?assertEqual(-1, maps:get(1, Machine#register_based_virtual_machine.jmp_table, -1)).

instruction_pointer_and_counter_should_be_increased_after_step(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {add, ir0, 0} ]),
    Final = bferl_vm_ir_executor:step(Machine),

    ?assertEqual(2, Final#register_based_virtual_machine.ip),
    ?assertEqual(1, Final#register_based_virtual_machine.ic).

instruction_pointer_and_counter_should_be_increased_after_run(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {add, ir0, 0}, {add, ir0, 0} ]),
    Final = bferl_vm_ir_executor:run(Machine),

    ?assertEqual(3, Final#register_based_virtual_machine.ip),
    ?assertEqual(2, Final#register_based_virtual_machine.ic).

address_should_be_updated_when_we_add_offset_to_it(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {add, ir0, 1} ]),
    Final = bferl_vm_ir_executor:run(Machine),

    ?assertEqual(1, Final#register_based_virtual_machine.ir0).

address_should_be_updated_when_we_perform_more_complicated_operations(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {add, ir0, 5}, {sub, ir0, 1}, {add, ir0, -1}, {sub, ir0, -2} ]),
    Final = bferl_vm_ir_executor:run(Machine),

    ?assertEqual(5, Final#register_based_virtual_machine.ir0).

value_should_be_loaded_from_corresponding_memory_address(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {add, ir0, 1}, {load, ir0, r0} ]),

    Memory = Machine#register_based_virtual_machine.memory,
    Modified = Machine#register_based_virtual_machine{memory = array:set(1, 100, Memory)},

    Final = bferl_vm_ir_executor:run(Modified),

    ?assertEqual(100, Final#register_based_virtual_machine.r0).

zero_flag_should_be_set_when_value_loaded_to_r0_is_equal_to_zero(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {load, ir0, r0} ]),
    Final = bferl_vm_ir_executor:run(Machine),

    ?assertEqual(1, Final#register_based_virtual_machine.zf).

zero_flag_should_not_be_set_when_value_loaded_to_r0_is_not_equal_to_zero(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {load, ir0, r0} ]),

    Memory = Machine#register_based_virtual_machine.memory,
    Modified = Machine#register_based_virtual_machine{memory = array:set(0, 1, Memory)},

    Final = bferl_vm_ir_executor:run(Modified),

    ?assertEqual(0, Final#register_based_virtual_machine.zf).
