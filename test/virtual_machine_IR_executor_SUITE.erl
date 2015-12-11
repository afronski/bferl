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
          register_should_be_updated_when_we_add_one_to_it/1,
          register_should_be_updated_when_we_perform_more_complicated_operations/1,
          value_should_be_loaded_from_corresponding_memory_address/1,
          zero_flag_should_be_set_when_value_loaded_to_r0_is_equal_to_zero/1,
          zero_flag_should_not_be_set_when_value_loaded_to_r0_is_not_equal_to_zero/1,
          storing_should_save_provided_value_from_register_to_corresponding_memory_address/1,
          constants_should_be_loaded_to_both_registers_with_one_instruction/1,
          after_loading_constant_to_R0_zero_flag_should_be_set_apropriately/1,
          unconditional_jump_should_set_up_IP_always/1,
          conditional_jump_when_0_should_set_up_IP_only_when_zero_flag_is_set/1,
          conditional_jump_when_not_0_should_set_up_IP_only_when_zero_flag_is_not_set/1,
          program_with_valid_jumps_should_terminate_in_finite_amount_of_steps/1,
          program_with_valid_jumps_and_valid_pointer_movements_should_terminate_in_finite_amount_of_steps/1,
          in_call_should_read_one_character_to_current_memory_cell/1,
          out_call_should_print_one_character_from_current_memory_cell/1,
          in_and_out_calls_should_be_able_to_share_state_through_memory/1,
          hello_world_program_without_optimizations_should_finish_and_output_proper_string/1 ]).

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
      register_should_be_updated_when_we_add_one_to_it,
      register_should_be_updated_when_we_perform_more_complicated_operations,
      value_should_be_loaded_from_corresponding_memory_address,
      zero_flag_should_be_set_when_value_loaded_to_r0_is_equal_to_zero,
      zero_flag_should_not_be_set_when_value_loaded_to_r0_is_not_equal_to_zero,
      storing_should_save_provided_value_from_register_to_corresponding_memory_address,
      constants_should_be_loaded_to_both_registers_with_one_instruction,
      after_loading_constant_to_R0_zero_flag_should_be_set_apropriately,
      unconditional_jump_should_set_up_IP_always,
      conditional_jump_when_0_should_set_up_IP_only_when_zero_flag_is_set,
      conditional_jump_when_not_0_should_set_up_IP_only_when_zero_flag_is_not_set,
      program_with_valid_jumps_should_terminate_in_finite_amount_of_steps,
      program_with_valid_jumps_and_valid_pointer_movements_should_terminate_in_finite_amount_of_steps,
      in_call_should_read_one_character_to_current_memory_cell,
      out_call_should_print_one_character_from_current_memory_cell,
      in_and_out_calls_should_be_able_to_share_state_through_memory,
      hello_world_program_without_optimizations_should_finish_and_output_proper_string ].

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
    Machine = bferl_vm_ir_executor:start_machine([ {add, ir0, 5}, {sub, ir0, 1}, {add, ir0, -1}, {sub, ir0, -1} ]),
    Final = bferl_vm_ir_executor:run(Machine),

    ?assertEqual(4, Final#register_based_virtual_machine.ir0).

register_should_be_updated_when_we_add_one_to_it(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {add, r0, 1} ]),
    Final = bferl_vm_ir_executor:run(Machine),

    ?assertEqual(1, Final#register_based_virtual_machine.r0).

register_should_be_updated_when_we_perform_more_complicated_operations(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {add, r0, 5}, {sub, r0, 1}, {add, r0, -1}, {sub, r0, -1} ]),
    Final = bferl_vm_ir_executor:run(Machine),

    ?assertEqual(4, Final#register_based_virtual_machine.r0).

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

storing_should_save_provided_value_from_register_to_corresponding_memory_address(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {add, ir0, 1}, {add, r0, 54}, {store, r0, ir0} ]),

    Final = bferl_vm_ir_executor:run(Machine),
    Memory = Final#register_based_virtual_machine.memory,

    ?assertEqual(54, array:get(1, Memory)).

constants_should_be_loaded_to_both_registers_with_one_instruction(_Context) ->
    Machine = bferl_vm_ir_executor:start_machine([ {const, ir0, 6}, {const, r0, 10} ]),

    Final = bferl_vm_ir_executor:run(Machine),

    ?assertEqual(6, Final#register_based_virtual_machine.ir0),
    ?assertEqual(10, Final#register_based_virtual_machine.r0).

after_loading_constant_to_R0_zero_flag_should_be_set_apropriately(_Context) ->
    MachineFor0 = bferl_vm_ir_executor:start_machine([ {const, r0, 0} ]),
    FinalFor0 = bferl_vm_ir_executor:run(MachineFor0),

    ?assertEqual(1, FinalFor0#register_based_virtual_machine.zf),

    MachineFor1 = bferl_vm_ir_executor:start_machine([ {const, r0, 1} ]),
    FinalFor1 = bferl_vm_ir_executor:run(MachineFor1),

    ?assertEqual(0, FinalFor1#register_based_virtual_machine.zf).

unconditional_jump_should_set_up_IP_always(_Context) ->
    MachineFor0 = bferl_vm_ir_executor:start_machine([ {load, ir0, r0}, {jmp, 100} ]),
    FinalFor0 = bferl_vm_ir_executor:run(MachineFor0),

    ?assertEqual(100, FinalFor0#register_based_virtual_machine.ip),

    MachineFor1 = bferl_vm_ir_executor:start_machine([ {load, ir0, r0}, {jmp, 120} ]),
    MemoryFor1 = array:set(0, 1, MachineFor1#register_based_virtual_machine.memory),
    MachineWithMemoryUpdateFor1 = MachineFor1#register_based_virtual_machine{memory = MemoryFor1},

    FinalFor1 = bferl_vm_ir_executor:run(MachineWithMemoryUpdateFor1),

    ?assertEqual(120, FinalFor1#register_based_virtual_machine.ip).

conditional_jump_when_0_should_set_up_IP_only_when_zero_flag_is_set(_Context) ->
    MachineFor0 = bferl_vm_ir_executor:start_machine([ {load, ir0, r0}, {jze, 100} ]),
    FinalFor0 = bferl_vm_ir_executor:run(MachineFor0),

    ?assertEqual(100, FinalFor0#register_based_virtual_machine.ip),

    MachineFor1 = bferl_vm_ir_executor:start_machine([ {load, ir0, r0}, {jze, 120} ]),
    MemoryFor1 = array:set(0, 1, MachineFor1#register_based_virtual_machine.memory),
    MachineWithMemoryUpdateFor1 = MachineFor1#register_based_virtual_machine{memory = MemoryFor1},

    FinalFor1 = bferl_vm_ir_executor:run(MachineWithMemoryUpdateFor1),

    ?assertEqual(3, FinalFor1#register_based_virtual_machine.ip).

conditional_jump_when_not_0_should_set_up_IP_only_when_zero_flag_is_not_set(_Context) ->
    MachineFor0 = bferl_vm_ir_executor:start_machine([ {load, ir0, r0}, {jnze, 100} ]),
    FinalFor0 = bferl_vm_ir_executor:run(MachineFor0),

    ?assertEqual(3, FinalFor0#register_based_virtual_machine.ip),

    MachineFor1 = bferl_vm_ir_executor:start_machine([ {load, ir0, r0}, {jnze, 120} ]),
    MemoryFor1 = array:set(0, 1, MachineFor1#register_based_virtual_machine.memory),
    MachineWithMemoryUpdateFor1 = MachineFor1#register_based_virtual_machine{memory = MemoryFor1},

    FinalFor1 = bferl_vm_ir_executor:run(MachineWithMemoryUpdateFor1),

    ?assertEqual(120, FinalFor1#register_based_virtual_machine.ip).

program_with_valid_jumps_should_terminate_in_finite_amount_of_steps(_Context) ->
    {translation_suceeded, Program} = bferl_vm_ir_translator:translate([ "+", "+", "[", "-", "]" ]),

    Machine = bferl_vm_ir_executor:start_machine(Program),
    Final = bferl_vm_ir_executor:run(Machine),

    %% IC = 2x3 '+', 2x6 '[-]', 1x for last jump, 1x for last pass.
    ?assertEqual(6 + 12 + 1 + 1, Final#register_based_virtual_machine.ic).

program_with_valid_jumps_and_valid_pointer_movements_should_terminate_in_finite_amount_of_steps(_Context) ->
    {translation_suceeded, Program} = bferl_vm_ir_translator:translate([ "+", "+", "[", ">", "+", "<", "-", "]" ]),

    Machine = bferl_vm_ir_executor:start_machine(Program),
    Final = bferl_vm_ir_executor:run(Machine),

    %% IC = 2x3 '+', 2x11 '[>+<-]', 1x for last jump, 1x for last pass.
    ?assertEqual(6 + 22 + 1 + 1, Final#register_based_virtual_machine.ic),

    ?assertEqual(0, array:get(0, Final#register_based_virtual_machine.memory)),
    ?assertEqual(2, array:get(1, Final#register_based_virtual_machine.memory)).

in_call_should_read_one_character_to_current_memory_cell(_Context) ->
    {ok, Pid} = bferl_io:start_link(),
    bferl_io:tape("A"),

    Machine = bferl_vm_ir_executor:start_machine([ {call, in} ]),
    MachineWithIO = bferl_vm_ir_executor:register_tape(Machine),

    Final = bferl_vm_ir_executor:run(MachineWithIO),

    ?assertEqual(65, array:get(0, Final#register_based_virtual_machine.memory)),

    exit(Pid, normal).

out_call_should_print_one_character_from_current_memory_cell(_Context) ->
    {ok, Pid} = bferl_io:start_link(),
    bferl_io:tape([]),

    Machine = bferl_vm_ir_executor:start_machine([ {const, r0, 65}, {store, r0, ir0}, {call, out} ]),
    MachineWithIO = bferl_vm_ir_executor:register_tape(Machine),

    bferl_vm_ir_executor:run(MachineWithIO),
    Output = bferl_io:get_output_tape(),

    ?assertEqual("A", Output),

    exit(Pid, normal).

in_and_out_calls_should_be_able_to_share_state_through_memory(_Context) ->
    {ok, Pid} = bferl_io:start_link(),
    bferl_io:tape("A"),

    Machine = bferl_vm_ir_executor:start_machine([ {call, in},
                                                   {load, ir0, r0}, {add, r0, 1}, {store, r0, ir0},
                                                   {call, out} ]),

    MachineWithIO = bferl_vm_ir_executor:register_tape(Machine),

    bferl_vm_ir_executor:run(MachineWithIO),
    Output = bferl_io:get_output_tape(),

    ?assertEqual("B", Output),

    exit(Pid, normal).

hello_world_program_without_optimizations_should_finish_and_output_proper_string(_Context) ->
    {ok, Pid} = bferl_io:start_link(),
    bferl_io:tape([]),

    Opcodes = bferl_tokenizer:from_file("../../../../test/assets/hello_world.bf"),
    {translation_suceeded, Program} = bferl_vm_ir_translator:translate(Opcodes),

    Machine = bferl_vm_ir_executor:start_machine(Program),
    MachineWithIO = bferl_vm_ir_executor:register_tape(Machine),

    bferl_vm_ir_executor:run(MachineWithIO),
    Output = bferl_io:get_output_tape(),

    ?assertEqual("Hello World!\n", Output),

    exit(Pid, normal).
