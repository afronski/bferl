-module(bferl_vm_ir_executor).

-include("../include/virtual_machine_definitions.hrl").

-export([ start_machine/1 ]).

-spec build_jump_entry({pos_integer(), bferl_types:ir_opcode()}, map()) -> map().
build_jump_entry({I, {test, r0, N}}, Jumps) -> Jumps#{ I => N };
build_jump_entry({I, {jmp, N}}, Jumps) -> Jumps#{ I => N };
build_jump_entry(_, Jumps) -> Jumps.

-spec build_jump_table(bferl_types:ir_program()) -> map().
build_jump_table(Program) ->
    ProgramWithIndexes = lists:zip(lists:seq(1, length(Program)), Program),
    lists:foldl(fun build_jump_entry/2, #{}, ProgramWithIndexes).

-spec start_machine(bferl_types:ir_program()) -> bferl_types:virtual_machine_state().
start_machine(Program) when is_list(Program) ->
    Jumps = build_jump_table(Program),
    #register_based_virtual_machine{ ir_code = Program, jmp_table = Jumps }.
