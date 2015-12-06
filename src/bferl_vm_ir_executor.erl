-module(bferl_vm_ir_executor).

-include("../include/virtual_machine_definitions.hrl").

-export([ start_machine/1, step/1 ]).

-spec build_jump_entry({pos_integer(), bferl_types:ir_opcode()}, map()) -> map().
build_jump_entry({I, {jze, N}}, Jumps) -> Jumps#{ I => N };
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

-spec execute(bferl_types:ir_opcode(), bferl_types:virtual_machine_state()) -> bferl_types:virtual_machine_state().
execute({load, ir0, r0}, Machine) ->
    IR0 = Machine#register_based_virtual_machine.ir0,
    Memory = Machine#register_based_virtual_machine.memory,

    NewR0 = array:get(IR0, Memory),
    NewZF = case NewR0 of
        0 -> 1;
        _ -> 0
    end,

    Machine#register_based_virtual_machine{r0 = NewR0, zf = NewZF};

execute({add, ir0, N}, Machine) ->
    NewIR0 = Machine#register_based_virtual_machine.ir0 + N,

    Machine#register_based_virtual_machine{ir0 = NewIR0}.

-spec step(bferl_types:virtual_machine_state()) -> bferl_types:virtual_machine_state().
step(Machine) ->
    IP = Machine#register_based_virtual_machine.ip,
    IC = Machine#register_based_virtual_machine.ic,

    Instruction = lists:nth(IP, Machine#register_based_virtual_machine.ir_code),

    Modified = execute(Instruction, Machine),

    Modified#register_based_virtual_machine{ip = IP + 1, ic = IC + 1}.
