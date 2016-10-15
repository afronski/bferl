-module(bferl_vm_thread).
-behaviour(gen_server).

-include("../include/virtual_machine_definitions.hrl").

-export([ start_link/1 ]).

-export([ init/1,
          handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).

enable_flags({debug, true}, State)       -> State#{ "Debug" => true };
enable_flags(debug, State)               -> State#{ "Debug" => true };
enable_flags({interactive, true}, State) -> State#{ "Interactive" => true };
enable_flags(interactive, State)         -> State#{ "Interactive" => true };
enable_flags({optimize, true}, State)    -> State#{ "Optimize" => true };
enable_flags(optimize, State)            -> State#{ "Optimize" => true };
enable_flags({jit, true}, State)         -> State#{ "JIT" => true };
enable_flags(jit, State)                 -> State#{ "JIT" => true };
enable_flags(_, State)                   -> State.

enable_flags(Flags) ->
    lists:foldl(fun enable_flags/2, #{}, Flags).

continue({finished, Result}) -> {false, Result};
continue(Intermediate)       -> {true, Intermediate}.

finished(Result) ->
    gen_server:cast(bferl_tools_virtual_machine, {thread_finished, self(), Result}),
    Result.

start_link(Context) ->
    gen_server:start_link(?MODULE, [ Context ], []).

init([ Context ]) ->
    State = enable_flags(maps:get("Flags", Context)),
    Program = maps:get("Program", Context),

    Machine = bferl_vm_ir_executor:start_machine(Program),

    MachineWithIO = case maps:get("Tape", Context) of
        not_attached ->
            bferl_vm_ir_executor:register_console(Machine);

        Tape ->
            bferl_io:tape(Tape),
            bferl_vm_ir_executor:register_tape(Machine)
    end,

    NewState = State#{ "Context" => Context, "Machine" => MachineWithIO },
    pretty_print_when_interactive(NewState, MachineWithIO),

    {ok, NewState}.

handle_call(step, _From, State) ->
    Machine = maps:get("Machine", State),

    {Status, NewMachine} = case continue(bferl_vm_ir_executor:step(Machine)) of
        {true, Intermediate} ->
            pretty_print_when_interactive(State, Intermediate),
            {running, Intermediate};

        {false, Result} ->
            {finished, finished(Result)}
    end,

    {reply, Status, State#{ "Machine" := NewMachine }}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

map_opcode_to_string({add, T, V})   -> io_lib:format("add(~p, ~B)", [T, V]);
map_opcode_to_string({sub, T, V})   -> io_lib:format("sub(~p, ~B)", [T, V]);

map_opcode_to_string({jze, T})      -> io_lib:format("jze(~B)", [T]);
map_opcode_to_string({jmp, T})      -> io_lib:format("jmp(~B)", [T]);
map_opcode_to_string({jnze, T})     -> io_lib:format("jnze(~B)", [T]);

map_opcode_to_string({const, T, V}) -> io_lib:format("const(~p, ~B)", [T, V]);

map_opcode_to_string({load, _, _})  -> "load(ir0, r0)";
map_opcode_to_string({store, _, _}) -> "store(r0, ir0)";

map_opcode_to_string({call, in})    -> "call(in)";
map_opcode_to_string({call, out})   -> "call(out)";
map_opcode_to_string({call, fork})  -> "call(fork)".

get_memory_cell(CellIndex, Machine) when CellIndex >= 0, CellIndex < ?VM_MEMORY_SIZE ->
    array:get(CellIndex, Machine#register_based_virtual_machine.memory);
get_memory_cell(_CellIndex, _Machine) ->
    0.

get_opcode(Index, Machine) when Index >= 1, Index =< length(Machine#register_based_virtual_machine.ir_code) ->
    map_opcode_to_string(lists:nth(Index, Machine#register_based_virtual_machine.ir_code));
get_opcode(_Index, _Machine) ->
    "".

pretty_print_when_interactive(State, _Machine) ->
    case maps:get("Interactive", State, false) of
        false -> ok;
        true  ->
            Context = maps:get("Context", State),

            {Input, Output} = case maps:get("Tape", Context, not_attached) of
                not_attached -> {"[No tape attached]", "[No tape attached]"};
                _            -> {bferl_io:get_input_tape(), bferl_io:get_output_tape()}
            end,

            DebugModeState = maps:get("Debug", State, false),
            DebugMode = case DebugModeState of
                true  -> "D";
                false -> "-"
            end,

            InteractiveModeState = maps:get("Interactive", State, false),
            InteractiveMode = case InteractiveModeState of
                true  -> "I";
                false -> "-"
            end,

            OptModeState = maps:get("Optimize", State, false),
            OptMode = case OptModeState of
                true  -> "O";
                false -> "-"
            end,

            JitModeState = maps:get("JIT", State, false),
            JitMode = case JitModeState of
                true  -> "J";
                false -> "-"
            end,

            Machine = maps:get("Machine", State),

            IP = Machine#register_based_virtual_machine.ip,
            IC = Machine#register_based_virtual_machine.ic,

            ZF = Machine#register_based_virtual_machine.zf,

            IR0 = Machine#register_based_virtual_machine.ir0,
            R0 = Machine#register_based_virtual_machine.r0,

            MM2 = get_memory_cell(IR0 - 2, Machine),
            MM1 = get_memory_cell(IR0 - 1, Machine),
            M0  = get_memory_cell(IR0    , Machine),
            MP1 = get_memory_cell(IR0 + 1, Machine),
            MP2 = get_memory_cell(IR0 + 2, Machine),

            Memory = [ MM2, MM1, M0, MP1, MP2, R0],

            OpcodeM2 = get_opcode(IP - 2, Machine),
            OpcodeM1 = get_opcode(IP - 1, Machine),
            Opcode0  = get_opcode(IP    , Machine),
            OpcodeP1 = get_opcode(IP + 1, Machine),
            OpcodeP2 = get_opcode(IP + 2, Machine),

            io:format("--MEMORY-FLAGS-AND-REGISTERS-------~n", []),
            io:format(".. ~3B ~3B (~3B) ~3B ~3B ..  R0: ~3B~n", Memory),
            io:format("..  -2  -1    0   +1  +2 .. IR0: ~3B~n", [IR0]),
            io:format("..                           IP: ~3B~n", [IP]),
            io:format("..                           ZF: ~3B~n", [ZF]),
            io:format("..                           IC: ~3B~n", [IC]),
            io:format("--CODE--------------------[~1s~1s][~1s~1s]-~n", [DebugMode, InteractiveMode, OptMode, JitMode]),
            io:format("   -2               ~s~n", [OpcodeM2]),
            io:format("   -1               ~s~n", [OpcodeM1]),
            io:format("    0 (IP: ~3B) ==> ~s~n", [IP, Opcode0]),
            io:format("   +1               ~s~n", [OpcodeP1]),
            io:format("   +2               ~s~n", [OpcodeP2]),
            io:format("--TAPE-----------------------------~n", []),
            io:format("Input:  ~s~n", [Input]),
            io:format("Output: ~s~n", [Output]),
            io:format("-----------------------------------~n", [])
    end.
