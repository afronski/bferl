-module(bferl_vm_thread).
-behaviour(gen_server).

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

    {MachineWithIO, Timeout} = case maps:get("Tape", Context) of
        not_attached ->
            {bferl_vm_ir_executor:register_console(Machine), 10};

        Tape ->
            bferl_io:tape(Tape),
            {bferl_vm_ir_executor:register_tape(Machine), 0}
    end,

    NewState = State#{ "Context" => Context, "Machine" => MachineWithIO },
    pretty_print_when_interactive(NewState, MachineWithIO),

    {ok, NewState, Timeout}.

handle_call(step, _From, State) ->
    Machine = maps:get("Machine", State),

    {Status, NewMachine} = case continue(bferl_vm_ir_executor:step(Machine)) of
        {true, Intermediate} ->
            pretty_print_when_interactive(State, Intermediate),
            step(State),

            {running, Intermediate};

        {false, Result} ->
            {finished, finished(Result)}
    end,

    {reply, Status, State#{ "Machine" := NewMachine }}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    step(State),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

step(State) ->
    case maps:get("Debug", State, false) of
        true  -> ok;
        false -> gen_server:call(self(), step)
    end.

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

            io:format("--MEMORY---------------------------~n", []),
            io:format("--CODE--------------------[~1s~1s][~1s~1s]-~n", [DebugMode, InteractiveMode, OptMode, JitMode]),
            io:format("--TAPE-----------------------------~n", []),
            io:format("Input:  ~s~n", [Input]),
            io:format("Output: ~s~n", [Output]),
            io:format("-----------------------------------~n", [])
    end.
