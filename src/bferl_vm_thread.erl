-module(bferl_vm_thread).
-behaviour(gen_server).

-export([ start_link/1 ]).

-export([ init/1,
          handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).

enable_flags({debug, true}, State)       -> State#{ "Debug" => true };
enable_flags({interactive, true}, State) -> State#{ "Interactive" => true };
enable_flags({optimize, true}, State)    -> State#{ "Optimize" => true };
enable_flags({jit, true}, State)         -> State#{ "JIT" => true };
enable_flags(_, State)                   -> State.

enable_flags(Flags) ->
    lists:foldl(fun enable_flags/2, #{}, Flags).

continue({finished, Result}) -> {false, Result};
continue(Intermediate)       -> {true, Intermediate}.

finished(Result) ->
    gen_server:call(bferl_tools_virtual_machine, {thread_finished, Result}).

start_link(Context) ->
    gen_server:start_link(?MODULE, [ Context ], []).

init([ Context ]) ->
    % TODO: If `debug` then stepping is enabled by default.
    % TODO: If `interactive` then each step should be printed.

    % TODO: If `optimize` then optimization stage should modify program first.
    % TODO: If `jit` then JIT will investigate source after each step.

    State = enable_flags(maps:get("Flags", Context)),
    Program = maps:get("Program", Context),

    Machine = bferl_vm_ir_executor:start_machine(Program),

    % TODO: Detect and attach tape when needed.

    MachineWithIO = bferl_vm_ir_executor:register_console(Machine),

    {ok, State#{ "Context" => Context, "Machine" => MachineWithIO }, 0}.

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    Machine = maps:get("Machine", State),

    NewState = case continue(bferl_vm_ir_executor:step(Machine)) of
        {true, Intermediate} ->
            gen_server:cast(self(), step),
            State#{"Machine" := Intermediate};

        {false, Result} ->
            finished(Result),
            State
    end,

    {noreply, NewState}.

handle_info(timeout, State) ->
    gen_server:cast(self(), step),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
