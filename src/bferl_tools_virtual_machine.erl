-module(bferl_tools_virtual_machine).
-behavior(gen_server).

-export([ start_link/0,
          start_vm_thread/3, get_result_for_thread/1 ]).

-export([ init/1,
          handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, empty, []).

start_vm_thread(Program, Type, Flags) ->
    gen_server:call(?MODULE, {start, Program, Type, Flags}).

get_result_for_thread(Pid) ->
    gen_server:call(?MODULE, {get_result, Pid}).

check_jit_and_debug(Flags, true) ->
    case proplists:lookup(jit, Flags) of
        {jit, _} -> {error, "You cannot pass flags `jit` and `debug` together."};
        _        -> {ok, true}
    end;

check_jit_and_debug(_Flags, false) ->
    {ok, false}.

check_optimize_and_debug(Flags, true) ->
    case proplists:lookup(optimize, Flags) of
        {optimize, _} -> {error, "You cannot pass flags `optimize` and `debug` together."};
        _             -> check_jit_and_debug(Flags, true)
    end;

check_optimize_and_debug(Flags, false) ->
    check_jit_and_debug(Flags, false).

verify_flags(Flags) ->
    DebugMode = case proplists:lookup(debug, Flags) of
        {debug, _} -> true;
        _          -> false
    end,

    check_optimize_and_debug(Flags, DebugMode).

pretty_print_when_debug(true, What) ->
    lists:foreach(fun ({Line, Args}) -> io:format(Line, Args) end, What);

pretty_print_when_debug(false, _What) -> ok.

init(empty) ->
    {ok, #{}}.

handle_call({start, Program, Type, Flags}, _From, State) ->
    case verify_flags(Flags) of
        {error, Reason} -> {reply, {error, Reason}, State};
        {ok, DebugMode} ->
            pretty_print_when_debug(DebugMode, [ {"--PROGRAM----------------------------~n~n", []},
                                                 {"~s~n~n", [ Program ]}
                                               ]),

            Opcodes = bferl_vm_ir_translator:translate(Program),

            pretty_print_when_debug(DebugMode, [ {"--VM-OPCODES-------------------------~n~n", []},
                                                 {"~p~n~n", [ Opcodes ]}
                                               ]),

            Context = #{ "Program" => Opcodes, "Type" => Type, "Flags" => Flags, "Result" => undefined },

            {ok, Pid} = bferl_vm_thread_sup:start_vm_thread(Context),
            NewState = State#{ Pid => Context },

            {reply, {started, Pid, Type, Flags}, NewState}
    end;

handle_call({thread_finished, Result}, From, State) ->
    Context = maps:get(From, State, undefined),

    case Context of
        undefined -> {reply, unknown_thread, State};
        _         ->
            NewContext = Context#{ "Result" := Result },
            {reply, acknowledged, State#{ From := NewContext }}
    end;

handle_call({get_result, For}, _From, State) ->
    Context = maps:get(For, State, undefined),
    case Context of
        undefined -> {reply, unknown_thread, State};
        _         ->
            {reply, {result, maps:get("Result", Context)}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
