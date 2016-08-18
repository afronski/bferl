-module(bferl_tools_virtual_machine).
-behavior(gen_server).

-export([ start_link/0,
          start_vm_thread/3, get_result_for_thread/1,
          step/1 ]).

-export([ init/1,
          handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, empty, []).

start_vm_thread(Program, Type, Flags) ->
    gen_server:call(?MODULE, {start, Program, Type, Flags}).

get_result_for_thread(Pid) ->
    gen_server:call(?MODULE, {get_result, Pid}).

step(Pid) ->
    gen_server:call(?MODULE, {step, Pid}).

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

parse_and_start_new_vm_thread(_DebugMode, _Type, _Flags, State, translation_error) ->
    {{error, "You have got invalid looping constructs in your program."}, State};

parse_and_start_new_vm_thread(DebugMode, Type, Flags, State, {translation_suceeded, Opcodes}) ->
    pretty_print_when_debug(DebugMode, [ {"--VM-OPCODES-------------------------~n~n", []},
                                         {"~p~n~n", [ Opcodes ]}
                                       ]),

    TapeState = proplists:get_value(tape, Flags, not_attached),
    Tape = case TapeState of
        not_attached -> "[No tape attached]";
        Value        -> Value
    end,

    ClearedFlags = proplists:delete(tape, Flags),

    pretty_print_when_debug(DebugMode, [ {"--TAPE-------------------------------~n~n", []},
                                         {"~p~n~n", [ Tape ]}
                                       ]),

    Context = #{ "Program" => Opcodes, "Type" => Type, "Flags" => ClearedFlags, "Tape" => Tape },

    {ok, Pid} = bferl_vm_threads_sup:start_new_thread(Context),
    NewState = State#{ Pid => Context },

    {{started, Pid, Type, Flags}, NewState}.

init(empty) ->
    {ok, #{ "Results" => #{} }}.

handle_call({start, Program, Type, Flags}, _From, State) ->
    case verify_flags(Flags) of
        {error, Reason} -> {reply, {error, Reason}, State};
        {ok, DebugMode} ->
            pretty_print_when_debug(DebugMode, [ {"--PROGRAM----------------------------~n~n", []},
                                                 {"~s~n~n", [ Program ]} ]),

            ParsingResult = bferl_vm_ir_translator:translate(Program),
            {Reply, NewState} = parse_and_start_new_vm_thread(DebugMode, Type, Flags, State, ParsingResult),

            {reply, Reply, NewState}
    end;

handle_call({get_result, For}, _From, State) ->
    Results = maps:get("Results", State),
    Result = maps:get(For, Results, undefined),

    case Result of
        undefined -> {reply, unknown_thread, State};
        _         -> {reply, {result, Result}, State}
    end;

handle_call({step, For}, _From, State) ->
    IsPresent = maps:is_key(For, State),

    Status = case IsPresent of
        false -> unknown_thread;
        _     -> gen_server:call(For, step)
    end,

    {reply, Status, State}.

handle_cast({thread_finished, From, Result}, State) ->
    Results = maps:get("Results", State),

    ModifiedResults = case maps:is_key(From, Results) of
        true  -> Results;
        false ->
            NewResults = Results#{ From => Result },
            Context = maps:get(From, State),

            case maps:get("Tape", Context) of
                not_attached -> ok;
                _            ->
                    OutputTape = bferl_io:get_output_tape(),
                    io:format("~s~n", [ OutputTape ])
            end,

            NewResults
    end,

    {noreply, State#{ "Results" := ModifiedResults }}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
