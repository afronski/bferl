-module(bferl_tools_compiler).
-behavior(gen_server).

-export([ start_link/0,
          compile_and_load/2,
          compile_and_load/3 ]).

-export([ init/1,
          handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

compile_and_load(Program, Type) ->
    gen_server:call({compile, Program, Type, []}, ?MODULE).

compile_and_load(Program, Type, Flags) ->
    gen_server:call({compile, Program, Type, Flags}, ?MODULE).

create_module_name(State) ->
    EvaluationCounter = maps:get("evaluation_counter", State),
    Name = io_lib:format("evaluation_~B", [EvaluationCounter]),

    {Name, State#{"evaluation_counter" := EvaluationCounter + 1}}.

filename(Name) ->
    io_lib:format("~s.erl", [Name]).

compile(Name, Program, Type, Flags) ->
    case proplists:lookup(debug, Flags) of
        {debug, _} -> io:format("LANGUAGE: ~s~n", [Type]);
        _          -> nop
    end,

    {ok, Tokens, _} = bferl_compiler_lexer:string(Program),
    {ok, Expressions} = bferl_compiler_parser:parse(Tokens),

    {ok, CoreRepresentation} = bferl_compiler_codegen:make_module(Name, Expressions, Flags),

    case proplists:lookup(pretty_print, Flags) of
        {pretty_print, _} ->
            io:format("--PRETTY-PRINT-----------------------~n~n", []),
            io:format("  * Core representation: ~n~n            ", []),
            io:format("      ~s~n~n                             ", [ core_pp:format(CoreRepresentation) ]),
            io:format("-------------------------------------~n  ", []);

       _ -> nop
    end,

    case core_lint:module(CoreRepresentation) of
        {ok, [{Name, []}]} ->
            File = filename(Name),
            {ok, _, Beam} = compile:forms(CoreRepresentation, [binary, from_core, return_errors, {source, File}]),
            {ok, Beam};

        {ok, [{Name, Warnings}]} ->
            {error, [], Warnings};

        {error, [{Name, Errors}], [{Name, Warnings}]} ->
            {error, Errors, Warnings}
    end.

load(Name, Beam) ->
    code:load_binary(Name, filename(Name), Beam).

compile_and_load(Name, Program, Type, Flags) ->
    case compile(Name, Program, Type, Flags) of
        {ok, Beam}                -> load(Name, Beam);
        {error, Errors, Warnings} -> {error, Errors, Warnings}
    end.

init([]) ->
    {ok, #{"evaluation_counter" => 1}}.

handle_call({compile, Program, Type, Flags}, _From, State) ->
    {Name, NewState} = create_module_name(State),
    Reply = compile_and_load(Name, Program, Type, Flags),
    {reply, Reply, NewState}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
