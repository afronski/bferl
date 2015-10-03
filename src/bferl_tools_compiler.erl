-module(bferl_tools_compiler).
-behavior(gen_server).

-include("../include/common_definitions.hrl").

-export([ start_link/0,
          compile_and_load/2,
          compile_and_load/3 ]).

-export([ init/1,
          handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

compile_and_load(Program, Type) ->
    gen_server:call(?MODULE, {compile, Program, Type, []}).

compile_and_load(Program, Type, Flags) ->
    gen_server:call(?MODULE, {compile, Program, Type, Flags}).

create_module_name(State) ->
    EvaluationCounter = maps:get("evaluation_counter", State),
    Name = "evaluation_" ++ integer_to_list(EvaluationCounter),

    {Name, State#{"evaluation_counter" := EvaluationCounter + 1}}.

filename(Name) ->
    Name ++ ".erl".

lex(Program, ?HUMAN_NAME_BF)  -> bferl_compiler_lexer_bf:string(Program);
lex(Program, ?HUMAN_NAME_BFO) -> bferl_compiler_lexer_bfo:string(Program).

parse(Tokens, ?HUMAN_NAME_BF)  -> bferl_compiler_parser_bf:parse(Tokens);
parse(Tokens, ?HUMAN_NAME_BFO) -> bferl_compiler_parser_bfo:parse(Tokens).

codegen({error, _}, Name, Type, Flags) ->
    {error, [], syntax_error, [Name, Type, Flags]};

codegen({ok, Expressions}, Name, Type, Flags) ->
    {ok, CoreRepresentation} = bferl_compiler_codegen:make_module(Name, Expressions, Type, Flags),

    case proplists:lookup(pretty_print, Flags) of
        {pretty_print, _} ->
            io:format("--CORE-ERLANG-PRETTY-PRINT-----------~n~n", []),
            io:format("~s~n~n", [ core_pp:format(CoreRepresentation) ]);

        _ -> nop
    end,

    AtomName = list_to_existing_atom(Name),

    case core_lint:module(CoreRepresentation) of
        {ok, [{AtomName, []}]} ->
            case proplists:lookup(debug, Flags) of
                {debug, _} -> io:format("--VALIDATION: SUCCESS----------------~n", []);
                _          -> nop
            end,

            File = filename(Name),
            {ok, _, Beam} = compile:forms(CoreRepresentation, [binary, from_core, return_errors, {source, File}]),
            {ok, Beam};

        {ok, [{AtomName, Warnings}]} ->
            {error, [], Warnings, [Name, Type, Flags]};

        {error, [{AtomName, Errors}], [{AtomName, Warnings}]} ->
            {error, Errors, Warnings, [Name, Type, Flags]}
    end.

compile(Name, Program, Type, Flags) ->
    case proplists:lookup(debug, Flags) of
        {debug, _} ->
            io:format("--LANGUAGE---------------------------~n", []),
            io:format("  ~s~n", [Type]),
            io:format("--FLAGS------------------------------~n", []),
            io:format("  ~p~n", [Flags]);

        _ -> nop
    end,

    {ok, Tokens, _} = lex(Program, Type),
    ParseResult = parse(Tokens, Type),

    codegen(ParseResult, Name, Type, Flags).

load(Name, Beam) ->
    AtomName = list_to_existing_atom(Name),
    code:load_binary(AtomName, filename(Name), Beam).

compile_and_load(Name, Program, Type, Flags) ->
    case compile(Name, Program, Type, Flags) of
        {ok, Beam}                      -> load(Name, Beam);
        {error, Errors, Warnings, Meta} -> {error, Errors, Warnings, Meta}
    end.

init([]) ->
    {ok, #{"evaluation_counter" => 1}}.

handle_call({compile, Program, Type, Flags}, _From, State) ->
    {Name, NewState} = create_module_name(State),
    Reply = compile_and_load(Name, string:join(Program, ""), Type, Flags),
    {reply, Reply, NewState}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
