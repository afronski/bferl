-module(bferl_io_console).
-behaviour(gen_event).

-include("../include/interpreter_definitions.hrl").

-export([ init/1,
          handle_event/2, handle_call/2, handle_info/2,
          terminate/2, code_change/3 ]).

init([ GroupLeader ]) ->
    group_leader(GroupLeader, self()),
    {ok, empty}.

handle_event({put_character, [ Char | _ ] = S}, State) when is_list(S) ->
    io:format("~c", [ Char ]),
    {ok, State};

handle_event({put_character, Char}, State) ->
    io:format("~c", [ Char ]),
    {ok, State}.

handle_call(get_character, State) ->
    [ Char | _ ] = io:get_line(?BRAINFUCK_PROMPT),
    {ok, Char, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
