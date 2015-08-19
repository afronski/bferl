-module(bferl_io_tape).
-behaviour(gen_event).

-export([ init/1,
          handle_event/2, handle_call/2, handle_info/2,
          terminate/2, code_change/3 ]).

init([ InputTape ]) ->
    {ok, {InputTape, []}}.

handle_event({put_character, Char}, {Input, Output}) ->
    {ok, {Input, Output ++ [ Char ]}}.

handle_call(get_character, {[], Output}) ->
    {ok, eof, {[], Output}};

handle_call(get_character, {[Char | Rest], Output}) ->
    {ok, Char, {Rest, Output}};

handle_call(get_tape, {_, Output} = State) ->
    {ok, Output, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
