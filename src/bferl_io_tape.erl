-module(bferl_io_tape).
-behaviour(gen_event).

-export([ get_tape/0 ]).

-export([ init/1,
          handle_event/2, handle_call/2, handle_info/2,
          terminate/2, code_change/3 ]).

% TODO: Implement tape.

get_tape() ->
    gen_event:call(bferl_io, get_tape).

init(Tape) ->
    {ok, Tape}.

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
