-module(bferl_io_console).
-behaviour(gen_event).

-export([ init/1,
          handle_event/2, handle_call/2, handle_info/2,
          terminate/2, code_change/3 ]).

% TODO: Implement console.

init([]) ->
    {ok, []}.

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
