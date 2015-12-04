-module(bferl_vm_thread).
-behaviour(gen_server).

-export([ start_link/1 ]).

-export([ init/1,
          handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).

start_link(Context) ->
    gen_server:start_link(?MODULE, [ Context ], []).

init([ Context ]) ->
    {ok, Context}.

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
