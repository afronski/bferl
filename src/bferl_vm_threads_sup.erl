-module(bferl_vm_threads_sup).
-behaviour(supervisor).

-export([ start_link/0, start_new_thread/1 ]).
-export([ init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_new_thread(Context) ->
    supervisor:start_child(?MODULE, [ Context ]).

init(_Args) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    Flags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Element = {bferl_vm_thread,
               {bferl_vm_thread, start_link, []},
               temporary, brutal_kill, worker,
               [ bferl_vm_thread ]},

    {ok, {Flags, [ Element ]}}.
