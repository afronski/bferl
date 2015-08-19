-module(bferl_main_sup).
-behaviour(supervisor).

-export([ start_link/0 ]).
-export([ init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 5,

    Flags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 1000,
    Type = worker,

    IoSubsystem = {bferl_io,
                   {bferl_io, start_link, []},
                   Restart, Shutdown, Type,
                   [ bferl_io ]},

    ToolsSupervisor = {bferl_tools_sup,
                       {bferl_tools_sup, start_link, []},
                       Restart, Shutdown, Type,
                       [ bferl_tools_sup ]},

    {ok, {Flags, [ IoSubsystem, ToolsSupervisor ]}}.
