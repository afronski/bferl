-module(bferl_main_sup).
-behaviour(supervisor).

-export([ start_link/0 ]).
-export([ init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = {one_for_all, 0, 1},

    IoSubsystem = {bferl_io, {bferl_io, start_link, []},
                   permanent, 2000, worker, [ bferl_io ]},

    ToolsSupervisor = {bferl_tools_sup, {bferl_tools_sup, start_link, []},
                       permanent, 2000, worker, [ bferl_tools_sup ]},

    Children = [ IoSubsystem, ToolsSupervisor ],

    {ok, {RestartStrategy, Children}}.
