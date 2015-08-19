-module(bferl_tools_sup).
-behaviour(supervisor).

-export([ start_link/0 ]).
-export([ init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 5,

    Flags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 1000,
    Type = worker,

    Interpreter = {bferl_tools_interpreter,
                   {bferl_tools_interpreter, start_link, []},
                   Restart, Shutdown, Type,
                   [ bferl_tools_interpreter ]},

    Compiler = {bferl_tools_compiler,
                {bferl_tools_compiler, start_link, []},
                Restart, Shutdown, Type,
                [ bferl_tools_compiler ]},

    VirtualMachine = {bferl_tools_virtual_machine,
                      {bferl_tools_virtual_machine, start_link, []},
                      Restart, Shutdown, Type,
                      [ bferl_tools_virtual_machine ]},

    {ok, {Flags, [ Interpreter, Compiler, VirtualMachine ]}}.
