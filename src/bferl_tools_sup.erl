-module(bferl_tools_sup).
-behaviour(supervisor).

-export([ start_link/0 ]).
-export([ init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    RestartStrategy = {one_for_one, 0, 1},

    Interpreter = {bferl_tools_interpreter, {bferl_tools_interpreter, start_link, []},
                   permanent, 2000, worker, [ bferl_tools_interpreter ]},

    Compiler = {bferl_tools_compiler, {bferl_tools_compiler, start_link, []},
                permanent, 2000, worker, [ bferl_tools_compiler ]},

    VirtualMachine = {bferl_tools_virtual_machine, {bferl_tools_virtual_machine, start_link, []},
                      permanent, 2000, worker, [ bferl_tools_virtual_machine ]},

    VmThreadsSupervisor = {bferl_vm_threads_sup, {bferl_vm_threads_sup, start_link, []},
                           permanent, 2000, supervisor, [ bferl_vm_threads_sup ]},

    Children = [ Interpreter, Compiler, VirtualMachine, VmThreadsSupervisor ],

    {ok, {RestartStrategy, Children}}.
