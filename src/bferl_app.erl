-module(bferl_app).
-behavior(application).

-export([ attach_console/0 ]).
-export([ start/2, stop/1 ]).

start(_Type, _Args) ->
    bferl_main_sup:start_link().

stop(_State) ->
    ok.

attach_console() ->
    bferl_io:console(group_leader()).
