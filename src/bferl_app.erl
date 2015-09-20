-module(bferl_app).
-behavior(application).

-export([ attach_console/0 ]).
-export([ run_file/1, run_code/1, repl/0,
          compile_file/1, compile_code/1,
          run_file_on_vm/1, run_code_on_vm/1,
          run_file_on_vm/2, run_code_on_vm/2 ]).

-export([ start/2, stop/1 ]).

start(_Type, _Args) ->
    bferl_main_sup:start_link().

stop(_State) ->
    ok.

%% Invoke this directly from the shell.
%% --
%% Otherwise, `erlang:group_leader/0` may return
%% different process identifier, which does not
%% have access to your STDIN.

attach_console() ->
    bferl_io:console(group_leader()).

%% Convenient wrappers.

run_file(Filename) ->
    Program = bferl_tokenizer:from_file(Filename),
    State = bferl_programming_language_logic:new(Program),
    StateWithConsole = bferl_programming_language_logic:register_console(State),
    bferl_programming_language_logic:run(StateWithConsole).

run_code(Code) ->
    Program = bferl_tokenizer:from_string(Code),
    State = bferl_programming_language_logic:new(Program),
    StateWithConsole = bferl_programming_language_logic:register_console(State),
    bferl_programming_language_logic:run(StateWithConsole).

repl() ->
    bferl_repl:start_loop().

compile_file(_Filename) ->
    not_implemented_yet.

compile_code(_Code) ->
    not_implemented_yet.

run_file_on_vm(Filename) ->
    run_file_on_vm(Filename, []).

run_code_on_vm(Code) ->
    run_code_on_vm(Code, []).

run_file_on_vm(_Filename, _Flags) ->
    not_implemented_yet.

run_code_on_vm(_Code, _Flags) ->
    not_implemented_yet.
