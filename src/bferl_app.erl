-module(bferl_app).
-behavior(application).

-include("../include/common_definitions.hrl").

-export([ attach_console/0 ]).
-export([ run_file/1, run_code/1, repl/0,
          compile_file/1, compile_code/1,
          compile_file/2, compile_code/2,
          run_file_on_vm/1, run_code_on_vm/1,
          run_file_on_vm/2, run_code_on_vm/2,
          run_file_on_vm/3, run_code_on_vm/3 ]).

-export([ start/2, stop/1 ]).

start(_Type, _Args) ->
    bferl_main_sup:start_link().

stop(_State) ->
    ok.

%% Private helpers.

detect_type(Filename) ->
    case filename:extension(Filename) of
        ?BRAINFORK_EXTENSION -> ?HUMAN_NAME_BFO;
        _                    -> ?HUMAN_NAME_BF
    end.

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

compile_file(Filename) ->
    Program = bferl_tokenizer:from_file(Filename),
    Type = detect_type(Filename),
    bferl_tools_compiler:compile_and_load(Program, Type).

compile_file(Filename, debug) ->
    Program = bferl_tokenizer:from_file(Filename),
    Type = detect_type(Filename),
    bferl_tools_compiler:compile_and_load(Program, Type, [debug, pretty_print]).

compile_code(Code) ->
    Program = bferl_tokenizer:from_string(Code),
    bferl_tools_compiler:compile_and_load(Program, ?HUMAN_NAME_BF).

compile_code(Code, debug) ->
    Program = bferl_tokenizer:from_string(Code),
    bferl_tools_compiler:compile_and_load(Program, ?HUMAN_NAME_BF, [debug, pretty_print]).

run_file_on_vm(Filename) ->
    Parsed = bferl_tokenizer:from_file(Filename),
    Type = detect_type(Filename),
    bferl_tools_virtual_machine:start_vm_thread(Parsed, Type, [optimize, jit]).

run_code_on_vm(Code) ->
    Parsed = bferl_tokenizer:from_string(Code),
    {ok, Pid} = bferl_tools_virtual_machine:start_vm_thread(Parsed, ?HUMAN_NAME_BF, [optimize, jit]),
    bferl_tools_virtual_machine:run_program(Pid).

run_file_on_vm(Filename, debug) ->
    Parsed = bferl_tokenizer:from_file(Filename),
    Type = detect_type(Filename),
    {ok, Pid} = bferl_tools_virtual_machine:start_vm_thread(Parsed, Type, [debug, interactive]),
    bferl_tools_virtual_machine:run_program(Pid);
run_file_on_vm(Filename, Tape) ->
    Parsed = bferl_tokenizer:from_file(Filename),
    Type = detect_type(Filename),
    {ok, Pid} = bferl_tools_virtual_machine:start_vm_thread(Parsed, Type, [optimize, jit, {tape, Tape}]),
    bferl_tools_virtual_machine:run_program(Pid).

run_code_on_vm(Code, debug) ->
    Parsed = bferl_tokenizer:from_string(Code),
    {ok, Pid} = bferl_tools_virtual_machine:start_vm_thread(Parsed, ?HUMAN_NAME_BF, [interactive, debug]),
    bferl_tools_virtual_machine:run_program(Pid);
run_code_on_vm(Code, Tape) ->
    Parsed = bferl_tokenizer:from_string(Code),
    {ok, Pid} = bferl_tools_virtual_machine:start_vm_thread(Parsed, ?HUMAN_NAME_BF, [optimize, jit, {tape, Tape}]),
    bferl_tools_virtual_machine:run_program(Pid).

run_file_on_vm(Filename, Tape, debug) ->
    Parsed = bferl_tokenizer:from_file(Filename),
    Type = detect_type(Filename),
    {ok, Pid} = bferl_tools_virtual_machine:start_vm_thread(Parsed, Type, [debug, interactive, {tape, Tape}]),
    bferl_tools_virtual_machine:run_program(Pid).

run_code_on_vm(Code, Tape, debug) ->
    Parsed = bferl_tokenizer:from_string(Code),
    {ok, Pid} = bferl_tools_virtual_machine:start_vm_thread(Parsed, ?HUMAN_NAME_BF, [interactive, debug, {tape, Tape}]),
    bferl_tools_virtual_machine:run_program(Pid).
