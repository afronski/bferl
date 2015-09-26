-module(bferl_compiler_codegen).

-include("../include/common_definitions.hrl").

-export([ make_module/4 ]).

%% We need to replicate `module_info/{0,1}` functions
%% because they are delivered to the Core Erlang representation.

codegen_module_info(ModuleName) ->
    M = cerl:c_atom(erlang),
    F = cerl:c_atom(get_module_info),

    Info0Name = cerl:c_fname(module_info, 0),
    Info0 = {Info0Name, cerl:c_fun([], cerl:c_call(M, F, [ModuleName]))},

    Key = cerl:c_var('Key'),
    Info1Name = cerl:c_fname(module_info, 1),
    Info1 = {Info1Name, cerl:c_fun([Key], cerl:c_call(M, F, [ModuleName, Key]))},

    {[Info0Name, Info1Name], [Info0, Info1]}.

codegen_print(In) ->
    Args = [cerl:c_string("DEBUG: ~p~n"), cerl:make_list([In])],
    cerl:c_seq(cerl:c_call(cerl:c_atom(io), cerl:c_atom(format), Args), In).

codegen_brainfuck(Flags) ->
    In = cerl:c_var('In'),

    DebugFunctions = case proplists:lookup(debug, Flags) of
        {debug, _} -> [{cerl:c_fname(print, 1), cerl:c_fun([In], codegen_print(In))}];
        none       -> []
    end,

    %% TODO: Implementation in *Core Erlang*.

    DebugFunctions ++ [
        {cerl:c_fname(build_state, 1), cerl:c_fun([In], In)},

        {cerl:c_fname(inc, 1), cerl:c_fun([In], In)},
        {cerl:c_fname(dec, 1), cerl:c_fun([In], In)},

        {cerl:c_fname(left, 1), cerl:c_fun([In], In)},
        {cerl:c_fname(right, 1), cerl:c_fun([In], In)},

        {cerl:c_fname(in, 1), cerl:c_fun([In], In)},
        {cerl:c_fname(out, 1), cerl:c_fun([In], In)},

        {cerl:c_fname(start_loop, 1), cerl:c_fun([In], In)},
        {cerl:c_fname(end_loop, 1), cerl:c_fun([In], In)}
    ].

codegen_brainfork() ->
    In = cerl:c_var('In'),

    [{cerl:c_fname(fork, 1), cerl:c_fun([In], In)}].

codegen_language_library(?HUMAN_NAME_BF, Flags)  -> codegen_brainfuck(Flags);
codegen_language_library(?HUMAN_NAME_BFO, Flags) -> codegen_brainfuck(Flags) ++ codegen_brainfork().

map_opcode(inc)        -> "+";
map_opcode(dec)        -> "-";
map_opcode(left)       -> "<";
map_opcode(right)      -> ">";
map_opcode(in)         -> ",";
map_opcode(out)        -> ".";
map_opcode(start_loop) -> "[";
map_opcode(end_loop)   -> "]";
map_opcode(fork)       -> "Y".

print_instruction(Opcode, Flags) ->
    case proplists:lookup(debug, Flags) of
        {debug, _} -> io:format("~1s", [map_opcode(Opcode)]);
        none       -> nop
    end.

opcode_in_erlang_core(inc, Body)        -> cerl:c_apply(cerl:c_fname(inc, 1), [Body]);
opcode_in_erlang_core(dec, Body)        -> cerl:c_apply(cerl:c_fname(dec, 1), [Body]);
opcode_in_erlang_core(left, Body)       -> cerl:c_apply(cerl:c_fname(left, 1), [Body]);
opcode_in_erlang_core(right, Body)      -> cerl:c_apply(cerl:c_fname(right, 1), [Body]);
opcode_in_erlang_core(in, Body)         -> cerl:c_apply(cerl:c_fname(in, 1), [Body]);
opcode_in_erlang_core(out, Body)        -> cerl:c_apply(cerl:c_fname(out, 1), [Body]);
opcode_in_erlang_core(start_loop, Body) -> cerl:c_apply(cerl:c_fname(start_loop, 1), [Body]);
opcode_in_erlang_core(end_loop, Body)   -> cerl:c_apply(cerl:c_fname(end_loop, 1), [Body]);
opcode_in_erlang_core(fork, Body)       -> cerl:c_apply(cerl:c_fname(fork, 1), [Body]).

opcode_in_erlang_core_debug(inc, Body) ->
    cerl:c_apply(cerl:c_fname(print, 1), [opcode_in_erlang_core(inc, Body)]);

opcode_in_erlang_core_debug(dec, Body) ->
    cerl:c_apply(cerl:c_fname(print, 1), [opcode_in_erlang_core(dec, Body)]);

opcode_in_erlang_core_debug(left, Body) ->
    cerl:c_apply(cerl:c_fname(print, 1), [opcode_in_erlang_core(left, Body)]);

opcode_in_erlang_core_debug(right, Body) ->
    cerl:c_apply(cerl:c_fname(print, 1), [opcode_in_erlang_core(right, Body)]);

opcode_in_erlang_core_debug(in, Body) ->
    cerl:c_apply(cerl:c_fname(print, 1), [opcode_in_erlang_core(in, Body)]);

opcode_in_erlang_core_debug(out, Body) ->
    cerl:c_apply(cerl:c_fname(print, 1), [opcode_in_erlang_core(out, Body)]);

opcode_in_erlang_core_debug(start_loop, Body) ->
    cerl:c_apply(cerl:c_fname(print, 1), [opcode_in_erlang_core(start_loop, Body)]);

opcode_in_erlang_core_debug(end_loop, Body) ->
    cerl:c_apply(cerl:c_fname(print, 1), [opcode_in_erlang_core(end_loop, Body)]);

opcode_in_erlang_core_debug(fork, Body) ->
    cerl:c_apply(cerl:c_fname(print, 1), [opcode_in_erlang_core(fork, Body)]).

build_record(release) ->
    cerl:c_apply(cerl:c_fname(build_state, 1), [cerl:c_var('Input')]);

build_record(debug) ->
    cerl:c_apply(cerl:c_fname(print, 1), [cerl:c_apply(cerl:c_fname(build_state, 1), [cerl:c_var('Input')])]).

codegen_instructions(I, [H | T], debug)   -> opcode_in_erlang_core_debug(I, codegen_instructions(H, T, debug));
codegen_instructions(I, [], debug)        -> opcode_in_erlang_core_debug(I, build_record(debug));

codegen_instructions(I, [H | T], release) -> opcode_in_erlang_core(I, codegen_instructions(H, T, release));
codegen_instructions(I, [], release)      -> opcode_in_erlang_core(I, build_record(release)).

build(ParsedProgram, Flags) ->
    Mode = case proplists:lookup(debug, Flags) of
        {debug, _} ->
            io:format("--PROCEEDED-TOKENS-------------------~n  ", []),
            debug;

        none -> release
    end,

    lists:foreach(fun(Opcode) -> print_instruction(Opcode, Flags) end, ParsedProgram),

    [H | T] = lists:reverse(ParsedProgram),
    Result = codegen_instructions(H, T, Mode),

    case proplists:lookup(debug, Flags) of
        {debug, _} -> io:format("~n", []);
        none       -> nop
    end,

    Result.

codegen_module(Name, ParsedProgram, Type, Flags) ->
    ModuleName = cerl:c_atom(Name),

    ParsedProgramCoreRepresentation = build(ParsedProgram, Flags),

    Input = cerl:c_var('Input'),
    Start1Name = cerl:c_fname(start, 1),
    Start1 = {Start1Name, cerl:c_fun([Input], ParsedProgramCoreRepresentation)},

    Start0Name = cerl:c_fname(start, 0),
    Start0 = {Start0Name, cerl:c_fun([], cerl:c_call(ModuleName, cerl:c_atom(start), [cerl:c_nil()]))},

    {ModuleInfoNames, ModuleInfoFunctions} = codegen_module_info(ModuleName),
    BrainfuckFunctions = codegen_language_library(Type, Flags),

    Exports = ModuleInfoNames ++ [Start0Name, Start1Name],
    Definitions = ModuleInfoFunctions ++ BrainfuckFunctions ++ [Start0, Start1],

    {ok, cerl:c_module(ModuleName, Exports, Definitions)}.

make_module(Name, Expressions, Type, Flags) ->
    codegen_module(Name, Expressions, Type, Flags).
