-module(bferl_compiler_codegen).

-include("../include/common_definitions.hrl").
-include("../include/interpreter_definitions.hrl").

-export([ make_module/4 ]).

map_opcode(inc)        -> "+";
map_opcode(dec)        -> "-";
map_opcode(left)       -> "<";
map_opcode(right)      -> ">";
map_opcode(in)         -> ",";
map_opcode(out)        -> ".";
map_opcode(start_loop) -> "[";
map_opcode(end_loop)   -> "]";

map_opcode(fork)       -> "Y".

print_opcodes_in_debug(Opcode, Flags) ->
    case proplists:lookup(debug, Flags) of
        {debug, _} -> io:format("~1s", [map_opcode(Opcode)]);
        none       -> nop
    end.

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
    %% TODO: Nicer output.
    Args = [cerl:c_string("DEBUG: ~p~n"), cerl:make_list([In])],
    cerl:c_seq(cerl:c_call(cerl:c_atom(io), cerl:c_atom(format), Args), In).

codegen_new_array() ->
    Options = [
        cerl:c_tuple([cerl:c_atom(size), cerl:c_int(?MEMORY_SIZE)]),
        cerl:c_tuple([cerl:c_atom(fixed), cerl:c_atom(true)]),
        cerl:c_tuple([cerl:c_atom(default), cerl:c_int(0)])
    ],

    cerl:c_call(cerl:c_atom(array), cerl:c_atom(new), [cerl:make_list(Options)]).

codegen_deferred_application(Instruction, release) ->
    StateIn = cerl:c_var('StateIn'),
    cerl:c_fun([StateIn], cerl:c_apply(cerl:c_fname(Instruction, 1), [StateIn]));

codegen_deferred_application(Instruction, debug) ->
    StateIn = cerl:c_var('StateIn'),

    DebugPrint = [cerl:c_fun([StateIn], cerl:c_apply(cerl:c_fname(print, 1), [StateIn]))],
    [codegen_deferred_application(Instruction, release)] ++ DebugPrint.

codegen_state(Program, release, In) ->
    Memory = cerl:c_var('Memory'),

    cerl:c_let(
        [Memory],
        codegen_new_array(),
        cerl:c_tuple([
            cerl:c_int(0),
            cerl:c_int(1), cerl:make_list(Program),
            cerl:c_int(0), Memory,
            cerl:c_nil(),
            In
        ]));

codegen_state(Program, debug, In) ->
    Result = cerl:c_var('Result'),

    cerl:c_let(
        [Result],
        codegen_state(Program, release, In),
        cerl:c_seq(
            cerl:c_apply(cerl:c_fname(print, 1), [Result]),
            Result
        )).

codegen_new_state(Program, Mode, In) ->
    ProgramCoreRepresentation = lists:flatten(lists:map(fun(I) -> codegen_deferred_application(I, Mode) end, Program)),
    codegen_state(ProgramCoreRepresentation, Mode, In).

codegen_execute(In) ->
    State = cerl:c_var('State'),
    Final = cerl:c_var('Final'),

    Element = cerl:c_var('Element'),
    In = cerl:c_var('In'),

    Program = cerl:c_var('Program'),

    Step = cerl:c_fun([Element, In], cerl:c_apply(Element, [In])),

    cerl:c_let(
        [State],
        cerl:c_apply(cerl:c_fname(build_state, 1), [In]),
        cerl:c_let(
            [Final],
            cerl:c_let(
                [Program],
                cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(element), [cerl:c_int(3), State]),
                cerl:c_call(cerl:c_atom(lists), cerl:c_atom(foldl), [Step, State, Program])
            ),
            cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(element), [cerl:c_int(1), Final])
        )
    ).

codegen_brainfuck(Program, Mode, Flags) ->
    In = cerl:c_var('In'),

    DebugFunctions = case proplists:lookup(debug, Flags) of
        {debug, _} -> [{cerl:c_fname(print, 1), cerl:c_fun([In], codegen_print(In))}];
        none       -> []
    end,

    DebugFunctions ++ [
        {cerl:c_fname(build_state, 1), cerl:c_fun([In], codegen_new_state(Program, Mode, In))},

        {cerl:c_fname(execute, 1), cerl:c_fun([In], codegen_execute(In))},

        %% TODO: Implementation in *Core Erlang*.

        {cerl:c_fname(inc_ip, 1), cerl:c_fun([In], In)},
        {cerl:c_fname(inc_ic, 1), cerl:c_fun([In], In)},

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

    %% TODO: Implementation in *Core Erlang*.

    [{cerl:c_fname(fork, 1), cerl:c_fun([In], In)}].

codegen_language_library(Program, Mode, ?HUMAN_NAME_BF, Flags) ->
    codegen_brainfuck(Program, Mode, Flags);

codegen_language_library(Program, Mode, ?HUMAN_NAME_BFO, Flags) ->
    codegen_brainfuck(Program, Mode, Flags) ++ codegen_brainfork().

codegen_main(Input) ->
    cerl:c_apply(cerl:c_fname(execute, 1), [Input]).

build(ParsedProgram, Input, Flags) ->
    Mode = case proplists:lookup(debug, Flags) of
        {debug, _} ->
            io:format("--PROCEEDED-TOKENS-------------------~n  ", []),
            lists:foreach(fun(Opcode) -> print_opcodes_in_debug(Opcode, Flags) end, ParsedProgram),
            debug;

        none -> release
    end,

    Result = codegen_main(Input),

    case proplists:lookup(debug, Flags) of
        {debug, _} -> io:format("~n", []);
        none       -> nop
    end,

    {Result, Mode}.

codegen_module(Name, ParsedProgram, Type, Flags) ->
    ModuleName = cerl:c_atom(Name),

    Input = cerl:c_var('Input'),
    {ParsedProgramCoreRepresentation, Mode} = build(ParsedProgram, Input, Flags),

    Start1Name = cerl:c_fname(start, 1),
    Start1 = {Start1Name, cerl:c_fun([Input], ParsedProgramCoreRepresentation)},

    Start0Name = cerl:c_fname(start, 0),
    Start0 = {Start0Name, cerl:c_fun([], cerl:c_call(ModuleName, cerl:c_atom(start), [cerl:c_nil()]))},

    {ModuleInfoNames, ModuleInfoFunctions} = codegen_module_info(ModuleName),
    BrainfuckFunctions = codegen_language_library(ParsedProgram, Mode, Type, Flags),

    Exports = ModuleInfoNames ++ [Start0Name, Start1Name],
    Definitions = ModuleInfoFunctions ++ BrainfuckFunctions ++ [Start0, Start1],

    {ok, cerl:c_module(ModuleName, Exports, Definitions)}.

make_module(Name, Expressions, Type, Flags) ->
    codegen_module(Name, Expressions, Type, Flags).
