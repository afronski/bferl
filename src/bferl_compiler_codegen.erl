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

print_opcode(Opcode)   -> io:format("~1s", [map_opcode(Opcode)]).

%% `cerl` helpers.

local_call(Name, Arity, Args) when is_atom(Name), is_integer(Arity), is_list(Args) ->
    cerl:c_apply(cerl:c_fname(Name, Arity), Args).

call(Module, Name, Args) when is_atom(Module), is_atom(Name), is_list(Args) ->
    cerl:c_call(cerl:c_atom(Module), cerl:c_atom(Name), Args).

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

%% Helpers.

codegen_get(N, In) ->
    Args = [cerl:c_int(N), In],
    call(erlang, element, Args).

codegen_safe_list_nth(In, Offset) ->
    Program = cerl:c_var('Program'),
    ProgramLength = cerl:c_var('ProgramLength'),
    Position = cerl:c_var('Position'),
    IP = cerl:c_var('IP'),

    cerl:c_let(
        [Program],
        codegen_get(8, In),
        cerl:c_let(
            [ProgramLength],
            call(erlang, length, [Program]),
            cerl:c_let(
                [IP],
                local_call(get_ip, 1, [In]),
                cerl:c_let(
                    [Position],
                    call(erlang, '+', [IP, Offset]),
                    cerl:c_case(
                        call(erlang, '<', [Position, cerl:c_int(1)]),
                        [ cerl:c_clause([cerl:c_atom(true)], cerl:c_char(32)),
                          cerl:c_clause([cerl:c_atom(false)],
                              cerl:c_case(
                                  call(erlang, '>', [Position, ProgramLength]),
                                  [ cerl:c_clause([cerl:c_atom(true)], cerl:c_char(32)),
                                    cerl:c_clause([cerl:c_atom(false)],
                                        call(lists, nth, [Position, Program])
                                    )
                                  ]
                              )
                          )
                        ]
                    )
                )
            )
        )
    ).

codegen_safe_array_get(In, Offset) ->
    Memory = cerl:c_var('Memory'),
    MemoryLength = cerl:c_var('MemoryLength'),
    Position = cerl:c_var('Position'),
    MP = cerl:c_var('MP'),

    cerl:c_let(
        [Memory],
        codegen_get(5, In),
        cerl:c_let(
            [MemoryLength],
            call(array, size, [Memory]),
            cerl:c_let(
                [MP],
                local_call(get_mp, 1, [In]),
                cerl:c_let(
                    [Position],
                    call(erlang, '+', [MP, Offset]),
                    cerl:c_case(
                        call(erlang, '<', [Position, cerl:c_int(0)]),
                        [ cerl:c_clause([cerl:c_atom(true)], cerl:c_int(0)),
                          cerl:c_clause([cerl:c_atom(false)],
                              cerl:c_case(
                                  call(erlang, '>', [Position, MemoryLength]),
                                  [ cerl:c_clause([cerl:c_atom(true)], cerl:c_int(0)),
                                    cerl:c_clause([cerl:c_atom(false)],
                                        call(array, get, [Position, Memory])
                                    )
                                  ]
                              )
                          )
                        ]
                    )
                )
            )
        )
    ).

%% Debugging facilities.

codegen_print(In) ->
    Pointers = [
        local_call(get_ic, 1, [In]),
        local_call(get_ip, 1, [In]),
        local_call(get_mp, 1, [In])
    ],

    Memory = [
        local_call(get_memory_offset, 2, [In, cerl:c_int(-2)]),
        local_call(get_memory_offset, 2, [In, cerl:c_int(-1)]),
        local_call(get_memory_offset, 2, [In, cerl:c_int( 0)]),
        local_call(get_memory_offset, 2, [In, cerl:c_int(+1)]),
        local_call(get_memory_offset, 2, [In, cerl:c_int(+2)])
    ],

    Program = [
        local_call(get_program_offset, 2, [In, cerl:c_int(-2)]),
        local_call(get_program_offset, 2, [In, cerl:c_int(-1)]),
        local_call(get_program_offset, 2, [In, cerl:c_int( 0)]),
        local_call(get_program_offset, 2, [In, cerl:c_int(+1)]),
        local_call(get_program_offset, 2, [In, cerl:c_int(+2)])
    ],

    TapeCodeGen = codegen_get(7, In),

    PointersPrint = [cerl:c_string("[DEBUG] POINTERS: IC: ~2B IP: ~2B MP: ~2B~n"), cerl:make_list(Pointers)],
    MemoryPrint   = [cerl:c_string("        MEMORY:  ~3B ~3B (~3B) ~3B ~3B~n"), cerl:make_list(Memory)],
    ProgramPrint  = [cerl:c_string("        PROGRAM:   ~1c   ~1c (  ~1c)   ~1c   ~1c~n"), cerl:make_list(Program)],
    InputPrint    = [cerl:c_string("        INPUT:    ~p~n"), cerl:make_list([TapeCodeGen])],

    cerl:c_seq(
        cerl:c_seq(
            cerl:c_seq(
                cerl:c_seq(
                   call(io, format, PointersPrint),
                   call(io, format, MemoryPrint)
                ),
                call(io, format, ProgramPrint)
            ),
            call(io, format, InputPrint)
        ),
        In
    ).

%% State and program structure.

codegen_new_array() ->
    Options = [
        cerl:c_tuple([cerl:c_atom(size), cerl:c_int(?MEMORY_SIZE)]),
        cerl:c_tuple([cerl:c_atom(fixed), cerl:c_atom(true)]),
        cerl:c_tuple([cerl:c_atom(default), cerl:c_int(0)])
    ],

    call(array, new, [cerl:make_list(Options)]).

codegen_deferred_application(Instruction, release) ->
    StateIn = cerl:c_var('StateIn'),
    cerl:c_fun([StateIn], local_call(Instruction, 1, [StateIn]));

codegen_deferred_application(Instruction, debug) ->
    StateIn = cerl:c_var('StateIn'),

    DebugPrint = [cerl:c_fun([StateIn], local_call(print, 1, [StateIn]))],
    [codegen_deferred_application(Instruction, release)] ++ DebugPrint.

codegen_state(Program, StringifiedCode, release, In) ->
    Memory = cerl:c_var('Memory'),

    cerl:c_let(
        [Memory],
        codegen_new_array(),
        cerl:c_tuple([
            cerl:c_int(0),
            cerl:c_int(1), cerl:make_list(Program),
            cerl:c_int(0), Memory,
            cerl:c_nil(),
            In,
            cerl:c_string(StringifiedCode)
        ]));

codegen_state(Program, Code, debug, In) ->
    Result = cerl:c_var('Result'),

    cerl:c_let(
        [Result],
        codegen_state(Program, Code, release, In),
        cerl:c_seq(
            local_call(print, 1, [Result]),
            Result
        )).

codegen_new_state(Program, Mode, In) ->
    ProgramCoreRepresentation = lists:flatten(lists:map(fun(I) -> codegen_deferred_application(I, Mode) end, Program)),
    StringifiedCode = string:join(lists:map(fun(I) -> map_opcode(I) end, Program), ""),
    codegen_state(ProgramCoreRepresentation, StringifiedCode, Mode, In).

%% Language logic and other helpers.

codegen_set(Element, Value, In) ->
    call(erlang, setelement, [cerl:c_int(Element), In, Value]).

codegen_inc(Element, In) ->
    codegen_update(Element, In, '+', 1).

codegen_dec(Element, In) ->
    codegen_update(Element, In, '-', 1).

codegen_update(Element, In, Modifier, Amount) ->
    Value = cerl:c_var('Value'),
    Changed = cerl:c_var('Changed'),

    cerl:c_let(
        [Value],
        codegen_get(Element, In),
        cerl:c_let(
            [Changed],
            call(erlang, Modifier, [Value, cerl:c_int(Amount)]),
            codegen_set(Element, Changed, In)
        )
    ).

%% Execution and libraries.

codegen_execute(In) ->
    State = cerl:c_var('State'),
    Final = cerl:c_var('Final'),

    Element = cerl:c_var('Element'),
    In = cerl:c_var('In'),

    Program = cerl:c_var('Program'),

    Step = cerl:c_fun([Element, In], cerl:c_apply(Element, [In])),

    cerl:c_let(
        [State],
        local_call(build_state, 1, [In]),
        cerl:c_let(
            [Final],
            cerl:c_let(
                [Program],
                call(erlang, element, [cerl:c_int(3), State]),
                call(lists, foldl, [Step, State, Program])
            ),
            call(erlang, element, [cerl:c_int(1), Final])
        )
    ).

codegen_brainfuck(Program, Mode, Flags) ->
    In = cerl:c_var('In'),
    Value = cerl:c_var('Value'),

    DebugFunctions = case proplists:lookup(debug, Flags) of
        {debug, _} -> [{cerl:c_fname(print, 1), cerl:c_fun([In], codegen_print(In))}];
        none       -> []
    end,

    DebugFunctions ++ [
        {cerl:c_fname(build_state, 1), cerl:c_fun([In], codegen_new_state(Program, Mode, In))},

        {cerl:c_fname(execute, 1), cerl:c_fun([In], codegen_execute(In))},

        {cerl:c_fname(get_ic, 1), cerl:c_fun([In], codegen_get(1, In))},
        {cerl:c_fname(get_ip, 1), cerl:c_fun([In], codegen_get(2, In))},
        {cerl:c_fname(get_mp, 1), cerl:c_fun([In], codegen_get(4, In))},

        {cerl:c_fname(get_memory_offset, 2), cerl:c_fun([In, Value], codegen_safe_array_get(In, Value))},
        {cerl:c_fname(get_program_offset, 2), cerl:c_fun([In, Value], codegen_safe_list_nth(In, Value))},

        {cerl:c_fname(inc_ic, 1), cerl:c_fun([In], codegen_inc(1, In))},

        {cerl:c_fname(inc_ip, 1), cerl:c_fun([In], codegen_inc(2, In))},
        {cerl:c_fname(set_ip, 2), cerl:c_fun([In, Value], codegen_set(2, Value, In))},

        {cerl:c_fname(inc_mp, 1), cerl:c_fun([In], codegen_inc(4, In))},
        {cerl:c_fname(dec_mp, 1), cerl:c_fun([In], codegen_dec(4, In))},

        %% TODO: Implementation in *Core Erlang*.

        {cerl:c_fname(inc, 1), cerl:c_fun([In], local_call(inc_ic, 1, [In]))},
        {cerl:c_fname(dec, 1), cerl:c_fun([In], local_call(inc_ic, 1, [In]))},

        {cerl:c_fname(left, 1), cerl:c_fun([In], local_call(inc_ic, 1, [In]))},
        {cerl:c_fname(right, 1), cerl:c_fun([In], local_call(inc_ic, 1, [In]))},

        {cerl:c_fname(in, 1), cerl:c_fun([In], local_call(inc_ic, 1, [In]))},
        {cerl:c_fname(out, 1), cerl:c_fun([In], local_call(inc_ic, 1, [In]))},

        {cerl:c_fname(start_loop, 1), cerl:c_fun([In], local_call(inc_ic, 1, [In]))},
        {cerl:c_fname(end_loop, 1), cerl:c_fun([In], local_call(inc_ic, 1, [In]))}
    ].

codegen_brainfork() ->
    In = cerl:c_var('In'),

    %% TODO: Implementation in *Core Erlang*.

    [{cerl:c_fname(fork, 1), cerl:c_fun([In], local_call(inc_ic, 1, [In]))}].

codegen_language_library(Program, Mode, ?HUMAN_NAME_BF, Flags) ->
    codegen_brainfuck(Program, Mode, Flags);

codegen_language_library(Program, Mode, ?HUMAN_NAME_BFO, Flags) ->
    codegen_brainfuck(Program, Mode, Flags) ++ codegen_brainfork().

%% Main flow.

codegen_main(Input) ->
    local_call(execute, 1, [Input]).

build(ParsedProgram, Input, Flags) ->
    Mode = case proplists:lookup(debug, Flags) of
        {debug, _} ->
            io:format("--PROCEEDED-TOKENS-------------------~n  ", []),
            lists:foreach(fun(Opcode) -> print_opcode(Opcode) end, ParsedProgram),
            io:format("~n", []),
            debug;

        none -> release
    end,

    {codegen_main(Input), Mode}.

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
