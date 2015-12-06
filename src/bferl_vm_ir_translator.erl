-module(bferl_vm_ir_translator).

-export([ translate/1 ]).

-type jump() :: non_neg_integer().
-type index() :: pos_integer().
-type stack() :: list(index()).

-type tmp_jump_table() :: {array:array(index()), stack()}.
-type jump_table() :: list(jump()).

-type program() :: list(bferl_types:opcode()).

-type translation_result() :: {translation_suceeded, bferl_types:ir_program()} |
                              translation_error.

-spec build_jump_entry({index(), bferl_types:ir_opcode()}, tmp_jump_table()) -> tmp_jump_table().
build_jump_entry({N, {jmp, _}}, {Array, [H | StartLoops]}) ->
    ClosedUpdated = array:set(N - 1, H - 1, Array),
    OpenedUpdated = array:set(H - 1, N + 1, ClosedUpdated),
    {OpenedUpdated, StartLoops};
build_jump_entry({N, {jze, _}}, {Array, StartLoops})       -> {Array, [N | StartLoops]};
build_jump_entry(_, {Array, StartLoops})                   -> {Array, StartLoops}.

-spec build_jump_table(bferl_types:ir_program()) -> jump_table().
build_jump_table(ValidProgram) ->
    ProgramWithIndexes = lists:zip(lists:seq(1, length(ValidProgram)), ValidProgram),
    Array = array:new(length(ValidProgram), [fixed, {default, 0}]),

    {JumpArray, []} = lists:foldl(fun build_jump_entry/2, {Array, []}, ProgramWithIndexes),
    array:to_list(JumpArray).

-spec remapping(program()) -> bferl_types:ir_program().
remapping(ValidProgram) ->
    Opcodes = lists:flatten(lists:map(fun to_opcode/1, ValidProgram)),
    JumpTable = build_jump_table(Opcodes),
    lists:map(fun correct_jump/1, lists:zip(JumpTable, Opcodes)).

-spec translate(program()) -> translation_result().
translate(Program) ->
    case loop_stack_should_be_empty(Program) of
        false -> translation_error;
        _     -> {translation_suceeded, remapping(Program)}
    end.

-spec load_and_store(bferl_types:ir_opcode()) -> list(bferl_types:ir_opcode()).
load_and_store(Opcode) ->
    [ {load, ir0, r0}, Opcode, {store, r0, ir0} ].

-spec to_opcode(bferl_types:opcode()) -> list(bferl_types:ir_opcode()).
to_opcode("+") -> load_and_store({add, r0, 1});
to_opcode("-") -> load_and_store({sub, r0, 1});

to_opcode("<") -> [ {add, ir0, 1} ];
to_opcode(">") -> [ {sub, ir0, 1} ];

to_opcode(",") -> [ {call, in} ];
to_opcode(".") -> [ {call, out} ];

to_opcode("Y") -> [ {call, fork} ];

to_opcode("[") -> [ {load, ir0, r0}, {jze, 0} ];
to_opcode("]") -> [ {jmp, 0} ].

-spec correct_jump({jump(), bferl_types:ir_opcode()}) -> bferl_types:ir_opcode().
correct_jump({N, {jze, _}})  -> {jze, N};
correct_jump({N, {jmp, _}})  -> {jmp, N};
correct_jump({_, Opcode})    -> Opcode.

-spec loop_stack_should_be_empty(program()) -> boolean().
loop_stack_should_be_empty(Program) ->
    Stack = lists:foldl(fun check_token/2, [], Program),
    length(Stack) =:= 0.

-spec check_token(bferl_types:opcodes(), program()) -> program().
check_token("[", Stack)           -> [ "[" | Stack ];

check_token("]", [ "[" | Stack ]) -> Stack;
check_token("]", Stack)           -> [ "]" | Stack ];

check_token(_, Stack)             -> Stack.
