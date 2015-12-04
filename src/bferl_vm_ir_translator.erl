-module(bferl_vm_ir_translator).

-export([ translate/1 ]).

-type ir_opcode() :: {add, ir0 | ir, -1 | 1}  |
                     {test, r0, 0}            |
                     {jmp, 0}                 |
                     {call, 1 | 2 | 3}.
-type ir_program() :: list(ir_opcode()).

-type jump() :: non_neg_integer().
-type index() :: pos_integer().
-type stack() :: list(index()).

-type tmp_jump_table() :: {array:array(index()), stack()}.
-type jump_table() :: list(jump()).

-type program() :: list(bferl_types:opcode()).

-type translation_result() :: {translation_suceeded, ir_program()} |
                              translation_error.

-spec build_jump_entry({index(), bferl_types:opcode()}, tmp_jump_table()) -> tmp_jump_table().
build_jump_entry({N, "]"}, {Array, [H | StartLoops]}) ->
    ClosedUpdated = array:set(N - 1, H, Array),
    OpenedUpdated = array:set(H - 1, N, ClosedUpdated),
    {OpenedUpdated, StartLoops};
build_jump_entry({N, "["}, {Array, StartLoops})       -> {Array, [N | StartLoops]};
build_jump_entry(_, {Array, StartLoops})              -> {Array, StartLoops}.

-spec build_jump_table(program()) -> jump_table().
build_jump_table(ValidProgram) ->
    ProgramWithIndexes = lists:zip(lists:seq(1, length(ValidProgram)), ValidProgram),
    Array = array:new(length(ValidProgram), [fixed, {default, 0}]),

    {JumpArray, []} = lists:foldl(fun build_jump_entry/2, {Array, []}, ProgramWithIndexes),

    array:to_list(JumpArray).

-spec remapping(program()) -> ir_program().
remapping(ValidProgram) ->
    Opcodes = lists:map(fun to_opcode/1, ValidProgram),
    JumpTable = build_jump_table(ValidProgram),
    lists:map(fun correct_jump/1, lists:zip(JumpTable, Opcodes)).

-spec translate(program()) -> translation_result().
translate(Program) ->
    case loop_stack_should_be_empty(Program) of
        false -> translation_error;
        _     -> {translation_suceeded, remapping(Program)}
    end.

-spec to_opcode(bferl_types:opcode()) -> ir_opcode().
to_opcode("+") -> {add, r0, 1};
to_opcode("-") -> {add, r0, -1};

to_opcode("<") -> {add, ir0, 1};
to_opcode(">") -> {add, ir0, -1};

to_opcode(",") -> {call, 1};
to_opcode(".") -> {call, 2};

to_opcode("Y") -> {call, 3};

to_opcode("[") -> {test, r0, 0};
to_opcode("]") -> {jmp, 0}.

-spec correct_jump({jump(), ir_opcode()}) -> ir_opcode().
correct_jump({N, {jmp, _}}) -> {jmp, N};
correct_jump({N, {test, r0, _}}) -> {test, r0, N};
correct_jump({_, Opcode}) -> Opcode.

-spec loop_stack_should_be_empty(program()) -> boolean().
loop_stack_should_be_empty(Program) ->
    Stack = lists:foldl(fun check_token/2, [], Program),
    length(Stack) =:= 0.

-spec check_token(bferl_types:opcodes(), program()) -> program().
check_token("[", Stack)           -> [ "[" | Stack ];

check_token("]", [ "[" | Stack ]) -> Stack;
check_token("]", Stack)           -> [ "]" | Stack ];

check_token(_, Stack)             -> Stack.
