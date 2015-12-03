-module(bferl_vm_ir_translator).

-export([ translate/1 ]).

-export_type([ ir_opcode/0, ir_program/0 ]).

-type ir_opcode() :: [ {add, integer()} | {sub, integer()} |
                       {left, pos_integer()} | {right, pos_integer()} |
                       {jmp, pos_integer()} | {test, pos_integer()} |
                       in | out
                     ].
-type ir_program() :: list(ir_opcode()).

-type jump() :: non_neg_integer().
-type index() :: pos_integer().
-type stack() :: list(index()).
-type jump_table() :: list(jump()).

-type token() :: string().
-type program() :: list(token).

-type translation_result() :: {translation_suceeded, ir_program()} |
                              translation_error.

-spec build_jump_entry({index(), token()}, {array:array(index()), stack()}) -> {array:array(index()), stack()}.
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

-spec to_opcode(token()) -> ir_opcode().
to_opcode("+") -> {add, 1};
to_opcode("-") -> {sub, 1};

to_opcode("<") -> {left, 1};
to_opcode(">") -> {right, 1};

to_opcode(",") -> in;
to_opcode(".") -> out;

to_opcode("[") -> {test, 0};
to_opcode("]") -> {jmp, 0}.

-spec correct_jump({jump(), ir_opcode()}) -> ir_opcode().
correct_jump({N, {jmp, _}}) -> {jmp, N};
correct_jump({N, {test, _}}) -> {test, N};
correct_jump({_, Opcode}) -> Opcode.

-spec loop_stack_should_be_empty(program()) -> boolean().
loop_stack_should_be_empty(Program) ->
    Stack = lists:foldl(fun check_token/2, [], Program),
    length(Stack) =:= 0.

-spec check_token(token(), program()) -> program().
check_token("[", Stack)           -> [ "[" | Stack ];

check_token("]", [ "[" | Stack ]) -> Stack;
check_token("]", Stack)           -> [ "]" | Stack ];

check_token(_, Stack)             -> Stack.
