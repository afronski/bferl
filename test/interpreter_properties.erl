-module(interpreter_properties).

-include_lib("proper/include/proper.hrl").

-include("../include/interpreter_definitions.hrl").

-export([ prop_programs_without_loops_should_have_IC_and_IP_equal_to_program_length/0,
          prop_programs_with_proper_loops_should_execute_properly/0 ]).

-type memory_operation_token() :: dec | inc.
-type memory_movement_token() :: left | right.

-type pure_token() :: memory_operation_token() | memory_movement_token().
-type loop_token() :: while | end_while.
-type impure_token() :: in | out.

-type token() :: pure_token() | impure_token() | loop_token().

-type pure_program() :: [ pure_token() ].
-type program_with_loops() :: [ memory_movement_token() | loop_token() ].
-type program_with_io() :: [ pure_token() | loop_token() | impure_token() ].

-type program() :: pure_program() | program_with_loops() | program_with_io().

prop_programs_without_loops_should_have_IC_and_IP_equal_to_program_length() ->
    ?FORALL(Program, pure_program(),
            begin
                Input = bferl_interpreter:init(to_tokens(Program)),
                Output = bferl_interpreter:run(Input),

                (Output#interpreter.instructions_counter =:= length(Program)) and
                (Output#interpreter.instructions_pointer =:= length(Program) + 1)
            end).

prop_programs_with_proper_loops_should_execute_properly() ->
    ?FORALL(Program, program_with_valid_loops(),
            begin
                Input = bferl_interpreter:init(to_tokens(Program)),
                Output = bferl_interpreter:run(Input),

                Output#interpreter.instructions_pointer =:= length(Program) + 1
            end).

%% Test Helpers.

-spec verify_loop_stack(program_with_loops()) -> integer().
verify_loop_stack(Program) ->
    Stack = lists:foldl(fun check_token/2, [], Program),
    length(Stack) =:= 0.

-spec check_token(loop_token() | memory_movement_token(), list(loop_token())) -> list(loop_token()).
check_token(while, Stack)                 -> [ while | Stack ];
check_token(end_while, [ while | Stack ]) -> Stack;
check_token(end_while, Stack)             -> [ end_while | Stack ];
check_token(_, Stack)                     -> Stack.

-spec program_with_valid_loops() -> program_with_loops().
program_with_valid_loops() ->
    ?LET(Program,
         ?SUCHTHAT(TestedProgram,
                   list(oneof([while, left, right, end_while])),
                   verify_loop_stack(TestedProgram)),
         Program).

-spec to_tokens(program()) -> [ string() ].
to_tokens(List) -> lists:map(fun type_to_token/1, List).

-spec type_to_token(token()) -> string().
type_to_token(dec)       -> "-";
type_to_token(inc)       -> "+";
type_to_token(left)      -> "<";
type_to_token(right)     -> ">";

type_to_token(while)     -> "[";
type_to_token(end_while) -> "]";

type_to_token(out)       -> ".";
type_to_token(in)        -> ",".
