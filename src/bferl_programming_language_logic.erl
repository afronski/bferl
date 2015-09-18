-module(bferl_programming_language_logic).

-include("../include/interpreter_definitions.hrl").

-export([ new/0, new/1, load/2,
          register_tape/1, register_console/1, register_io/4,
          get_memory_cell/2,
          step/1, run/1 ]).

-spec new() -> bferl_types:interpreter_state().
new() ->
    #interpreter{}.

-spec new(bferl_types:program()) -> bferl_types:interpreter_state().
new(Program) ->
    load(Program, #interpreter{}).

-spec load(bferl_types:program(), bferl_types:interpreter_state()) -> bferl_types:interpreter_state().
load(Program, State) when is_list(Program) ->
    State#interpreter{instructions = Program}.

-spec register_tape(bferl_types:interpreter_state()) -> bferl_types:interpreter_state().
register_tape(State) ->
    register_io(fun bferl_io:get_character_from_tape/0, fun bferl_io:put_character/1, fun bferl_io:new_line/0, State).

-spec register_console(bferl_types:interpreter_state()) -> bferl_types:interpreter_state().
register_console(State) ->
    register_io(fun bferl_io:get_character_from_console/0, fun bferl_io:put_character/1, fun bferl_io:new_line/0, State).

-spec register_io(fun(), fun(), fun(), bferl_types:interpreter_state()) -> bferl_types:interpreter_state().
register_io(In, Out, NewLine, State) when is_function(In), is_function(Out), is_function(NewLine) ->
    State#interpreter{io = {In, Out, NewLine}}.

-spec get_memory_cell(non_neg_integer(), bferl_types:interpreter_state()) -> integer().
get_memory_cell(CellIndex, State) ->
    array:get(CellIndex, State#interpreter.memory).

-spec step(bferl_types:interpreter_state()) -> no_program_loaded | end_of_program | bferl_types:interpreter_state().
step(State) when State#interpreter.instructions =:= undefined ->
    no_program_loaded;

step(State) when State#interpreter.instructions_pointer =< 0 ->
    end_of_program;

step(State) when State#interpreter.instructions_pointer > length(State#interpreter.instructions) ->
    end_of_program;

step(State) ->
    IP = State#interpreter.instructions_pointer,
    Instruction = lists:nth(IP, State#interpreter.instructions),

    TemporaryState = do(Instruction, State),

    IC = TemporaryState#interpreter.instructions_counter,
    TemporaryState#interpreter{instructions_counter = IC + 1}.

-spec run(bferl_types:interpreter_state()) -> bferl_types:interpreter_state().
run(State) ->
    NewState = step(State),
    case NewState of
        end_of_program ->
            {_, _, NewLine} = State#interpreter.io,

            case is_function(NewLine) of
                true -> NewLine(), State;
                false -> State
            end;

        _ ->
            run(NewState)
    end.

%% Opcodes
%% --
%% Brainfuck

-spec do(bferl_types:opcodes(), bferl_types:interpreter_state()) -> bferl_types:interpreter_state().
do("+", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    Cell = get_memory_cell(CellIndex, InputState),

    IP = InputState#interpreter.instructions_pointer,

    InputState#interpreter{memory = array:set(CellIndex, Cell + 1, InputState#interpreter.memory),
                           instructions_pointer = IP + 1};

do("-", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    Cell = get_memory_cell(CellIndex, InputState),

    IP = InputState#interpreter.instructions_pointer,

    InputState#interpreter{memory = array:set(CellIndex, Cell - 1, InputState#interpreter.memory),
                           instructions_pointer = IP + 1};

do("<", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    IP = InputState#interpreter.instructions_pointer,

    InputState#interpreter{memory_pointer = max(CellIndex - 1, 0),
                           instructions_pointer = IP + 1};

do(">", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    IP = InputState#interpreter.instructions_pointer,

    InputState#interpreter{memory_pointer = min(CellIndex + 1, ?MEMORY_SIZE),
                           instructions_pointer = IP + 1};

do("[", InputState) ->
    IP = InputState#interpreter.instructions_pointer,

    CellIndex = InputState#interpreter.memory_pointer,
    Result = case get_memory_cell(CellIndex, InputState) of
        0 ->
            NewStack = case InputState#interpreter.stack of
                []         -> [];
                [_ | Tail] -> Tail
            end,

            NewIP = case find_corresponding_closing_bracket(IP, InputState#interpreter.instructions) of
                0 -> InputState#interpreter.instructions_pointer + 1;
                Position -> Position + IP + 1
            end,

            InputState#interpreter{stack = NewStack, instructions_pointer = NewIP};

        _ ->
            NewStack = case InputState#interpreter.stack of
                [H | _] = OldStack when H =:= IP -> OldStack;
                _                                -> [IP | InputState#interpreter.stack]
            end,

            InputState#interpreter{stack = NewStack, instructions_pointer = IP + 1}
    end,

    Result;

do("]", InputState) when length(InputState#interpreter.stack) =< 0 -> InputState;
do("]", InputState) when length(InputState#interpreter.stack)  > 0 ->
    [NewIP | _] = InputState#interpreter.stack,
    InputState#interpreter{instructions_pointer = NewIP};

do(".", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    Value = get_memory_cell(CellIndex, InputState),

    {_, Out, _} = InputState#interpreter.io,
    Out(Value),

    IP = InputState#interpreter.instructions_pointer,

    InputState#interpreter{instructions_pointer = IP + 1};

do(",", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    {In, _, _} = InputState#interpreter.io,

    Value = In(),
    IP = InputState#interpreter.instructions_pointer,

    InputState#interpreter{memory = array:set(CellIndex, Value, InputState#interpreter.memory),
                           instructions_pointer = IP + 1}.

%% Opcodes Helper.

-spec find_corresponding_closing_bracket(pos_integer(), bferl_types:program()) -> non_neg_integer() | -1.
find_corresponding_closing_bracket(IP, Program) when is_integer(IP), is_list(Program) ->
    SubProgram = lists:sublist(Program, IP + 1, length(Program)),
    SubProgramWithIndexes = lists:zip(SubProgram, lists:seq(1, length(SubProgram))),

    {NewIP, 0} = lists:foldl(fun handle_brackets/2, {-1, 0}, SubProgramWithIndexes),
    NewIP.

-type loop_ending() :: {non_neg_integer() | -1, non_neg_integer()}.
-spec handle_brackets({string(), non_neg_integer()}, loop_ending()) -> loop_ending().
handle_brackets({"]", Position}, {OldPosition, 0}) when OldPosition =:= -1 -> {Position, 0};
handle_brackets({"]", _}, {Position, N}) when Position =:= -1 -> {Position, N - 1};
handle_brackets({"[", _}, {Position, N}) when Position =:= -1 -> {Position, N + 1};
handle_brackets(_, Result) -> Result.
