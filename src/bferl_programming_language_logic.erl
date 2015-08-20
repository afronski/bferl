-module(bferl_programming_language_logic).

-include("../include/interpreter_definitions.hrl").

-export([ new/0, new/1, load/2,
          register_tape/1, register_console/1, register_io/3,
          get_memory_cell/2,
          step/1, run/1 ]).

new() ->
    #interpreter{}.

new(Program) ->
    load(Program, #interpreter{}).

load(Program, State) when is_list(Program) ->
    State#interpreter{instructions = Program}.

register_tape(State) ->
    register_io(fun bferl_io:get_character_from_tape/0, fun bferl_io:put_character/1, State).

register_console(State) ->
    register_io(fun bferl_io:get_character_from_console/0, fun bferl_io:put_character/1, State).

register_io(In, Out, State) when is_function(In) andalso is_function(Out) ->
    State#interpreter{io = {In, Out}}.

get_memory_cell(CellIndex, State) ->
    array:get(CellIndex, State#interpreter.memory).

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

run(State) ->
    NewState = step(State),
    case NewState of
        end_of_program -> State;
        _              -> run(NewState)
    end.

%% Opcodes
%% --
%% Brainfuck

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

    {_, Out} = InputState#interpreter.io,
    Out(Value),

    IP = InputState#interpreter.instructions_pointer,

    InputState#interpreter{instructions_pointer = IP + 1};

do(",", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    {In, _} = InputState#interpreter.io,

    Value = In(),
    IP = InputState#interpreter.instructions_pointer,

    InputState#interpreter{memory = array:set(CellIndex, Value, InputState#interpreter.memory),
                           instructions_pointer = IP + 1}.

%% Opcodes Helper.

find_corresponding_closing_bracket(IP, Program) when is_integer(IP), is_list(Program) ->
    SubProgram = lists:sublist(Program, IP + 1, length(Program)),
    SubProgramWithIndexes = lists:zip(SubProgram, lists:seq(1, length(SubProgram))),

    {NewIP, 0} = lists:foldl(fun handle_brackets/2, {-1, 0}, SubProgramWithIndexes),
    NewIP.

handle_brackets({"]", Position}, {OldPosition, 0}) when OldPosition =:= -1 -> {Position, 0};
handle_brackets({"]", _}, {Position, N}) when Position =:= -1 -> {Position, N - 1};
handle_brackets({"[", _}, {Position, N}) when Position =:= -1 -> {Position, N + 1};
handle_brackets(_, Result) -> Result.