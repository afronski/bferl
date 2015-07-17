-module(bferl_interpreter).

-include("../include/interpreter_definitions.hrl").

-export([ init/0, init/1,
          load/2, register_io/2,
          get_memory_cell/2,
          step/1, run/1 ]).

init() ->
    #interpreter{}.

init(Program) ->
    load(Program, #interpreter{}).

load(Program, State) when is_list(Program) ->
    State#interpreter{instructions = Program}.

register_io(IoProcess, State) when is_pid(IoProcess) ->
    State#interpreter{io = IoProcess}.

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

            NewIP = case find_first_closing_bracket(IP, InputState#interpreter.instructions) of
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

    IoProcess = InputState#interpreter.io,
    IoProcess ! {put_char, Value},

    IP = InputState#interpreter.instructions_pointer,

    InputState#interpreter{instructions_pointer = IP + 1};

do(",", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    IoProcess = InputState#interpreter.io,

    IoProcess ! {get_char, self()},
    Value = receive
        {received_char, Char} -> Char
    end,

    IP = InputState#interpreter.instructions_pointer,

    InputState#interpreter{memory = array:set(CellIndex, Value, InputState#interpreter.memory),
                           instructions_pointer = IP + 1}.

%% Opcodes Helper.

find_first_closing_bracket(IP, Program) when is_integer(IP), is_list(Program) ->
    SubProgram = lists:sublist(Program, IP + 1, length(Program)),
    Code = string:join(SubProgram, ""),
    string:chr(Code, $]).
