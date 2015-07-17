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

step(State) when State#interpreter.instructions_pointer < 0 ->
    end_of_program;

step(State) when State#interpreter.instructions_pointer >= length(State#interpreter.instructions) ->
    end_of_program;

step(State) ->
    IP = State#interpreter.instructions_pointer,
    Instruction = lists:nth(IP + 1, State#interpreter.instructions),
    TemporaryState = do(Instruction, State),
    TemporaryState#interpreter{instructions_pointer = IP + 1}.

run(State) ->
    IP = State#interpreter.instructions_pointer,
    Program = State#interpreter.instructions,
    SubProgram = lists:sublist(Program, IP + 1, length(Program)),
    lists:foldl(fun (_, PartialState) -> step(PartialState) end, State, SubProgram).

%% Opcodes
%% --
%% Brainfuck

do("+", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    Cell = get_memory_cell(CellIndex, InputState),
    InputState#interpreter{memory = array:set(CellIndex, Cell + 1, InputState#interpreter.memory)};

do("-", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    Cell = get_memory_cell(CellIndex, InputState),
    InputState#interpreter{memory = array:set(CellIndex, Cell - 1, InputState#interpreter.memory)};

do("<", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    InputState#interpreter{memory_pointer = max(CellIndex - 1, 0)};

do(">", InputState) ->
    CellIndex = InputState#interpreter.memory_pointer,
    InputState#interpreter{memory_pointer = min(CellIndex + 1, ?MEMORY_SIZE)};

do("[", InputState) ->
    InputState;

do("]", InputState) ->
    InputState;

do(".", InputState) ->
    %% TODO: How to handle output?
    InputState;

do(",", InputState) ->
    %% TODO: How to handle input?
    InputState.
