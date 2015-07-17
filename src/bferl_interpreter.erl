-module(bferl_interpreter).

-include("../include/interpreter_definitions.hrl").

-export([ init/0, init/1,
          load/2, get_memory_cell/2,
          step/1, run/1 ]).

init() ->
    #interpreter_state{}.

init(Program) ->
    load(Program, #interpreter_state{}).

load(Program, State) when is_list(Program) ->
    State#interpreter_state{instructions = Program}.

get_memory_cell(CellIndex, State) ->
    array:get(CellIndex, State#interpreter_state.memory).

step(State) when State#interpreter_state.instructions =:= undefined ->
    no_program_loaded;

step(State) when State#interpreter_state.instructions_pointer < 0 ->
    end_of_program;

step(State) when State#interpreter_state.instructions_pointer >= length(State#interpreter_state.instructions) ->
    end_of_program;

step(State) ->
    IP = State#interpreter_state.instructions_pointer,
    Instruction = lists:nth(IP + 1, State#interpreter_state.instructions),
    TemporaryState = do(Instruction, State),
    TemporaryState#interpreter_state{instructions_pointer = IP + 1}.

run(State) ->
    IP = State#interpreter_state.instructions_pointer,
    Program = State#interpreter_state.instructions,
    SubProgram = lists:sublist(Program, IP + 1, length(Program)),
    lists:foldl(fun (_, PartialState) -> step(PartialState) end, State, SubProgram).

%% Opcodes
%% --
%% Brainfuck

do("+", InputState) ->
    CellIndex = InputState#interpreter_state.memory_pointer,
    Cell = get_memory_cell(CellIndex, InputState),
    InputState#interpreter_state{memory = array:set(CellIndex, Cell + 1, InputState#interpreter_state.memory)};

do("-", InputState) ->
    CellIndex = InputState#interpreter_state.memory_pointer,
    Cell = get_memory_cell(CellIndex, InputState),
    InputState#interpreter_state{memory = array:set(CellIndex, Cell - 1, InputState#interpreter_state.memory)};

do("<", InputState) ->
    CellIndex = InputState#interpreter_state.memory_pointer,
    InputState#interpreter_state{memory_pointer = max(CellIndex - 1, 0)};

do(">", InputState) ->
    CellIndex = InputState#interpreter_state.memory_pointer,
    InputState#interpreter_state{memory_pointer = min(CellIndex + 1, ?MEMORY_SIZE)};

do("[", InputState) ->
    InputState;

do("]", InputState) ->
    InputState;

do(",", InputState) ->
    InputState;

do(".", InputState) ->
    InputState.
