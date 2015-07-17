-module(brainfuck_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/interpreter_definitions.hrl").

-export([ all/0 ]).
-export([ increment_value_in_the_cell/1, decrement_value_in_the_cell/1,
          moving_pointer_left/1, moving_pointer_right/1,
          edge_cases_for_moving_pointer/1,
          testing_loop/1,
          testing_input_and_output/1,
          testing_hello_world/1 ]).

all() ->
    [ increment_value_in_the_cell, decrement_value_in_the_cell,
      moving_pointer_left, moving_pointer_right,
      edge_cases_for_moving_pointer,
      testing_loop,
      testing_input_and_output,
      testing_hello_world ].

increment_value_in_the_cell(_Context) ->
    State = bferl_interpreter:init(["+"]),
    Output = bferl_interpreter:run(State),

    ?assertEqual(1, bferl_interpreter:get_memory_cell(0, Output)).

decrement_value_in_the_cell(_Context) ->
    State = bferl_interpreter:init(["-"]),
    Output = bferl_interpreter:run(State),

    ?assertEqual(-1, bferl_interpreter:get_memory_cell(0, Output)).

moving_pointer_left(_Context) ->
    State = bferl_interpreter:init(["<"]),
    NextState = State#interpreter{memory_pointer = 1},
    Output = bferl_interpreter:run(NextState),

    ?assertEqual(0, Output#interpreter.memory_pointer).

moving_pointer_right(_Context) ->
    State = bferl_interpreter:init([">"]),
    Output = bferl_interpreter:run(State),

    ?assertEqual(1, Output#interpreter.memory_pointer).

edge_cases_for_moving_pointer(_Context) ->
    InvalidLeft = bferl_interpreter:init(["<"]),
    LeftOutput = bferl_interpreter:run(InvalidLeft),

    ?assertEqual(0, LeftOutput#interpreter.memory_pointer),

    InvalidRight = bferl_interpreter:init([">"]),
    NextInvalidRight = InvalidRight#interpreter{memory_pointer = ?MEMORY_SIZE},
    RightOutput = bferl_interpreter:run(NextInvalidRight),

    ?assertEqual(?MEMORY_SIZE, RightOutput#interpreter.memory_pointer).

testing_loop(_Context) ->
    State = bferl_interpreter:init(["+", "+", "+", "[", "-", "]", "-"]),
    Output = bferl_interpreter:run(State),

    % Calculation: 3x '+', 3x '[-]', 1x '-' and +1 for `end_of_program`.
    ?assertEqual(3 + 3 * 3 + 1 + 1, Output#interpreter.instructions_counter),
    ?assertEqual(length(Output#interpreter.instructions) + 1, Output#interpreter.instructions_pointer),

    ?assertEqual(-1, bferl_interpreter:get_memory_cell(0, Output)).

testing_input_and_output(_Context) ->
    State = bferl_interpreter:init([",", "+", "."]),
    IoProcess = spawn_link(fun() -> io_handler("") end),

    StateWithIO = bferl_interpreter:register_io(IoProcess, State),
    Output = bferl_interpreter:run(StateWithIO),

    Tape = get_tape(IoProcess),

    ?assertEqual("AB", Tape),
    ?assertEqual(length(Output#interpreter.instructions), Output#interpreter.instructions_counter).

testing_hello_world(_Context) ->
    Program = bferl_tokenizer:from_file("../../../../test/assets/hello_world.bf"),
    State = bferl_interpreter:init(Program),

    IoProcess = spawn_link(fun() -> io_handler("") end),
    StateWithIO = bferl_interpreter:register_io(IoProcess, State),

    bferl_interpreter:run(StateWithIO),
    Tape = get_tape(IoProcess),

    ?assertEqual("Hello World!\n", Tape).

%% Test Helpers.

io_handler(Tape) ->
    NewTape = receive
        {put_char, Char} ->
            Tape ++ [Char];

        {get_char, Sender} ->
            Sender ! {received_char, $A},
            Tape ++ [$A];

        {get_tape, Sender} ->
            Sender ! {tape, Tape},
            Tape
    end,
    io_handler(NewTape).

get_tape(IoProcess) ->
    IoProcess ! {get_tape, self()},

    receive
        {tape, Tape} -> Tape
    after
        1000 -> ""
    end.
