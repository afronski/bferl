-module(io_model).

-include_lib("proper/include/proper.hrl").

-include("../../include/interpreter_definitions.hrl").

-export([ prop_exhaustive_input_and_output_operations_from_tape_should_be_printed_in_reverse_order/0,
          prop_printing_from_memory_should_reflect_ASCII_representation/0 ]).

%% Properties.

prop_exhaustive_input_and_output_operations_from_tape_should_be_printed_in_reverse_order() ->
    ?FORALL(Input, string(),
            begin
                InputLength = length(Input),
                Program = string:copies(",>", InputLength) ++ string:copies("<.", InputLength),

                {ok, Pid} = bferl_io:start_link(),
                bferl_io:tape(Input),

                Tokens = bferl_tokenizer:from_string(Program),

                State = bferl_programming_language_logic:new(Tokens),
                StateWithIO = bferl_programming_language_logic:register_tape(State),

                bferl_programming_language_logic:run(StateWithIO),
                Output = bferl_io:get_output_tape(),

                exit(Pid, normal),

                ((lists:reverse(Input) =:= Output) and (InputLength =:= length(Output)))
            end).

prop_printing_from_memory_should_reflect_ASCII_representation() ->
    ?FORALL(Input, string(),
            begin
                InputLength = length(Input),
                Program = string:copies(".>", InputLength),

                {ok, Pid} = bferl_io:start_link(),
                bferl_io:tape(Input),

                Tokens = bferl_tokenizer:from_string(Program),
                State = bferl_programming_language_logic:new(Tokens),

                InputWithIndices = case Input of
                  [] -> [];
                  _  -> lists:zip(Input, lists:seq(0, length(Input) - 1))
                end,

                ModifiedState = lists:foldl(fun prepare_memory_cell_from_token/2, State, InputWithIndices),

                StateWithIO = bferl_programming_language_logic:register_tape(ModifiedState),

                bferl_programming_language_logic:run(StateWithIO),
                Output = bferl_io:get_output_tape(),

                exit(Pid, normal),

                ((Input =:= Output) and (InputLength =:= length(Output)))
            end).

%% Test Helpers.

prepare_memory_cell_from_token({Token, Index}, TemporalState) ->
    ModifiedMemory = array:set(Index, Token, TemporalState#interpreter.memory),
    TemporalState#interpreter{memory = ModifiedMemory}.
