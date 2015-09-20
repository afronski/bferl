-module(bferl_repl).

-include("../include/interpreter_definitions.hrl").

-export([ start_loop/0 ]).

strip(String) -> re:replace(String, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).
extract_input(String) ->
    case re:run(String, "^\\[(.*?)\\]$", [global, {capture, [1], list}]) of
        nomatch               -> no_tape;
        {match, [[Tape] | _]} -> Tape
     end.

sanitize(eof)                           -> "";
sanitize([$? | _CommandName] = Command) -> strip(Command);
sanitize(BrainfuckCode)                 -> bferl_tokenizer:from_string(BrainfuckCode).

print(Type, Message) ->
    io:format("[~s]: ~s~n~n", [Type, Message]).

print_error(Message) -> print("ERR", Message).

pretty_print_state(State) ->
    InterpreterState = bferl_tools_interpreter:get_state(),

    SP = length(InterpreterState#interpreter.stack),
    MP = InterpreterState#interpreter.memory_pointer,

    MM2 = bferl_programming_language_logic:get_memory_cell(MP - 2, InterpreterState),
    MM1 = bferl_programming_language_logic:get_memory_cell(MP - 1, InterpreterState),
    M0  = bferl_programming_language_logic:get_memory_cell(MP, InterpreterState),
    MP1 = bferl_programming_language_logic:get_memory_cell(MP + 1, InterpreterState),
    MP2 = bferl_programming_language_logic:get_memory_cell(MP + 2, InterpreterState),

    Memory = [MM2, MM1, M0, MP1, MP2, SP],

    IC = InterpreterState#interpreter.instructions_counter,
    IP = InterpreterState#interpreter.instructions_pointer,

    CM2 = bferl_programming_language_logic:get_opcode(IP - 2, InterpreterState),
    CM1 = bferl_programming_language_logic:get_opcode(IP - 1, InterpreterState),
    C0  = bferl_programming_language_logic:get_opcode(IP, InterpreterState),
    CP1 = bferl_programming_language_logic:get_opcode(IP + 1, InterpreterState),
    CP2 = bferl_programming_language_logic:get_opcode(IP + 2, InterpreterState),

    Code = [CM2, CM1, C0, CP1, CP2, IC],

    TapeState = maps:get("tape", State),

    {Input, Output} = case TapeState of
        no_tape -> {"[No tape attached]", "[No tape attached]"};
        _       -> {bferl_io:get_input_tape(), bferl_io:get_output_tape()}
    end,

    io:format("--MEMORY---------------------------~n", []),
    io:format(".. ~3B ~3B (~3B) ~3B ~3B .. SP: ~3B~n", Memory),
    io:format("..  -2  -1    0   +1  +2 .. MP: ~3B~n", [MP]),
    io:format("--CODE-----------------------------~n", []),
    io:format(".. ~3s ~3s ( ~1s ) ~3s ~3s .. IC: ~3B~n", Code),
    io:format("..  -2  -1   0    +1  +2 .. IP: ~3B~n", [IP]),
    io:format("--TAPE-----------------------------~n", []),
    io:format("Input:  ~s~n", [Input]),
    io:format("Output: ~s~n", [Output]),
    io:format("-----------------------------------~n", []),

    {more, State}.

toggle_pretty_print(State) ->
    PrettyPrintState = maps:get("always_pretty_print_state", State),
    State#{"always_pretty_print_state" := not PrettyPrintState }.

clear(State) ->
    bferl_tools_interpreter:clear(),
    NewState = State#{"tape" := no_tape},
    {more, NewState}.

reset(State) ->
    bferl_tools_interpreter:reset(),
    {more, State}.

pretty_print_state_if_not_interactive(State) ->
    case maps:get("always_pretty_print_state", State) of
        true ->
            {more, State};

        _ ->
            pretty_print_state(State)
    end.

attach_tape(Input, State) ->
    Result = extract_input(Input),
    NewState = State#{"tape" := Result},

    case Result of
        no_tape ->
            {{no_input_for_tape, Input}, NewState};

        Tape ->
            bferl_io:tape(Tape),
            bferl_tools_interpreter:tape_attached(),
            {more, NewState}
    end.

perform_repl_command("exit", State) -> {exit, State};
perform_repl_command("e", State) -> {exit, State};

perform_repl_command("interactive", State) -> {more, toggle_pretty_print(State)};
perform_repl_command("i", State) -> {more, toggle_pretty_print(State)};

perform_repl_command("clear", State) -> clear(State);
perform_repl_command("c", State) -> clear(State);

perform_repl_command("reset", State) -> reset(State);
perform_repl_command("r", State) -> reset(State);

perform_repl_command("state", State) -> pretty_print_state_if_not_interactive(State);
perform_repl_command("s", State) -> pretty_print_state_if_not_interactive(State);

perform_repl_command("tape" ++ Rest, State) -> attach_tape(Rest, State);
perform_repl_command("t" ++ Rest, State) -> attach_tape(Rest, State);

perform_repl_command(Command, State) -> {{unknown_repl_command, Command}, State}.

dispatch([$? | CommandName], State) ->
    {_, NewState} = Result = perform_repl_command(CommandName, State),

    case maps:get("always_pretty_print_state", NewState) of
        true ->
            _ = pretty_print_state(NewState),
            Result;

        _ ->
            Result
    end;

dispatch([], State) ->
    {continue, State};

dispatch(BrainfuckCode, State) ->
    bferl_tools_interpreter:evaluate_code(BrainfuckCode),

    case maps:get("always_pretty_print_state", State) of
        true -> pretty_print_state(State);
        _    -> {more, State}
    end.

step(State) ->
    Line = io:get_line(?BRAINFUCK_INTERPRETER_PROMPT),
    {Status, NewState} = dispatch(sanitize(Line), State),

    Result = case Status of
        {unknown_repl_command, Input} ->
            print_error(io_lib:format("Unknown REPL command: '~s'.", [Input])),
            continue;

        {no_input_for_tape, Input} ->
            print_error(io_lib:format("You did not passed tape content with command argument: '~s'.", [Input])),
            continue;

        exit ->
            exit_repl;

        _  ->
            continue
    end,

    if
        Result =:= exit_repl -> done;
        true                 -> step(NewState)
    end.

start_loop() ->
    step(#{"always_pretty_print_state" => false, "tape" => no_tape}).
