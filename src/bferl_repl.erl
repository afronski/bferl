-module(bferl_repl).

-include("../include/interpreter_definitions.hrl").

-export([ start_loop/0 ]).

strip(String) -> re:replace(String, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).
extract_input(String) ->
    case re:run(String, "^\\[(.*?)\\]$", [global, {capture, [1], list}]) of
        nomatch             -> no_tape;
        {match, [Tape | _]} -> Tape
     end.

sanitize(eof)                           -> "";
sanitize([$? | _CommandName] = Command) -> strip(Command);
sanitize(BrainfuckCode)                 -> bferl_tokenizer:from_string(BrainfuckCode).

print(Type, Message) ->
    io:format("[~s]: ~s~n~n", [Type, Message]).

print_error(Message) -> print("ERR", Message).

pretty_print_state(State) ->
    %% TODO: Pretty print and ASCII art.
    io:format("PrettyPrint!", []),
    {more, State}.

toggle_pretty_print(State) ->
    PrettyPrintState = maps:get("always_pretty_print_state", State),
    State#{"always_pretty_print_state" := not PrettyPrintState }.

pretty_print_state_if_not_interactive(State) ->
    case maps:get("always_pretty_print_state", State) of
        true -> {more, State};
        _    -> pretty_print_state(State)
    end.

attach_tape(Input, State) ->
    Result = extract_input(Input),
    NewState = State#{ "tape" := Result },

    case Result of
        no_tape ->
            {{no_input_for_tape, Input}, NewState};

        Tape ->
            bferl_io:tape(Tape),
            {more, NewState}
    end.

perform_repl_command("exit", State) -> {exit, State};
perform_repl_command("e", State) -> {exit, State};

perform_repl_command("interactive", State) -> {more, toggle_pretty_print(State)};
perform_repl_command("i", State) -> {more, toggle_pretty_print(State)};

perform_repl_command("state", State) -> pretty_print_state_if_not_interactive(State);
perform_repl_command("s", State) -> pretty_print_state_if_not_interactive(State);

perform_repl_command("tape" ++ Rest, State) -> attach_tape(Rest, State);
perform_repl_command("t" ++ Rest, State) -> attach_tape(Rest, State);

perform_repl_command(Command, State) -> {{unknown_repl_command, Command}, State}.

dispatch([$? | CommandName], State) ->
    Result = perform_repl_command(CommandName, State),

    case maps:get("always_pretty_print_state", State) of
        true -> pretty_print_state(State), Result;
        _    -> Result
    end;

dispatch(_BrainfuckCode, State) ->
    %% TODO: Send code to interpreter.

    case maps:get("always_pretty_print_state", State) of
        true -> pretty_print_state(State);
        _    -> {more, State}
    end.

step(State) ->
    Line = io:get_line(?BRAINFUCK_INTERPRETER_PROMPT),
    {Status, NewState} = dispatch(sanitize(Line), State),

    Result = case Status of
        {unknown_repl_command, Input} ->
            print_error(io_lib:format("Unknown REPL command: '~s'", [Input])),
            continue;

        {no_input_for_tape, Input} ->
            print_error(io_lib:format("You did not passed tape content with command argument: '~s'", [Input])),
            continue;

        {no_command, Input} ->
            print_error(io_lib:format("No command, neither Brainfuck code delivered in '~s'", [Input])),
            continue;

        exit -> exit_repl;
        _  -> continue
    end,

    if
        Result =:= exit_repl -> done;
        true                 -> step(NewState)
    end.

start_loop() ->
    step(#{"always_pretty_print_state" => false, "tape" => no_tape}).
