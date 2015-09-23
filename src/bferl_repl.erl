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

    DebugModeState = maps:get("debug_mode", State),

    DebugMode = case DebugModeState of
        true -> "D";
        _    -> "-"
    end,

    io:format("--MEMORY---------------------------~n", []),
    io:format(".. ~3B ~3B (~3B) ~3B ~3B .. SP: ~3B~n", Memory),
    io:format("..  -2  -1    0   +1  +2 .. MP: ~3B~n", [MP]),
    io:format("--CODE---------------------------~1s-~n", [DebugMode]),
    io:format(".. ~3s ~3s ( ~1s ) ~3s ~3s .. IC: ~3B~n", Code),
    io:format("..  -2  -1   0    +1  +2 .. IP: ~3B~n", [IP]),
    io:format("--TAPE-----------------------------~n", []),
    io:format("Input:  ~s~n", [Input]),
    io:format("Output: ~s~n", [Output]),
    io:format("-----------------------------------~n", []),

    {more, State}.

help(State) ->
    io:format("--bferl--------------------------------------------by-afronski--~n", []),
    io:format("~n* Available commands:                                       ~n~n", []),
    io:format("  ?h, ?help        - This message. List of available commands.  ~n", []),
    io:format("  ?e, ?exit        - Exit the REPL.                             ~n", []),
    io:format("  ?i, ?interactive - Debugging with step mode. Executing whole  ~n", []),
    io:format("                     input atomically, one element at a time    ~n", []),
    io:format("                     (toggle).                                  ~n", []),
    io:format("  ?a, ?autoprint   - Printing state of the REPL after each      ~n", []),
    io:format("                     evaluation or command (toggle).            ~n", []),
    io:format("  ?c, ?clear       - Clear whole REPL state.                    ~n", []),
    io:format("  ?r, ?reset       - Reset only pointers in the REPL state.     ~n", []),
    io:format("  ?s, ?state       - Immediately print state of the REPL.       ~n", []),
    io:format("  ?t[T], ?tape[T]  - Attach new input tape, with content        ~n", []),
    io:format("                     represented by 'T' sequence inside square  ~n", []),
    io:format("                     brackets.                                ~n~n", []),
    io:format("----------------------------------------------------------------~n", []),

    {more, State}.

toggle_debug_mode(State) ->
    bferl_tools_interpreter:debug_mode(),

    DebugModeState = maps:get("debug_mode", State),
    ModifiedState = case not DebugModeState of
        true -> State#{"prompt" := ?BRAINFUCK_INTERPRETER_PROMPT_DEBUG};
        _    -> State#{"prompt" := ?BRAINFUCK_INTERPRETER_PROMPT}
    end,

    {more, ModifiedState#{"debug_mode" := not DebugModeState}}.

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

perform_repl_command("help", State) -> help(State);
perform_repl_command("h", State) -> help(State);

perform_repl_command("exit", State) -> {exit, State};
perform_repl_command("e", State) -> {exit, State};

perform_repl_command("interactive", State) -> toggle_debug_mode(State);
perform_repl_command("i", State) -> toggle_debug_mode(State);

perform_repl_command("autoprint", State) -> {more, toggle_pretty_print(State)};
perform_repl_command("a", State) -> {more, toggle_pretty_print(State)};

perform_repl_command("clear", State) -> clear(State);
perform_repl_command("c", State) -> clear(State);

perform_repl_command("reset", State) -> reset(State);
perform_repl_command("r", State) -> reset(State);

perform_repl_command("state", State) -> pretty_print_state_if_not_interactive(State);
perform_repl_command("s", State) -> pretty_print_state_if_not_interactive(State);

perform_repl_command("tape" ++ Rest, State) -> attach_tape(Rest, State);
perform_repl_command("t" ++ Rest, State) -> attach_tape(Rest, State);

perform_repl_command(Command, State) -> {{unknown_repl_command, Command}, State}.

evaluate_code_internal_call(Code) ->
    InterpreterStateBeforeEvaluation = bferl_tools_interpreter:get_state(),

    try
        bferl_tools_interpreter:evaluate_code(Code)
    catch
        exit:{timeout, _} -> {timeout, InterpreterStateBeforeEvaluation}
    end.

next_step(true, State) ->
    bferl_tools_interpreter:evaluate_code([]),

    case maps:get("always_pretty_print_state", State) of
        true -> pretty_print_state(State), ok;
        _    -> ok
    end;

next_step(_, _) ->
    nop.

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
    next_step(maps:get("debug_mode", State), State),
    {continue, State};

dispatch(BrainfuckCode, State) ->
    {EvaluationStatus, Response} = evaluate_code_internal_call(BrainfuckCode),

    {Prompt, PromptMore} = case maps:get("debug_mode", State) of
        true -> {?BRAINFUCK_INTERPRETER_PROMPT_DEBUG, ?BRAINFUCK_INTERPRETER_PROMPT_DEBUG_MORE};
        _    -> {?BRAINFUCK_INTERPRETER_PROMPT, ?BRAINFUCK_INTERPRETER_PROMPT_MORE}
    end,

    {ReplStatus, ModifiedState} = case EvaluationStatus of
        more_tokens -> {continue, State#{"prompt" := PromptMore}};
        valid       -> {continue, State#{"prompt" := Prompt}};
        timeout     -> {timeout, State};
        _           -> {error, State}
    end,

    case ReplStatus of
        continue ->
            case maps:get("always_pretty_print_state", ModifiedState) of
                true -> pretty_print_state(ModifiedState);
                _    -> {more, ModifiedState}
            end;

        timeout ->
            {{timeout, Response}, ModifiedState};

        error ->
            {{syntax_error, Response}, ModifiedState}
    end.

step(State) ->
    Line = io:get_line(maps:get("prompt", State)),
    {Status, NewState} = dispatch(sanitize(Line), State),

    Result = case Status of
        {unknown_repl_command, Input} ->
            print_error(io_lib:format("Unknown REPL command: '~s'.", [Input])),
            continue;

        {no_input_for_tape, Input} ->
            print_error(io_lib:format("No content passed in tape command argument: '~s'.", [Input])),
            continue;

        {syntax_error, Code} ->
            print_error(io_lib:format("Last instructions contained a syntax error: '~s'.", [Code])),
            continue;

        {timeout, InterpreterStateToRestore} ->
            print_error(io_lib:format("Last instructions caused a long running loop. State restored.", [])),
            bferl_tools_interpreter:restore(InterpreterStateToRestore),
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
    step(#{"always_pretty_print_state" => false,
           "debug_mode" => false,
           "prompt" => ?BRAINFUCK_INTERPRETER_PROMPT,
           "tape" => no_tape}).
