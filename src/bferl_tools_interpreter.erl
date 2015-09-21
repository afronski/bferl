-module(bferl_tools_interpreter).
-behavior(gen_server).

-include("../include/interpreter_definitions.hrl").

-export([ start_link/0,
          get_state/0,
          restore/1,
          clear/0, reset/0,
          tape_attached/0,
          evaluate_code/1,
          validate/1 ]).

-export([ init/1,
          handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_state() ->
    gen_server:call(?MODULE, dump).

restore(InterpreterState) ->
    gen_server:call(?MODULE, {restore, InterpreterState}).

clear() ->
    gen_server:call(?MODULE, clear).

reset() ->
    gen_server:call(?MODULE, reset).

tape_attached() ->
    gen_server:call(?MODULE, tape_attached).

evaluate_code(Code) ->
    gen_server:call(?MODULE, {eval, Code}).

new_state() ->
    bferl_programming_language_logic:register_console(bferl_programming_language_logic:new()).

validation_rules("[", [H | T], X) -> validation_rules(H, T, X + 1);
validation_rules("[", [], _) -> more_tokens;

validation_rules("]", [H | T], X) when X > 0 -> validation_rules(H, T, X - 1);
validation_rules("]", [], X) when X =:= 1 -> valid;
validation_rules("]", [], X) when X > 1 -> not_valid;
validation_rules("]", _, X) when X =:= 0 -> not_valid;

validation_rules(_, [H | T], X) -> validation_rules(H, T, X);

validation_rules(_, [], X) when X > 0 -> more_tokens;
validation_rules(_, [], X) when X =:= 0 -> valid.

validate([]) ->
    valid;

validate([Head | Tail]) ->
    validation_rules(Head, Tail, 0).

init([]) ->
    State = #{"interpreter" => new_state()},
    {ok, State};

init(StartInterpreterState) ->
    State = #{"interpreter" => StartInterpreterState},
    {ok, State}.

handle_call({restore, InterpreterState}, _From, State) ->
    {reply, restored, State#{"interpreter" := InterpreterState}};

handle_call(dump, _From, State) ->
    {reply, maps:get("interpreter", State), State};

handle_call(clear, _From, State) ->
    Cleared = State#{"interpreter" := new_state()},
    {reply, maps:get("interpreter", Cleared), Cleared};

handle_call(reset, _From, State) ->
    InterpreterState = maps:get("interpreter", State),
    Reset = InterpreterState#interpreter{instructions_counter = 0, instructions_pointer = 1, memory_pointer = 0},
    {reply, Reset, State#{"interpreter" := Reset}};

handle_call(tape_attached, _From, State) ->
    NewState = bferl_programming_language_logic:register_tape(maps:get("interpreter", State)),
    {reply, NewState, State#{"interpreter" := NewState}};

handle_call({eval, Code}, _From, State) ->
    Interpreter = maps:get("interpreter", State),

    PreviousCode = case Interpreter#interpreter.instructions of
        undefined -> [];
        _         -> Interpreter#interpreter.instructions
    end,

    case validate(PreviousCode ++ Code) of
        not_valid ->
            {reply, {not_valid, Code}, State};

        more_tokens ->
            ModifiedState = Interpreter#interpreter{instructions = PreviousCode ++ Code},
            {reply, {more_tokens, ModifiedState}, State#{"interpreter" := ModifiedState}};

        valid ->
            Before = Interpreter#interpreter{instructions = PreviousCode ++ Code},

            MessageRef = make_ref(),
            Parent = self(),

            {Pid, MonitorRef} = spawn_monitor(fun() ->
                Result = bferl_programming_language_logic:run(Before),
                Parent ! {evaluated, MessageRef, Result}
            end),

            receive
                {evaluated, MessageRef, After} ->
                    {reply, {valid, After}, State#{"interpreter" := After}};

                {'DOWN', MonitorRef, Pid, _, _} ->
                    {noreply, State}
            after
                5000 ->
                    exit(Pid, kill),
                    {noreply, State}
            end
    end.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
