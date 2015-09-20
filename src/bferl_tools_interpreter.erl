-module(bferl_tools_interpreter).
-behavior(gen_server).

-include("../include/interpreter_definitions.hrl").

-export([ start_link/0,
          get_state/0,
          clear/0, reset/0,
          tape_attached/0,
          evaluate_code/1 ]).

-export([ init/1,
          handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_state() ->
    gen_server:call(?MODULE, dump).

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

init([]) ->
    State = new_state(),
    {ok, State}.

handle_call(dump, _From, State) ->
    {reply, State, State};

handle_call(clear, _From, _State) ->
    NewState = new_state(),
    {reply, NewState, NewState};

handle_call(reset, _From, State) ->
    NewState = State#interpreter{instructions_counter = 0, instructions_pointer = 1, memory_pointer = 0},
    {reply, NewState, NewState};

handle_call(tape_attached, _From, State) ->
    NewState = bferl_programming_language_logic:register_tape(State),
    {reply, NewState, NewState};

handle_call({eval, Code}, _From, State) ->
    PreviousCode = case State#interpreter.instructions of
        undefined -> [];
        _         -> State#interpreter.instructions
    end,

    ModifiedState = State#interpreter{instructions = PreviousCode ++ Code},
    NewState = bferl_programming_language_logic:run(ModifiedState),

    {reply, NewState, NewState}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
