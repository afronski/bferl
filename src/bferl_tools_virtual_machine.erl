-module(bferl_tools_virtual_machine).
-behavior(gen_server).

-export([ start_link/0,
          clear/0,
          load/3,
          start/0,
          start/1 ]).

-export([ init/1,
          handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, empty, []).

clear() ->
    gen_server:call(?MODULE, clear).

load(Program, Type, Flags) ->
    gen_server:call(?MODULE, {load, Program, Type, Flags}).

start() ->
    start([]).

start(Flags) ->
    gen_server:call(?MODULE, {start, Flags}).

new_state() ->
    #{"program_loaded" => false}.

execute_when_loaded(false, State, _StartupFlags) ->
   {reply, program_not_loaded, State};

execute_when_loaded(true, State, StartupFlags) ->
   Type = maps:get("type", State),
   LoadingFlags = maps:get("loading_flags", State),

   NewState = State#{"startup_flags" => StartupFlags},

   %% TODO: IR is more readable and verbose than BF, and it contains more generic operations like add, sub, jmp.
   %% TODO: Optimizer, and IR compiler as separate processes.
   %% TODO: Staged compilation is just a composition of calls.
   %% TODO: Storing intermediate and optimized versions of programs in ETS, table owner is tools sup.

   {reply, {started, Type, {load, LoadingFlags}, {start, StartupFlags}}, NewState}.

init(empty) ->
    {ok, new_state()}.

handle_call(clear, _From, _State) ->
   {reply, cleared, new_state()};

handle_call({load, _Program, Type, Flags}, _From, State) ->
   NewState = State#{"type" => Type, "loading_flags" => Flags, "program_loaded" => true},
   {reply, {loaded, Type, Flags}, NewState};

handle_call({start, Flags}, _From, State) ->
   Loaded = maps:get("program_loaded", State),
   execute_when_loaded(Loaded, State, Flags).

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
