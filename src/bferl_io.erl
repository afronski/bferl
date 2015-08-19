-module(bferl_io).

-export([ start_link/0,
          put_character/1,
          console/1, get_character_from_console/0,
          tape/1, get_character_from_tape/0, get_output_tape/0 ]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

put_character(Char) ->
    gen_event:notify(?MODULE, {put_character, Char}).

console(GroupLeader) ->
    gen_event:add_sup_handler(?MODULE, bferl_io_console, [ GroupLeader ]).

get_character_from_console() ->
    get_character(bferl_io_console).

tape(Input) ->
    gen_event:add_sup_handler(?MODULE, bferl_io_tape, [ Input ]).

get_character_from_tape() ->
    get_character(bferl_io_tape).

get_output_tape() ->
    gen_event:call(?MODULE, bferl_io_tape, get_tape).

%% Private immplementation.

get_character(From) ->
    gen_event:call(?MODULE, From, get_character).
