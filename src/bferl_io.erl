-module(bferl_io).

-export([ start_link/0,
          console/1, put_character_to_console/1, get_character_from_console/0,
          new_line_on_console/0,
          tape/1, put_character_to_tape/1, get_character_from_tape/0,
          new_line_on_tape/0,
          get_input_tape/0, get_output_tape/0 ]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

console(GroupLeader) ->
    gen_event:add_sup_handler(?MODULE, bferl_io_console, [ GroupLeader ]).

put_character_to_console(Char) ->
    gen_event:call(?MODULE, bferl_io_console, {put_character, Char}).

get_character_from_console() ->
    gen_event:call(?MODULE, bferl_io_console, get_character, infinity).

new_line_on_console() ->
    gen_event:call(?MODULE, bferl_io_console, new_line).

tape(Input) ->
    gen_event:add_sup_handler(?MODULE, bferl_io_tape, [ Input ]).

put_character_to_tape(Char) ->
    gen_event:call(?MODULE, bferl_io_tape, {put_character, Char}).

get_character_from_tape() ->
    gen_event:call(?MODULE, bferl_io_tape, get_character).

new_line_on_tape() ->
    gen_event:call(?MODULE, bferl_io_tape, new_line).

get_input_tape() ->
    gen_event:call(?MODULE, bferl_io_tape, get_input_tape).

get_output_tape() ->
    gen_event:call(?MODULE, bferl_io_tape, get_output_tape).
