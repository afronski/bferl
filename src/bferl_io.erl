-module(bferl_io).

-export([ start_link/0, add/1,
          put_character/1, get_character/0 ]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add(Handler) ->
    gen_event:add_handler(Handler, ?MODULE, []).

put_character(Char) ->
    gen_event:notify(?MODULE, {put_character, Char}).

get_character() ->
    gen_event:call(?MODULE, get_character).
