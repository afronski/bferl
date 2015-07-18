-module(interpreter_properties).

-include_lib("proper/include/proper.hrl").

prop_test() ->
    ?FORALL({X,Y}, {integer(),integer()}, begin X + Y =:= Y + X end).
