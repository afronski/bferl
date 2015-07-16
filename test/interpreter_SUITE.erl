-module(interpreter_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/interpreter_definitions.hrl").

-export([ all/0 ]).
-export([ empty_state_should_have_fixed_memory_size/1,
          pointers_should_be_set_at_the_beginning_after_init/1 ]).

all() -> [ empty_state_should_have_fixed_memory_size,
           pointers_should_be_set_at_the_beginning_after_init ].

empty_state_should_have_fixed_memory_size(_Context) ->
    State = bferl_interpreter:init(),
    ?assertEqual(?MEMORY_SIZE, array:size(State#interpreter_state.memory)).

pointers_should_be_set_at_the_beginning_after_init(_Context) ->
    State = bferl_interpreter:init(),
    ?assertEqual(0, State#interpreter_state.instructions_pointer),
    ?assertEqual(0, State#interpreter_state.memory_pointer).
