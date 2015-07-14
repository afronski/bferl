-module(file_access_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/tokens_definitions.hrl").

-define(HELLO_WORLD_BRAINFUCK, "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.").
-define(HELLO_WORLD_BRAINFORK, "Y" ++ ?HELLO_WORLD_BRAINFUCK).

-define(TO_STRING(Code), string:join(Code, "")).

-export([ all/0 ]).
-export([ reading_content_from_file/1,
          reading_content_from_file_with_different_tokens_set/1 ]).

all() -> [ reading_content_from_file,
           reading_content_from_file_with_different_tokens_set ].

reading_content_from_file(_Context) ->
    ?assertEqual(?HELLO_WORLD_BRAINFUCK, ?TO_STRING(bferl_tokenizer:from_file("../../../../test/assets/hello_world.bf"))),
    ?assertEqual(?HELLO_WORLD_BRAINFUCK, ?TO_STRING(bferl_tokenizer:from_file("../../../../test/assets/hello_world_with_comments.bf"))).

reading_content_from_file_with_different_tokens_set(_Context) ->
    ?assertEqual(?HELLO_WORLD_BRAINFORK, ?TO_STRING(bferl_tokenizer:from_file("../../../../test/assets/hello_world.bfo", ?BRAINFORK))),
    ?assertEqual(?HELLO_WORLD_BRAINFORK, ?TO_STRING(bferl_tokenizer:from_file("../../../../test/assets/hello_world_with_comments.bfo", ?BRAINFORK))).
