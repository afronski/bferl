-module(tokenizer_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/tokens_definitions.hrl").

-export([ all/0 ]).
-export([ tokenize_input/1, tokenization_edge_cases/1,
          clean_input/1,
          accepting_tokens_from_other_sets/1 ]).

all() -> [ tokenize_input, tokenization_edge_cases,
           clean_input,
           accepting_tokens_from_other_sets ].

tokenize_input(_Context) ->
    ?assertEqual([".", ","], bferl_tokenizer:from_string(".,")),
    ?assertEqual(["+", "-"], bferl_tokenizer:from_string("+-")),
    ?assertEqual(["[", "]"], bferl_tokenizer:from_string("[]")),
    ?assertEqual(["<", ">"], bferl_tokenizer:from_string("<>")).

tokenization_edge_cases(_Context) ->
    ?assertEqual([], bferl_tokenizer:from_string("\tqwertyuiop{}\\|asdfghjkl:;\"'zxcvbnm?/\n\r1234567890!@#$%^&*()`~_=")),
    ?assertEqual([], bferl_tokenizer:from_string("")).

clean_input(_Context) ->
    ?assertEqual([".", ",", "[", "]", "<", ">", "+", "-"], bferl_tokenizer:from_string("\tABCD .,[]<>+- ABCD\t")).

accepting_tokens_from_other_sets(_Context) ->
    ?assertEqual([".", ",", "[", "]", "<", ">", "+", "-", "Y"], bferl_tokenizer:from_string("\tABCD .,[]<>+-Y ABCD\t", ?BRAINFORK)),
    ?assertEqual(["Y"], bferl_tokenizer:from_string("XYZ", ?BRAINFORK)).
