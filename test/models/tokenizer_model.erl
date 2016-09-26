-module(tokenizer_model).

-include_lib("proper/include/proper.hrl").

-include("../../include/tokens_definitions.hrl").

-export([ prop_tokenizer_should_exclude_superfluous_tokens_for_brainfuck/0,
          prop_tokenizer_should_exclude_superfluous_tokens_for_brainfork/0,
          prop_tokenizer_should_whitelist_tokens_based_on_input/0 ]).

%% Properties.

prop_tokenizer_should_exclude_superfluous_tokens_for_brainfork() ->
    ?FORALL(Input, string(),
            begin
                BrainforkTokens = bferl_tokenizer:from_string(Input, ?BRAINFORK),
                lists:all(fun (Token) -> sets:is_element(Token, ?BRAINFORK) end, BrainforkTokens)
            end).

prop_tokenizer_should_exclude_superfluous_tokens_for_brainfuck() ->
    ?FORALL(Input, string(),
            begin
                BrainfuckTokens = bferl_tokenizer:from_string(Input),
                lists:all(fun (Token) -> sets:is_element(Token, ?BRAINFUCK) end, BrainfuckTokens)
            end).

prop_tokenizer_should_whitelist_tokens_based_on_input() ->
    ?FORALL(Input, string(),
            ?FORALL(Whitelist, string(),
            begin
                ValidTokens = sets:from_list(Whitelist),
                Tokens = bferl_tokenizer:from_string(Input, ValidTokens),
                lists:all(fun (Token) -> sets:is_element(Token, ValidTokens) end, Tokens)
            end)).
