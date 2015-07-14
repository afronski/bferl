-module(bferl_tokenizer).
-export([ from_string/1, from_string/2 ]).

-define(BRAINFUCK, sets:from_list([".", ",", "[", "]", "<", ">", "+", "-"])).
-define(BRAINFORK, sets:add_element("Y", ?BRAINFUCK)).

from_string(Input) ->
    from_string(Input, ?BRAINFUCK).

from_string(Input, Tokens) ->
    exclude_unknown_tokens(lists:map(fun (Char) -> string:chars(Char, 1) end, Input), Tokens).

exclude_unknown_tokens(List, Tokens) ->
    lists:filter(fun (Token) -> sets:is_element(Token, Tokens) =:= true end, List).
