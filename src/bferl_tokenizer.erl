-module(bferl_tokenizer).

-include("../include/tokens_definitions.hrl").

-export([ from_string/1, from_string/2,
          from_file/1, from_file/2 ]).

from_string(Input) ->
    from_string(Input, ?BRAINFUCK).

from_string(Input, Tokens) ->
    exclude_unknown_tokens(lists:map(fun (Char) -> string:chars(Char, 1) end, Input), Tokens).

exclude_unknown_tokens(List, Tokens) ->
    lists:filter(fun (Token) -> sets:is_element(Token, Tokens) =:= true end, List).

from_file(Filename) ->
    from_file(Filename, ?BRAINFUCK).

from_file(Filename, Tokens) ->
    {ok, Binary} = file:read_file(Filename),
    Content = unicode:characters_to_list(Binary),
    from_string(Content, Tokens).
