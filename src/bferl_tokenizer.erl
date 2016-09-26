-module(bferl_tokenizer).

-include("../include/tokens_definitions.hrl").

-export([ from_string/1, from_string/2,
          from_file/1, from_file/2 ]).

-spec from_string(bferl_types:program()) -> bferl_types:instructions().
from_string(Input) when is_list(Input) ->
    from_string(Input, ?BRAINFORK).

-spec from_string(bferl_types:program(), bferl_types:tokens()) -> bferl_types:instructions().
from_string(Input, Tokens) when is_list(Input) ->
    exclude_unknown_tokens(lists:map(fun (Char) -> string:chars(Char, 1) end, Input), Tokens).

-spec exclude_unknown_tokens(bferl_types:program(), bferl_types:tokens()) -> bferl_types:instructions().
exclude_unknown_tokens(List, Tokens) when is_list(List)  ->
    lists:filter(fun (Token) -> sets:is_element(Token, Tokens) =:= true end, List).

-spec from_file(string()) -> bferl_types:instructions().
from_file(Filename) when is_list(Filename)  ->
    from_file(Filename, ?BRAINFUCK).

-spec from_file(string(), bferl_types:tokens()) -> bferl_types:instructions().
from_file(Filename, Tokens) when is_list(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    Content = unicode:characters_to_list(Binary),
    from_string(Content, Tokens).
