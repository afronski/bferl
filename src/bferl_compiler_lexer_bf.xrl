Definitions.

NO_OPCODE = [^,\.\[\]<>\+\-]

Rules.

\+ : {token, {inc, TokenLine, list_to_existing_atom(TokenChars)}}.
\- : {token, {dec, TokenLine, list_to_existing_atom(TokenChars)}}.

<  : {token, {left, TokenLine, list_to_existing_atom(TokenChars)}}.
>  : {token, {right, TokenLine, list_to_existing_atom(TokenChars)}}.

\[ : {token, {start_loop, TokenLine, list_to_existing_atom(TokenChars)}}.
\] : {token, {end_loop, TokenLine, list_to_existing_atom(TokenChars)}}.

,  : {token, {in, TokenLine, list_to_existing_atom(TokenChars)}}.
\. : {token, {out, TokenLine, list_to_existing_atom(TokenChars)}}.

{NO_OPCODE}+ : skip_token.

%%% Nasty trick for Dialyzer :( - duplicated rule, with 'end_token' atom.
%%%
%%% Lexer will match the first one, and will not go to the next one.
%%%
%%% But, you should have 'end_token' atom in your lexer, otherwise `dialyzer`
%%% even in the Erlang 18+ (where ignore directives are implemented) will
%%% complain about not used function `yyrev/2`.

{NO_OPCODE}* : {end_token, undefined}.

Erlang code.
