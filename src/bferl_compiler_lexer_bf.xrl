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

Erlang code.
