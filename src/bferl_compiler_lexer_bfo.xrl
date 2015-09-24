Definitions.

NO_OPCODE = [^,\.\[\]<>\+\-Y]

Rules.

\+ : {token, {inc, TokenChars}}.
\- : {token, {dec, TokenChars}}.

<  : {token, {left, TokenChars}}.
>  : {token, {right, TokenChars}}.

\[ : {token, {start_loop, TokenChars}}.
\] : {token, {end_loop, TokenChars}}.

,  : {token, {in, TokenChars}}.
\. : {token, {out, TokenChars}}.

Y  : {token, {fork, TokenChars}}.

{NO_OPCODE}+ : skip_token.

Erlang code.
