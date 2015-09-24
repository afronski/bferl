Nonterminals
opcode loop opcodes program.

Terminals
start_loop end_loop
inc dec
left right
in out.

Rootsymbol program.

program -> opcodes : lists:flatten('$1').

opcodes -> opcode : ['$1'].
opcodes -> loop : '$1'.
opcodes -> loop opcodes : '$1' ++ '$2'.
opcodes -> opcode opcodes : ['$1'] ++ '$2'.

loop -> start_loop opcodes end_loop : [start_loop, '$2', end_loop].
loop -> start_loop end_loop : [start_loop, end_loop].

opcode -> inc : inc.
opcode -> dec : dec.

opcode -> left : left.
opcode -> right : right.

opcode -> in : in.
opcode -> out : out.

Erlang code.
