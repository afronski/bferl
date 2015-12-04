-module(bferl_types).

-include("../include/interpreter_definitions.hrl").

-export_type([ io_callbacks/0,
               program/0, opcode/0, instructions/0, tokens/0,
               interpreter_state/0 ]).

-type optional_fun() :: undefined | fun().

-type io_callbacks() :: {optional_fun(), optional_fun(), optional_fun()}.

-type program() :: string().
-type opcode() :: [ 43 | 44 | 45 | 46 | 60 | 62 | 91 | 93 | 89 ].
-type instructions() :: list(string()).
-type tokens() :: sets:set(string()).
-type interpreter_state() :: #interpreter{}.
