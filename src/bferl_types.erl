-module(bferl_types).

-include("../include/interpreter_definitions.hrl").
-include("../include/virtual_machine_definitions.hrl").

-export_type([ io_callbacks/0,
               program/0, opcode/0, instructions/0, tokens/0,
               interpreter_state/0,
               ir_opcode/0, ir_program/0,
               virtual_machine_state/0 ]).

-type optional_fun() :: undefined | fun().

-type io_callbacks() :: {optional_fun(), optional_fun(), optional_fun()}.

-type program() :: string().
-type opcode() :: [ 43 | 44 | 45 | 46 | 60 | 62 | 91 | 93 | 89 ].
-type instructions() :: list(string()).
-type tokens() :: sets:set(string()).
-type interpreter_state() :: #interpreter{}.

-type ir_opcode() :: {add, ir0 | ir, integer()} |
                     {test, r0, pos_integer()} |
                     {jmp, pos_integer()} |
                     {call, 1 | 2 | 3} |
                     {const, ir0 | ir, integer()} |
                     {mov, r0, ir0} |
                     {mov, ir0, r0}.
-type ir_program() :: list(ir_opcode()).
-type virtual_machine_state() :: #register_based_virtual_machine{}.
