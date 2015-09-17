-define(BRAINFUCK_INTERPRETER_PROMPT, "bf> ").
-define(BRAINFUCK_IO_PROMPT, "> ").

-define(MEMORY_SIZE, 30000).
-define(EMPTY_MEMORY, array:new([ {size, ?MEMORY_SIZE}, {fixed, true}, {default, 0} ])).

-record(interpreter, { io = {undefined, undefined, undefined} :: bferl_types:io_callbacks(),
                       stack = []                             :: list(pos_integer()),
                       instructions_counter = 0               :: non_neg_integer(),
                       instructions = undefined               :: undefined | bferl_types:instructions(),
                       instructions_pointer = 1               :: pos_integer(),
                       memory_pointer = 0                     :: non_neg_integer(),
                       memory = ?EMPTY_MEMORY                 :: array:array(integer())
                     }).
