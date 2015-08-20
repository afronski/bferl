-define(BRAINFUCK_INTERPRETER_PROMPT, "bf> ").
-define(BRAINFUCK_IO_PROMPT, "> ").

-define(MEMORY_SIZE, 30000).
-define(EMPTY_MEMORY, array:new([ {size, ?MEMORY_SIZE}, {fixed, true}, {default, 0} ])).

-record(interpreter, { io = {undefined, undefined, undefined}, stack = [], instructions_counter = 0,
                       instructions = undefined, instructions_pointer = 1,
                       memory_pointer = 0, memory = ?EMPTY_MEMORY }).
