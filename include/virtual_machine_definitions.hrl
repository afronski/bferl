-define(VM_MEMORY_SIZE, 30000).
-define(VM_EMPTY_MEMORY, array:new([ {size, ?VM_MEMORY_SIZE}, {fixed, true}, {default, 0} ])).

-record(register_based_virtual_machine, { ir_code                   :: list(bferl_types:ir_opcode()),
                                          jmp_table                 :: map(),
                                          memory = ?VM_EMPTY_MEMORY :: array:array(integer()),
                                          r0 = 0                    :: integer(),
                                          ir0 = 0                   :: non_neg_integer(),
                                          ic = 0                    :: non_neg_integer(),
                                          zf = 0                    :: 0 | 1
                                        }).
