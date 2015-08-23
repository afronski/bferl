-module(io_properties_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ all/0 ]).
-export([ exhaustive_input_and_output_operations_from_tape_should_be_printed_in_reverse_order/1,
          printing_from_memory_should_reflect_ASCII_representation/1 ]).


all() ->
    [ exhaustive_input_and_output_operations_from_tape_should_be_printed_in_reverse_order,
      printing_from_memory_should_reflect_ASCII_representation ].

exhaustive_input_and_output_operations_from_tape_should_be_printed_in_reverse_order(_Context) ->
    ?assertEqual(true,
                 proper:quickcheck(io_model:prop_exhaustive_input_and_output_operations_from_tape_should_be_printed_in_reverse_order(),
                                   [ {to_file, user}, {numtests, 100} ])).

printing_from_memory_should_reflect_ASCII_representation(_Context) ->
    ?assertEqual(true,
                 proper:quickcheck(io_model:prop_printing_from_memory_should_reflect_ASCII_representation(),
                                   [ {to_file, user}, {numtests, 100} ])).
