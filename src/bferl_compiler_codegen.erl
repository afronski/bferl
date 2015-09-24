-module(bferl_compiler_codegen).

-export([ make_module/4 ]).

make_module(_Name, _Expressions, _Type, _Flags) ->
    io:format("Name: ~s Code: ~p Type: ~s Flags: ~p~n", [_Name, _Expressions, _Type, _Flags]),
    {ok, []}.
