-module(sys_info_parser_test).
-author("André Theuma").

-include_lib("eunit/include/eunit.hrl").

parse_transition_file_test() ->
    FileContent = [
        "{START, NULL, s0};\n",
        "{s0, -1, s3};\n",
        "{s0, 1, s1};\n",
        "{s1, 0, s2};\n",
        "{s2, N, s1};\n",
        "{s3, Z \\ 0, s3};"
    ],

    Expected = [
        {'START', {{'fun', null}, []}, s0},
        {s0, {is_integer, -1}, s3},
        {s0, {is_integer, 1}, s1},
        {s1, {is_integer, 0}, s2},
        {s2, {{'fun', is_natural_integer}, []}, s1},
        {s3, {{'fun', is_any_integer}, [setminus | {is_integer, 0}]}, s3}
    ],

    TempFile = filename:join(["test", "regeneration", "test_transitions.tmp"]),
    try
        ok = file:write_file(TempFile, list_to_binary(FileContent)),
        ?assertEqual(Expected, sys_info_parser:parse_file(TempFile))
    after
        file:delete(TempFile)
    end.
