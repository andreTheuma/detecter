-module(sys_info_parser_test).
-author("André Theuma").

-include_lib("eunit/include/eunit.hrl").

parse_start_null_transition_test() ->
    FileContent = [
        "{START, NULL, s0};\n"
    ],

    Expected = [
        {'START', {{'fun', null}, []}, s0}
    ],

    ?assertEqual(Expected, parse_lines(FileContent)).

parse_integer_events_test() ->
    FileContent = [
        "{s0, -1, s3};\n",
        "{s0, 1, s1};\n",
        "{s1, 0, s2};\n"
    ],

    Expected = [
        {s0, {is_integer, -1}, s3},
        {s0, {is_integer, 1}, s1},
        {s1, {is_integer, 0}, s2}
    ],

    ?assertEqual(Expected, parse_lines(FileContent)).

parse_symbolic_range_test() ->
    FileContent = [
        "{s2, N, s1};\n"
    ],

    Expected = [
        {s2, {{'fun', is_natural_integer}, []}, s1}
    ],

    ?assertEqual(Expected, parse_lines(FileContent)).

parse_setminus_guard_test() ->
    FileContent = [
        "{s3, Z \\ 0, s3};\n"
    ],

    Expected = [
        {s3, {{'fun', is_any_integer}, [setminus | {is_integer, 0}]}, s3}
    ],

    ?assertEqual(Expected, parse_lines(FileContent)).


parse_complex_events_test() ->
    FileContent = [
        "{s2, N, s1};\n",
        "{s3, Z \\ 0, s3};"
    ],

    Expected = [
        {s2, {{'fun', is_natural_integer}, []}, s1},
        {s3, {{'fun', is_any_integer}, [setminus | {is_integer, 0}]}, s3}
    ],
    ?assertEqual(Expected, parse_lines(FileContent)).

parse_lines(FileContent) ->
    TempFile = filename:join([
        "test",
        "regeneration",
        "test_transitions_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".tmp"
    ]),
    try
        ok = file:write_file(TempFile, list_to_binary(FileContent)),
        sys_info_parser:parse_file(TempFile)
    after
        file:delete(TempFile)
    end.
