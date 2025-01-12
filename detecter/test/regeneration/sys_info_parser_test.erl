-module(sys_info_parser_test).
-author("André Theuma").

-include_lib("eunit/include/eunit.hrl").

% -export([parse_transition_file/0]).

% Test cases
parse_transition_file_test() ->
    % Test the parsing of the transition file.
    % Simulate the file content
    FileContent = [
        "{start, NULL, s0};\n",
        "{s0, -1, s3};\n",
        "{s0, 1, s1};\n",
        "{s1, 0, s2};\n",
        "{s2, N, s1};\n",
        "{s3, Z \\ 0, s3};"
    ],

    % Write the test content to a temporary file
    TempFile = "../test_transitions.txt",
    file:write_file(TempFile, list_to_binary(FileContent)),
    

    % Parse the file
    Transitions = sys_info_parser:parse_file(TempFile),

    % % The expected result
    Expected = [
        {'START', null, s0},
        {s0, -1, s3},
        {s0, 1, s1},
        {s1, 0, s2},
        {s2, fun(Event) -> is_integer(Event) andalso Event > 0 end, s1},
        {s3, fun(Event) -> is_integer(Event) andalso Event =/= 0 end, s3}
    ],

    % % % Compare the result with the expected one
    ?assertEqual(Expected, Transitions),

    % % Clean up the temporary file
    file:delete(TempFile).