%%% -------------------------------- %%%
%%% TESTS FOR event_table_parser MODULE %%%
%%% -------------------------------- %%%

-module(event_table_parser_test).
-author("André Theuma").

%%% Includes.
-include_lib("eunit/include/eunit.hrl").

%%% ----------------------------------------------------------------------------
%%% Tests.
%%% ----------------------------------------------------------------------------

check_table_test() ->
    {ok,Table} = event_table_parser:parse_table("../../priv/sys_info_test.spec"),
    io:format("Table: ~p~n", [Table]).

parse_event_table_test() ->
    {ok, EventTable} = event_table_parser:parse_table("../../priv/sys_info_test.spec"),
    ?assertEqual(
        [{"SPAWN",[],["SPAWNED"]},
              {"SPAWNED",["SPAWN"],["SEND"]},
              {"SEND",["SPAWNED","RECEIVE"],["RECEIVE"]},
              {"RECEIVE",["SEND"],["SEND","EXIT"]},
              {"EXIT",["RECEIVE"],[]}]
        ,EventTable).

string_tokenizer_test() -> 
    ?assertEqual(
        {"SPAWN",[],["SPAWNED"]},
        event_table_parser:string_tokenizer("SPAWN | {}; {SPAWNED}")),
    ?assertEqual(
        {"SPAWNED",["SPAWN"],["SEND"]},
        event_table_parser:string_tokenizer("SPAWNED | {SPAWN}; {SEND}")),
    ?assertEqual(
        {"SEND",["SPAWNED","RECEIVE"],["RECEIVE"]},
        event_table_parser:string_tokenizer("SEND | {SPAWNED,RECEIVE}; {RECEIVE}")),
    ?assertEqual(
        {"RECEIVE",["SEND"],["SEND","EXIT"]},
        event_table_parser:string_tokenizer("RECEIVE | {SEND}; {SEND,EXIT}")),
    ?assertEqual(
        {"EXIT",["RECEIVE"],[]},
        event_table_parser:string_tokenizer("EXIT | {RECEIVE}; {}")).

file_format_checker_test() ->
    ?assertEqual(
        event_table_parser:file_format_checker(["SPAWN | {}; {SPAWNED}"]),
        ok),
    ?assertEqual(
        event_table_parser:file_format_checker(["SPAWN  {SPAWNED}; {SPAWNED}"]),
        {error, "Invalid format. The format should be <current_event> | {<previous_events>}; {<next_events>"}).