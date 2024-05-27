%%% -------------------------------- %%%
%%% TESTS FOR sys_info_eval MODULE %%%
%%% -------------------------------- %%%

-module(sys_info_eval_test).
-author("André Theuma").

%%% Includes.
-include_lib("eunit/include/eunit.hrl").

-define(TABLE_INFO_FILE, "../../priv/sys_info_test.spec").

%%% ----------------------------------------------------------------------------
%%% Tests.
%%% ----------------------------------------------------------------------------

create_sys_info_test() ->
    {ok, SysInfoSource} = event_table_parser:parse_table(?TABLE_INFO_FILE),
    sys_info_eval:create_sys_info(SysInfoSource),
    ?assertEqual(ets:lookup(sysInfoTable, sys_info), [{sys_info,SysInfoSource}]),
    ets:delete(sysInfoTable).

delete_sys_info_table_test() ->
    {ok, SysInfoSource} = event_table_parser:parse_table(?TABLE_INFO_FILE),
    sys_info_eval:create_sys_info(SysInfoSource),
    sys_info_eval:delete_sys_info_table(),
    ?assertEqual(ets:info(sysInfoTable), undefined).

return_sys_info_test() ->
    {ok, SysInfoSource} = event_table_parser:parse_table(?TABLE_INFO_FILE),
    sys_info_eval:create_sys_info(SysInfoSource),
    ReturnedTable = sys_info_eval:return_sys_info(),
    ?assertEqual(ReturnedTable, SysInfoSource),
    ets:delete(sysInfoTable).