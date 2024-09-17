%%% ----------------------------------------------------------------------------
%%% @author André Theuma
%%% 
%%% @doc This module contains functions that are used to evaluate the
%%% system information table. The system information table is the  table that contains process information 
%%% for the system that is being monitored. This table is primarily used to store 
%%% information about events that are directly related to trace events being 
%%% dispatched to tracers.
%%% ----------------------------------------------------------------------------

-module(sys_info_eval).
-author("André Theuma").

-include("log.hrl").

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Allocated System Information Table
-define(SYS_INFO_TABLE, sysInfoTable).
-define(SYS_INFO_KEY, sys_info).

-export([delete_sys_info_table/0, create_sys_info/1, return_sys_info/0]).

%%% ----------------------------------------------------------------------------
%%% System Info Table (SIT) functions.
%%% ----------------------------------------------------------------------------

%% @doc Delete the system information table. 
%%
%% {@returns true} 
-spec delete_sys_info_table() -> true.
delete_sys_info_table() ->
    case ets:info(?SYS_INFO_TABLE) of
        [] ->
            true;
        _ ->
            ets:delete(?SYS_INFO_TABLE),
            ?TRACE("System Information Table deleted.", []),
            true
    end.

%% @doc Create the system information table.
%% 
%% {@params
%%  {@name InfoTable}
%%  {@desc The file name that contains the system information.}
%% }
%%
%% {@returns true}
-spec create_sys_info(InfoTable) -> true
    when
    InfoTable :: event_table_parser:event_table().
create_sys_info(InfoTable) ->
    case ets:info(?SYS_INFO_TABLE) of
        undefined ->
            ets:new(?SYS_INFO_TABLE, [set, public, named_table]),
            ets:insert(?SYS_INFO_TABLE, {?SYS_INFO_KEY, InfoTable}),
            ?TRACE("~p inserted into System Information Table.", [InfoTable]);
        _ ->
            ?TRACE("System Information Table already exists. Returning existing table.", [])
    end.

%% @doc Create the system information AST by wrapping the system information table 
%% in a syntax tree.
-spec create_sys_info_ast(InfoTable) -> erl_syntax:syntaxTree()
    when
    InfoTable :: event_table_parser:event_table().
create_sys_info_ast(InfoTable) ->
    pass.


%% @doc Return the system information table.
%%
%% {@returns The system information table.}
-spec return_sys_info() -> EventTable 
    when 
    EventTable :: event_table_parser:event_table().
return_sys_info() ->
    case ets:lookup(?SYS_INFO_TABLE, ?SYS_INFO_KEY) of
        [] ->
            [];
        [{?SYS_INFO_KEY,InfoTable}] ->
            InfoTable
    end.