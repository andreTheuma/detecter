-module(regeneration_test).
-author("André Theuma").

-define(TEST,true).
-include_lib("eunit/include/eunit.hrl").

-compile([debug_info]).

% PID examples
-define(PID1_TEST, c:pid(0, 11, 0)).
-define(PID2_TEST, c:pid(0, 12, 0)).
-define(PID3_TEST, c:pid(0, 13, 0)).
-define(PID4_TEST, c:pid(0, 14, 0)).
% list of events
-define(EVENT_TRACE, [
    % {trace, c:pid(0, 11, 0), spawn, c:pid(0, 12, 0), {token_server, loop, [undefined, undefined]}},
    % {trace, c:pid(0, 12, 0), spawned , c:pid(0, 13, 0), {token_server, loop, [undefined, undefined]}},
    {trace, c:pid(0, 13, 0), send, "message1" ,c:pid(0, 12, 0)},
    {trace, c:pid(0, 12, 0), 'receive', "message1"}
]).

-define(POST_EVENTS, [{delay, 0, {recv, c:pid(0, 11, 0), msg_to_p2}},
                {delay, 0, {recv, c:pid(0, 12, 0), msg_to_p3}},
                {delay, 0, {recv, c:pid(0, 13, 0), msg_to_p4}},
                {delay, 0, {exit, c:pid(0, 12, 0), normal}},
                {delay, 0, {exit, c:pid(0, 12, 0), normal}},
                {delay, 0, {exit, c:pid(0, 12, 0), normal}}]).


offline_trace_test_() ->
    
    Event1 = {fork, ?PID1_TEST, ?PID2_TEST, {m2, f2, []}},

    % monitor:start_offline("test_log.log", ?PID1_TEST, fun prop_no_leak:mfa_spec/1 ,[]).
    monitor:start_outline("", ?PID1_TEST, fun prop_no_leak:mfa_spec/1 ,[]),
    % log_tracer:post_event({delay,0,Event1}),
    % io:format("Posted event 1 LOG ~n"),
    % lin_tracer:post_event(Event_1),
    % lin_tracer:start(""),
    lin_tracer:event_handler({delay, 0, Event1}),

    Event2 = {init, ?PID2_TEST, ?PID1_TEST, {m2, f2, []}},

    monitor:stop().

missing_event_test() ->
    
    % EventTable = event_table_parser:parse("event_table_v1.txt"),
    % put event table in global state
    % put(event_table, EventTable),

    EmptyEvent = {},
    Event1 = {fork, ?PID1_TEST, ?PID2_TEST, {m2, f2, []}},
    Event2 = {init, ?PID2_TEST, ?PID1_TEST, {m2, f2, []}},
    Event3 = {exit, ?PID2_TEST, normal},
    Event4Missing = {send, ?PID1_TEST, ?PID2_TEST, "message1"},
    Event5 = {recv, ?PID2_TEST, "message1"},

    monitor:start_outline("", ?PID1_TEST, fun prop_no_leak:mfa_spec/1 ,[]),

    {_, _,CurrentEvent} = event_passer_test(event:to_evm_event(Event1),{delay, 0, Event2}),
    {_, _,CurrentEvent1} = event_passer_test(CurrentEvent,{delay, 0, Event3}),
    {error, PreviousEvent, MissingEvent} = event_passer_test(CurrentEvent1,{delay, 0, Event4Missing}),
    % Ping the next event
    NextEvent = Event5,

    case regen_eval:generate_missing_event(NextEvent,PreviousEvent, ?EVENT_TRACE) of
        {ok, MissingEvent} ->
            io:format("Missing event found: ~p~n",[MissingEvent]);
        undefined ->
            io:format("No missing event found~n");
        {error, Reason} ->
            io:format("Error: ~p~n",[Reason])
    end,

    monitor:stop().


event_passer_test(PreviousEvent,CurrentEventPayload) ->

    case lin_tracer:event_handler(PreviousEvent,CurrentEventPayload) of
        {ok, PreviousEvent,CurrentEvent} ->
            {ok, PreviousEvent,CurrentEvent};
        {error, PreviousEvent, MissingEvent} ->
            {error, PreviousEvent, MissingEvent}
    end.

generate_system_info_table_test() ->
    INFOTABLE = lin_tracer:create_sys_info_table(),
    io:format("INFO TABLE: ~p~n",[INFOTABLE]),

    {ok, TABLE} = event_table_parser:parse_table("event_table_v1.txt"),

    case lin_tracer:create_sys_info(TABLE) of
        true ->
            io:format("System info table created successfully.~n");
        false ->
            io:format("Error creating system info table.~n")
    end,

    {table,RETRIEVEDTABLE} = lin_tracer:return_sys_info(),
    io:format("RETRIEVED TABLE: ~p~n",[RETRIEVEDTABLE]),

    case lin_tracer:delete_sys_info_table() of
        true ->
            io:format("System info table deleted successfully.~n");
        false ->
            io:format("Error deleting system info table.~n")
    end.

% ets:new(sysInfo, [set,public,named_table]).
% ets:insert(sysInfo, {table, [{"START",[],["init"]}, {"run",["init","idle"],["read","idle"]}]}).
