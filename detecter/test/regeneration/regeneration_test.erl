-module(regeneration_test).
-author("André Theuma").

-define(TEST,true).
-include_lib("eunit/include/eunit.hrl").
-include("../../include/log.hrl").

-compile([debug_info, {d, 'TEST'}]).

% PID examples
-define(PID1_TEST, c:pid(0, 11, 0)).
-define(PID2_TEST, c:pid(0, 12, 0)).
-define(PID3_TEST, c:pid(0, 13, 0)).
-define(PID4_TEST, c:pid(0, 14, 0)).

-define(EVENT_TRACE, [{
    {fork, c:pid(0, 100, 0), c:pid(0, 102, 0), {root, loop, [0]}},
    {fork, c:pid(0, 102, 0), c:pid(0, 808, 0), {calc_server_bug, loop, [0]}},
    {exit, c:pid(0, 102, 0), normal},
    {init, c:pid(0, 808, 0), c:pid(0, 102, 0), {calc_server_bug, loop, [0]}},
    {recv, c:pid(0, 808, 0), {c:pid(0, 585, 0), {add, 10.0, 97.0}}},
    {send, c:pid(0, 808, 0), c:pid(0, 585, 0), {ok, -87.0}},
    {recv, c:pid(0, 808, 0), {c:pid(0, 585, 0), {mul, 10.0, 97.0}}},
    {send, c:pid(0, 808, 0), c:pid(0, 585, 0), {ok, 970.0}},
    {recv, c:pid(0, 808, 0), {c:pid(0, 585, 0), stp}},
    {send, c:pid(0, 808, 0), c:pid(0, 585, 0), {bye, 0}},
    {exit, c:pid(0, 808, 0), normal},
    {exit, c:pid(0, 100, 0), normal}
}]).

%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%

%%% -------------------------------- %%%
%%% TESTS FOR regen_eval MODULE %%%
%%% -------------------------------- %%%

% Test to check if the previous events of a given event are returned correctly
get_previous_events_test() ->

    CurrentEvent= {recv, ?PID2_TEST, "message1"},
    {ok,EventTable} = event_table_parser:parse_table("sys_info.txt"),

    PreviousEvents = regen_eval:get_previous_events(event:to_evm_event(CurrentEvent), EventTable),

    io:format("Previous events: ~p~n",[PreviousEvents]).
% Test to check if the next events of a given event are returned correctly
get_next_events_test() ->

    CurrentEvent = {recv, ?PID2_TEST, "message1"},
    {ok,EventTable} = event_table_parser:parse_table("sys_info.txt"),

    NextEvents = regen_eval:get_next_events(event:to_evm_event(CurrentEvent),EventTable),

    io:format("Next events: ~p~n",[NextEvents]). 

% TODO: GO OVER THIS TEST
% Test to check if the missing event is generated
generate_missing_event_test() ->

    PreviousEvent = {send, ?PID1_TEST, ?PID2_TEST, "message1"},
    % missing event is {recv, ?PID2_TEST, "message1"}
    NextEvent = {exit, ?PID2_TEST, normal},
    {ok,EventTable} = event_table_parser:parse_table("sys_info.txt"),

    case regen_eval:generate_missing_event(event:to_evm_event(NextEvent),event:to_evm_event(PreviousEvent), EventTable) of

        {ok, MissingEvent} ->
            io:format("Missing event found: ~p~n",[MissingEvent]);
        undefined ->
            io:format("No missing event found~n");
        {error, Reason} ->
            io:format("Error: ~p~n",[Reason])
    end.

%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%


%%% -------------------------------- %%%
%%% TESTS FOR lin_tracer MODULE %%%
%%% -------------------------------- %%%

post_event_outline_test() ->

    % Event1 = {fork, c:pid(0, 100, 0), c:pid(0, 102, 0), {root, loop, [0]}},
    % Event2 = {fork, c:pid(0, 102, 0), c:pid(0, 808, 0), {calc_server_bug, loop, [0]}},
    % Event3 ={exit, c:pid(0, 102, 0), normal},
    % Event4 = {init, c:pid(0, 808, 0), c:pid(0, 102, 0), {calc_server_bug, loop, [0]}},
    % Event5 = {recv, c:pid(0, 808, 0), {c:pid(0, 585, 0), {add, 10.0, 97.0}}},
    % Event6 = {send, c:pid(0, 808, 0), c:pid(0, 585, 0), {ok, -87.0}},


    monitor:start_outline("../../priv/trace_test_2.log", c:pid(0,102,0), fun prop_add_rec:mfa_spec/1 ,[]).

    % lin_tracer:post_event(Event1),
    % lin_tracer:post_event(Event2),
    % lin_tracer:post_event(Event3),
    % lin_tracer:post_event(Event4),
    % lin_tracer:post_event(Event5),

    % monitor:stop().

post_event_outline_missing_test() ->

    {Event1, Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event9, Event10} = ?EVENT_TRACE,

    Event5Missing = "X",

    monitor:start_outline("", c:pid(0,102,0), fun prop_no_leak:mfa_spec/1 ,[]),

    lin_tracer:post_event(Event1),
    lin_tracer:post_event(Event2),
    lin_tracer:post_event(Event3),
    lin_tracer:post_event(Event5Missing),

    monitor:stop().

event_queue_creation_test() ->

    Event1 = {fork,c:pid(0,102,0),c:pid(0,808,0),{calc_server_bug,loop,[0]}},
    Event2 = {init,c:pid(0,808,0),c:pid(0,102,0),{calc_server_bug,loop,[0]}},
    Event3 = {recv,c:pid(0,808,0),{c:pid(0,585,0),{add,10.0,97.0}}},

    analyzer:init_event_queue_table(),
    monitor:create_event_queue_table(Event1,Event2,Event3),
    EventQueue = monitor:return_event_queue_table(),

    io:format("Event Queue: ~p~n",[EventQueue]),

    monitor:delete_event_queue_table().

update_event_queue_table_test() ->

    Event1 = {fork,c:pid(0,102,0),c:pid(0,808,0),{calc_server_bug,loop,[0]}},
    Event2 = {init,c:pid(0,808,0),c:pid(0,102,0),{calc_server_bug,loop,[0]}},
    Event3 = {recv,c:pid(0,808,0),{c:pid(0,585,0),{add,10.0,97.0}}},
    Event4 = {send,c:pid(0,808,0),c:pid(0,585,0),{ok,-87.0}},

    monitor:init_event_queue_table(),
    monitor:update_event_queue_table(Event1),
    TABLE1 = monitor:return_event_queue_table(),
    io:format("Event Queue 1: ~p~n",[TABLE1]),
    monitor:update_event_queue_table(Event2),
    TABLE2 = monitor:return_event_queue_table(),
    io:format("Event Queue 2: ~p~n",[TABLE2]),
    monitor:update_event_queue_table(Event3),
    TABLE3 = monitor:return_event_queue_table(),
    io:format("Event Queue 3: ~p~n",[TABLE3]),
    monitor:update_event_queue_table(Event4),
    TABLE4 = monitor:return_event_queue_table(),
    io:format("Event Queue 4: ~p~n",[TABLE4]),

    monitor:delete_event_queue_table().

%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%

%%% -------------------------------- %%%
%%% TESTS FOR fault_gen MODULE %%%
%%% -------------------------------- %%%

generate_faults_from_trace_test() ->

    Event1 = {fork,c:pid(0,102,0),c:pid(0,808,0),{calc_server_bug,loop,[0]}},
    Event2 = {init,c:pid(0,808,0),c:pid(0,102,0),{calc_server_bug,loop,[0]}},
    Event3 = {recv,c:pid(0,808,0),{c:pid(0,585,0),{add,10.0,97.0}}},
    Event4 = {send,c:pid(0,808,0),c:pid(0,585,0),{ok,-87.0}},

    FaultyTrace = fault_gen:generate_faults_from_trace([Event1,Event2,Event3,Event4], 2),

    io:format("Original trace: ~p~n", [[Event1,Event2,Event3,Event4]]),
    io:format("Faulty trace: ~p~n",[FaultyTrace]).

%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%

%%% -------------------------------- %%%
%%% GEN TESTS %%%
%%% -------------------------------- %%%


% post_event_missing_gen_test() ->

%     % monitor:init_event_queue_table(),

%     Event1 = {fork,c:pid(0,102,0),c:pid(0,808,0),{calc_server_bug,loop,[0]}},
%     Event2 = {init,c:pid(0,808,0),c:pid(0,102,0),{calc_server_bug,loop,[0]}},
%     Event3 = {recv,c:pid(0,808,0),{c:pid(0,585,0),{add,10.0,97.0}}},
%     Event4Missing = "X",
    
%     Event4 = {send,c:pid(0,808,0),c:pid(0,585,0),{ok,-87.0}},
%     Event5 = {recv,c:pid(0,808,0),{c:pid(0,585,0),{mul,10.0,97.0}}},

%     lin_tracer:post_event(Event1),
%     lin_tracer:post_event(Event2),
%     lin_tracer:post_event(Event3),
%     lin_tracer:post_event(Event4Missing),
%     lin_tracer:post_event(Event5).         

post_event_to_monitor_test() ->
    % {foreachx,
    % fun({Monitors}) ->
    %     monitor:start_outline("", c:pid(0,102,0), Monitors,[]),
    %     lin_tracer:post_event({fork, c:pid(0,102,0), c:pid(0,808,0), {calc_server_bug, loop, [0]}}),
    %     monitor:stop()
    % end,
    % [{prop_no_leak, mfa_spec}]}.
    monitor:start_offline("", c:pid(0,100,0), fun prop_no_leak:mfa_spec/1 ,[]),
    lin_tracer:post_event(?EVENT_TRACE).

% post_event_offline_test() ->

%     Event1 = {fork,c:pid(0,102,0),c:pid(0,808,0),{calc_server_bug,loop,[0]}},
%     Event2 = {init,c:pid(0,808,0),c:pid(0,102,0),{calc_server_bug,loop,[0]}},
%     Event3 = {recv,c:pid(0,808,0),{c:pid(0,585,0),{add,10.0,97.0}}},
%     Event4 = {send,c:pid(0,808,0),c:pid(0,585,0),{ok,-87.0}},
%     Event5 = {recv,c:pid(0,808,0),{c:pid(0,585,0),{mul,10.0,97.0}}},
%     Event6 = {send,c:pid(0,808,0),c:pid(0,585,0),{ok,970.0}},
%     Event7 = {recv,c:pid(0,808,0),{c:pid(0,585,0),stp}},
%     Event8 = {send,c:pid(0,808,0),c:pid(0,585,0),{bye,0}},
%     Event9 = {exit,c:pid(0,808,0),normal},

%     monitor:start_offline("", c:pid(0,102,0), fun prop_no_leak:mfa_spec/1 ,[]),

%     log_tracer:post_event({delay,0,Event1}).



%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%
%%% -------------------------------------------------------------- %%%
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

handle_callback_test() ->

    Event1 = {fork, ?PID1_TEST, ?PID2_TEST, {m2, f2, []}},

    monitor:start_outline("", ?PID1_TEST, fun prop_no_leak:mfa_spec/1 ,[]),

     % ! THIS WILL NOT WORK 
    sys_info_eval:init_sys_info_table(),
    lin_tracer:event_handler({delay, 0, event:to_evm_event(Event1)}).

