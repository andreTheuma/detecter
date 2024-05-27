%%% -------------------------------- %%%
%%% TESTS FOR monitor MODULE CONCERNING %%%
%%%             REGEN ONLY           %%%
%%% -------------------------------- %%%

-module(monitor_test).
-author("André Theuma").

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

% -define(E1, {recv,c:pid(0,808,0),c:pid(0,630,0),{add,10.0,97.0}}).
% -define(E2, {send,c:pid(0,808,0),c:pid(0,630,0),{ok,-87.0}}).
% -define(E3, {recv,c:pid(0,808,0),c:pid(0,630,0),{mul,10.0,97.0}}).
% -define(E4, {send,c:pid(0,808,0),c:pid(0,630,0),{ok,970.0}}).

-define(ROOT_PID, c:pid(0, 102, 0)).
-define(E1, 'fork(<0.102.0>,<0.808.0>,{calc_server_bug,loop,[0]})').
-define(E2, 'exit(<0.102.0>,normal)').
-define(E3, 'init(<0.808.0>,<0.102.0>,{calc_server_bug,loop,[0]})').
-define(E4, 'recv(<0.808.0>,{<0.630.0>,{add,10.0,97.0}})').
-define(E5, 'send(<0.808.0>,<0.630.0>,{ok,-87.0})').

-define(TRACE_FILE, "../../priv/trace_test_4.log").
-define(PID_QUEUE_1, c:pid(0, 808, 0)).
-define(PID_QUEUE_2, c:pid(0, 630, 0)).


%%% Includes.
-include_lib("eunit/include/eunit.hrl").
-include("../../include/event.hrl").

%%% ----------------------------------------------------------------------------
%%% Tests.
%%% ----------------------------------------------------------------------------

init_event_buffer_test() ->
    EventBuffer = analyzer:init_event_buffer(),
    case ets:info(EventBuffer) of
        undefined -> ?assert(false);
        _ -> ?assert(true),
         ets:delete(EventBuffer)
    end.

post_event_to_event_buffer_test() ->
    EventBuffer = analyzer:init_event_buffer(),
    analyzer:post_to_event_buffer(self(),?E1),
    [{_,Event}] = ets:lookup(EventBuffer, self()),
    io:format("Event: ~p~n", [Event]),
    ?assertEqual(Event, {?E1}),
    
    ets:lookup(EventBuffer, self()),
    ets:delete(EventBuffer).

post_list_of_events_to_event_buffer_test() ->
    EventBuffer = analyzer:init_event_buffer(),
    analyzer:post_to_event_buffer(self(),[?E1,?E2]),
    [{_,Events}] = ets:lookup(EventBuffer, self()),
    io:format("Events: ~p~n", [Events]),
    
    ets:delete(EventBuffer).

return_event_buffer_test() ->
    EventBuffer = analyzer:init_event_buffer(),
    analyzer:create_event_buffer(self()),
    analyzer:post_to_event_buffer(self(),[?E1]),
    Event = analyzer:return_event_buffer(self()),
    io:format("Event: ~p~n", [Event]),
    case is_list(Event) of
        true -> io:format("Event is a list~n");
        false -> io:format("Event is not a list~n")
    end,
    ets:delete(EventBuffer).

return_multiple_event_buffer_test() ->
    EventBuffer = analyzer:init_event_buffer(),
    analyzer:create_event_buffer(self()),
    analyzer:post_to_event_buffer(self(),[?E1,?E2]),
    Event = analyzer:return_event_buffer(self()),
    io:format("Event: ~p~n", [Event]),
    case is_list(Event) of
        true -> io:format("Event is a list~n");
        false -> io:format("Event is not a list~n")
    end,
    ets:delete(EventBuffer).


% post_to_event_queue_table_test() ->
%     EventQueue = analyzer:init_event_queue_table(),
%     analyzer:post_to_event_queue_table(?PID_QUEUE_1,?E1),
%     ?assertEqual(ets:lookup(EventQueue, ?PID_QUEUE_1),[{?PID_QUEUE_1,{?EMPTY_EVENT,?EMPTY_EVENT,?E1}}]),

%     analyzer:post_to_event_queue_table(?PID_QUEUE_1,?E2),
%     ?assertEqual(ets:lookup(EventQueue, ?PID_QUEUE_1), [{?PID_QUEUE_1,{?EMPTY_EVENT,?E1,?E2}}]),


%     analyzer:post_to_event_queue_table(?PID_QUEUE_1,?E3),
%     ?assertEqual(ets:lookup(EventQueue, ?PID_QUEUE_1), [{?PID_QUEUE_1,{?E1,?E2,?E3}}]),

%     analyzer:post_to_event_queue_table(?PID_QUEUE_1,?E4),
%     ?assertEqual(ets:lookup(EventQueue, ?PID_QUEUE_1), [{?PID_QUEUE_1,{?E2,?E3,?E4}}]),

%     ets:delete(EventQueue).

% post_to_multiple_event_queue_table_test() ->

%     EventQueue = analyzer:init_event_queue_table(),
%     analyzer:post_to_event_queue_table(?PID_QUEUE_1,?E1),
%     ?assertEqual(ets:lookup(EventQueue, ?PID_QUEUE_1),[{?PID_QUEUE_1,{?EMPTY_EVENT,?EMPTY_EVENT,?E1}}]),

%     analyzer:post_to_event_queue_table(?PID_QUEUE_1,?E2),
%     ?assertEqual(ets:lookup(EventQueue, ?PID_QUEUE_1), [{?PID_QUEUE_1,{?EMPTY_EVENT,?E1,?E2}}]),


%     analyzer:post_to_event_queue_table(?PID_QUEUE_1,?E3),
%     ?assertEqual(ets:lookup(EventQueue, ?PID_QUEUE_1), [{?PID_QUEUE_1,{?E1,?E2,?E3}}]),

%     analyzer:post_to_event_queue_table(?PID_QUEUE_1,?E4),
%     ?assertEqual(ets:lookup(EventQueue, ?PID_QUEUE_1), [{?PID_QUEUE_1,{?E2,?E3,?E4}}]),

%     analyzer:post_to_event_queue_table(?PID_QUEUE_2,?E1),
%     ?assertEqual(ets:lookup(EventQueue, ?PID_QUEUE_2),[{?PID_QUEUE_2,{?EMPTY_EVENT,?EMPTY_EVENT,?E1}}]),

%     analyzer:post_to_event_queue_table(?PID_QUEUE_2,?E2),
%     ?assertEqual(ets:lookup(EventQueue, ?PID_QUEUE_2), [{?PID_QUEUE_2,{?EMPTY_EVENT,?E1,?E2}}]).

% % delete_event_queue_table_test() ->
% %     EventQueue = analyzer:init_event_queue_table(),
% %     analyzer:post_to_event_queue_table(?E1),
% %     analyzer:post_to_event_queue_table(?E2),
% %     analyzer:post_to_event_queue_table(?E3),
% %     analyzer:post_to_event_queue_table(?E4),
% %     analyzer:delete_event_queue_table(),
% %     case ets:info(EventQueue) of
% %         undefined -> ?assert(true);
% %         _ -> ?assert(false)
% %     end.

% % return_event_queue_table_test() ->
% %     EventQueue = analyzer:init_event_queue_table(),
% %     analyzer:post_to_event_queue_table(?E1),
% %     analyzer:post_to_event_queue_table(?E2),
% %     analyzer:post_to_event_queue_table(?E3),
% %     ?assertEqual(analyzer:return_event_queue_table(), {?E1,?E2,?E3}),
% %     ets:delete(EventQueue).

% % post_events_from_trace_to_monitor_test() ->
% %     monitor:start_outline(?TRACE_FILE,?ROOT_PID, fun prop_add_rec:mfa_spec/1,[]),
% %     % fork(<0.102.0>,<0.808.0>,{calc_server_bug,loop,[0]})
% %     % timer:sleep(1000),
% %     % outline_simulator:post_event_to_log({fork, c:pid(0, 102, 0), c:pid(0, 808, 0), {calc_server_bug, loop, [0]}}),
% %     % timer:sleep(1000),
% %     % outline_simulator:post_event_to_log({exit, c:pid(0, 102, 0), normal}),
% %     % timer:sleep(1000),
% %     % outline_simulator:post_event_to_log({init, c:pid(0, 808, 0), c:pid(0, 102, 0), {calc_server_bug, loop, [0]}}),
% %     % timer:sleep(1000),
% %     % outline_simulator:post_event_to_log({recv, c:pid(0, 808, 0), {c:pid(0, 585, 0), {add, 10.0, 97.0}}}),
% %     % timer:sleep(1000),
% %     % outline_simulator:post_event_to_log({send, c:pid(0, 808, 0), c:pid(0, 585, 0), {ok, -87.0}}),
% %     % timer:sleep(5000),

% %     timer:sleep(5000),
% %     outline_simulator:post_event_to_log(?E1),
% %     timer:sleep(5000),
% %     outline_simulator:post_event_to_log(?E2),

% %     monitor:stop().