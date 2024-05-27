-module(outline_simulator_test).
-author("André Theuma").

%%% Includes.
-include_lib("eunit/include/eunit.hrl").

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(E1, 'fork(<0.102.0>,<0.808.0>,{calc_server_bug,loop,[0]})').
-define(E2, 'exit(<0.102.0>,normal)').
-define(E3, 'init(<0.808.0>,<0.102.0>,{calc_server_bug,loop,[0]})').
-define(E4, 'recv(<0.808.0>,{<0.585.0>,{add,10.0,97.0}})').
-define(E5, 'send(<0.808.0>,<0.585.0>,{ok,-87.0})').

-define(E6, 'fork(<0.102.0>,<0.809.0>,{calc_server_bug,loop,[0]})').
-define(E7, 'fork(<0.102.0>,<0.810.0>,{calc_server_bug,loop,[0]})').
-define(E8, 'fork(<0.102.0>,<0.811.0>,{calc_server_bug,loop,[0]})').


-define(E9, 'recv(<0.808.0>,{<0.585.0>,{mul,10.0,97.0}})').
-define(E10, 'send(<0.808.0>,<0.585.0>,{ok,970.0})').
-define(E11, 'recv(<0.808.0>,{<0.585.0>,{div,10.0,97.0}})').
-define(E12, 'send(<0.808.0>,<0.585.0>,{ok,0.10309278350515463})').
-define(E13, 'recv(<0.808.0>,{<0.585.0>,{sub,10.0,97.0}})').
-define(E14, 'send(<0.808.0>,<0.585.0>,{ok,-87.0})').



% -define(E1, {recv,c:pid(0,630,0),{add,10.0,97.0}}).
% -define(E2, {send,c:pid(0,808,0),c:pid(0,630,0),{ok,-87.0}}).
% -define(E3, {recv,c:pid(0,630,0),{mul,10.0,97.0}}).
% -define(E4, {send,c:pid(0,808,0),c:pid(0,630,0),{ok,970.0}}).

-define(TRACE_FILE, "../../priv/trace_test_5.log").

post_event_test() ->
    outline_simulator:post_event_to_log(?TRACE_FILE,?E1),
    outline_simulator:post_event_to_log(?TRACE_FILE,?E2),
    outline_simulator:post_event_to_log(?TRACE_FILE,?E3),
    outline_simulator:post_event_to_log(?TRACE_FILE,?E4).

post_multiple_fork_events_and_offline_monitor_test() ->

    monitor:start_outline(?TRACE_FILE, c:pid(0,102,0), fun prop_add_rec:mfa_spec/1 ,[]),

    timer:sleep(2000),

    outline_simulator:post_event_to_log(?TRACE_FILE,?E1),
    outline_simulator:post_event_to_log(?TRACE_FILE,?E6),
    outline_simulator:post_event_to_log(?TRACE_FILE,?E7),
    outline_simulator:post_event_to_log(?TRACE_FILE,?E8).

post_multiple_send_recv_events_and_offline_monitor_test() ->

    monitor:start_outline(?TRACE_FILE, c:pid(0,808,0), fun prop_add_rec:mfa_spec/1 ,[]),

    timer:sleep(2000),

    outline_simulator:post_event_to_log(?TRACE_FILE,?E9),
    outline_simulator:post_event_to_log(?TRACE_FILE,?E10),
    outline_simulator:post_event_to_log(?TRACE_FILE,?E11),
    outline_simulator:post_event_to_log(?TRACE_FILE,?E12),
    outline_simulator:post_event_to_log(?TRACE_FILE,?E13),
    outline_simulator:post_event_to_log(?TRACE_FILE,?E14).