-module(automated_event_streamer).
-author("Andre Theuma").

-export([modular_monitor_test/0]).

modular_monitor_test() ->

    _PropFile = "../props/prop_no_failure.hml",
    Module = prop_no_failure_flu,
    Event = {trace, self(), spawned, some_pid, {token_server, loop, [-1, other_arg]}},

    % Start Monitor
    MonitorPid = spawn(Module, flu_spec,[]),

    % Send an event
    MonitorPid ! Event,

    receive
    Response ->
        io:format("Monitor responded with: ~p~n", [Response])
    end.
