%%% ----------------------------------------------------------------------------
%%% Manual driver for a generated modular monitor.
%%%
%%% Not part of the automated suite: it expects a pre-compiled
%%% prop_no_failure_flu module on the code path and prints the monitor's
%%% response to an init trace event. Kept for interactive experiments and as
%%% a seed for the Phase 15 AGM experiment suite; automated end-to-end init
%%% coverage lives in maxhml_synthesis_test.
%%%
%%% Usage:
%%%   erl -pa ebin -eval 'automated_event_streamer:run().'
%%% ----------------------------------------------------------------------------
-module(automated_event_streamer).
-author("Andre Theuma").

-export([run/0]).

%% Renamed from modular_monitor_test/0 so the name cannot be mistaken for an
%% automated EUnit case.
run() ->

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
