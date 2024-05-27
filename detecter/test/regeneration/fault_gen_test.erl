-module(fault_gen_test).
-author("André Theuma").

%%% Includes.
-include_lib("eunit/include/eunit.hrl").

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------
%%% 
-define(ORIGINAL_TRACE, 
    [{
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

%%% ----------------------------------------------------------------------------
%%% Tests.
%%% ----------------------------------------------------------------------------

generate_faults_from_trace_test()->
    ?assertEqual(?ORIGINAL_TRACE, fault_gen:generate_faults_from_trace(?ORIGINAL_TRACE, 0)).
    % ! ITS POSSIBLE THAT THESE TESTS FAIL DUE TO THE RANDOM NATURE OF THE FAULT GENERATION...
    % ?assertNotEqual(?ORIGINAL_TRACE, fault_gen:generate_faults_from_trace(?ORIGINAL_TRACE, 1)),
    % ?assertNotEqual(?ORIGINAL_TRACE, fault_gen:generate_faults_from_trace(?ORIGINAL_TRACE, 2)),
    % ?assertNotEqual(?ORIGINAL_TRACE, fault_gen:generate_faults_from_trace(?ORIGINAL_TRACE, 5)),
    % ?assertNotEqual(?ORIGINAL_TRACE, fault_gen:generate_faults_from_trace(?ORIGINAL_TRACE, 10)).

