-module(event_test).
-author("André Theuma").

%%% Includes.
-include_lib("eunit/include/eunit.hrl").
-include("../../include/event.hrl").

-define(VALID_EVENT, 'fork(<0.102.0>,<0.808.0>,{calc_server_bug,loop,[0]})').
-define(INVALID_EVENT, 'fork(<0.102.0>,BadPID,{calc_server_bug,loop,[0]})').

is_empty_event_test() ->
    ?assertEqual(?is_empty('E'), true),
    ?assertEqual(?is_empty('recv(<0.808.0>,{<0.585.0>,{add,10.0,97.0}})'), true).

is_mfa_test() ->
    ?assertEqual(?is_mfa('calc_server_bug',loop,[0]), true),
    ?assertEqual(?is_mfa(calc_server_bug,loop,twenty), false).

is_mfa_2_test() ->
    case ?is_mfa('calc_server_bug',loop,[0]) of
        true -> io:format("true~n");
        _ -> io:format("false~n")
    end,
    case ?is_mfa(calc_server_bug,loop,2) of
        true -> io:format("true~n");
        _ -> io:format("false~n")
    end.
