%%% -------------------------------- %%%
%%% TESTS FOR lin_tracer MODULE %%%
%%% -------------------------------- %%%

-module(lin_tracer_test).
-author("André Theuma").

%%% Includes.
-include_lib("eunit/include/eunit.hrl").

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(P1, c:pid(0, 100, 0)).
-define(P2, c:pid(0, 102, 0)).
-define(P3, c:pid(0, 104, 0)).
-define(P4, c:pid(0, 106, 0)).

-define(TRACE_FILE, "../../priv/trace_test.log").

% assertEqualWithWildcard(Actual, Expected) ->
%     case {Expected, Actual} of
%         {_, _} when Expected == Actual -> ok;
%         {_, '_'} -> ok;  % Wildcard match
%         {_, _} ->
%             Msg = io_lib:format("Expected ~p but got ~p", [Expected, Actual]),
%             throw({assertion_failed, Msg})
%     end.

%%% ----------------------------------------------------------------------------
%%% Tests.
%%% ----------------------------------------------------------------------------

tracer_start_test() ->
    {ok, Pid} = lin_tracer:start(""),
    ?assertEqual(lin_tracer:return_alloc(Pid), Pid),
    lin_tracer:stop().

tracer_allocation_test() ->
    lin_tracer:start(?TRACE_FILE).

missing_event_test() ->
    case log_lexer:string("_X_(<0.808.0>,{<0.585.0>,{add,10.0,97.0}})") of
        {ok, [], _} ->
            {ok, skip};
        {ok, Tokens, _} ->
            io:format("Tokens: ~p~n", [Tokens]),
            % when Tokens contains the corrupt event token 
            % the following case will be executed
            case log_parser:parse(Tokens) of
                {ok, Ast} ->
                    case Ast of
                        {corrupt_payload, Event ,Pid} ->
                            {ok,Pid};
                        _ ->
                            error("Expected corrupt event")
                    end;
                {error, {_, _, ErrorDesc}} ->
                    error({error, log_parser:format_error(ErrorDesc)})
            end;

        {error, {_, _, ErrorDesc}, _} ->
            error({error, log_lexer:format_error(ErrorDesc)})
    end.
