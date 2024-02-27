-module(regen_eval).
-author("André Theuma").

-compile([debug_info]).

-export([event_exists/1]).
-export([get_previous_events/2, get_next_events/2]).
-export([generate_missing_event/3]).

-export([test/3]).

%% @doc Reads an event from the trace and checks if it exists ie: when mfa is not 'undefined'. if it is undefined,
%% return false, else return true.
-spec event_exists(Event) -> boolean() 
    when 
    Event :: event:evm_event().
event_exists(Event) ->
    case Event of
        {trace,_,spawn,_,Mfa} when Mfa =:= undefined -> 
            true;
        {trace,_,spawned,_,Mfa} when Mfa =:= undefined ->
            true;
        {trace,_,exit,_}-> 
            true;
        {trace,_,send,_,_} ->
            true;
        {trace,_,'receive',_} ->
            true;
        _ ->
            false
    end.

%% @doc Returns the previous events of the current event by checking the event table and returning the previous events 
-spec get_previous_events(CurrentEvent, EventTable) -> [event:event_atom()]
    when
    CurrentEvent :: event_table_parser:event_atom(),
    EventTable :: event_table_parser:event_table().
get_previous_events(_, []) ->
    [];
get_previous_events(CurrentEvent, [{Event, PreviousEvents, _} | Rest]) ->
    case CurrentEvent =:= Event of
        true ->
            PreviousEvents;
        false ->
            get_previous_events(CurrentEvent, Rest)
    end.

-spec get_next_events(CurrentEvent, EventTable) -> [event:event_atom()]
    when
    CurrentEvent :: event_table_parser:event_atom(),
    EventTable :: event_table_parser:event_table().
get_next_events(_, []) ->
    [];
get_next_events(CurrentEvent, [{Event, _, NextEvents} | Rest]) ->
    case CurrentEvent =:= Event of
        true ->
            NextEvents;
        false ->
            get_next_events(CurrentEvent, Rest)
    end.

-spec generate_missing_event(NextEvent,PreviousEvent, EventTable) -> [event:event_atom()]
    when
    NextEvent :: event_table_parser:event_atom(),
    PreviousEvent :: event_table_parser:event_atom(),
    EventTable :: event_table_parser:event_table().
generate_missing_event(NextEvent,PreviousEvent, EventTable)->

    PossibleCurrentEventList1 = get_previous_events(NextEvent, EventTable),
    PossibleCurrentEventList2 = get_next_events(PreviousEvent, EventTable),

    case {PreviousEvent, NextEvent} of {_,_} ->

        Intersection = ordsets:intersection(ordsets:from_list(PossibleCurrentEventList1), ordsets:from_list(PossibleCurrentEventList2)),
        case length(Intersection) of
            1->
                {ok,hd(Intersection)};
            0 ->
                undefined;
            _ -> 
                {error,"Error: More than one event in common between previous and next events."}
        end;

        _ ->
            {error, "Something went wrong."}

    end.
%% TESTS

test(PreviousEvent, NextEvent, FileName) ->
    case event_table_parser:parse_table(FileName) of
        {ok, Table} -> 
            io:format("Table is ~p~n", [Table]),
            % PreviousEvents = get_previous_events(CurrentEvent, Table),
            % io:format("Previous events of ~p are ~p~n", [CurrentEvent, PreviousEvents]);
            case generate_missing_event(NextEvent,PreviousEvent, Table) of
                undefined ->
                    io:format("No missing event found.~n");
                {ok, MissingEvent} ->
                    io:format("Missing event is ~p~n", [MissingEvent]);
                {error, Error} ->
                    io:format("Error: ~p~n", [Error])
            end;
        {error, Error} -> 
            io:format("Error: ~p~n", [Error])
    end.