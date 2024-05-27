-module(regen_eval).
-author("André Theuma").

% -ifdef(TEST).
-export([get_previous_events/2, generate_current_event_list/3]).
% -endif.

-include("log.hrl").
-include("event.hrl").

-export([generate_missing_event/3]).

%% @doc Returns the previous possible actions of a given event and a system information table.
-spec get_previous_events(CurrentEvent, EventTable) -> [event:int_event_atom()] when
    CurrentEvent :: event:evm_event(),
    EventTable :: event_table_parser:event_table().
get_previous_events(_, []) ->
    [];
get_previous_events(CurrentEvent, [{Event, PreviousEvents, _} | Rest]) ->
    CurrentEventAction = atom_to_list(element(3, CurrentEvent)),
    case CurrentEventAction =:= Event of
        true ->
            lists:map(fun(X) -> list_to_existing_atom(X) end, PreviousEvents);
        false ->
            get_previous_events(CurrentEvent, Rest)
    end.

%% @doc Returns the next events of a given event and a system information table.
-spec get_next_events(CurrentEvent, EventTable) -> [event:int_event_atom()] when
    CurrentEvent :: event:evm_event(),
    EventTable :: event_table_parser:event_table().
get_next_events(_, []) ->
    [];
get_next_events(CurrentEvent, [{Event, _, NextEvents} | Rest]) ->
    CurrentEventAction = atom_to_list(element(3, CurrentEvent)),
    case CurrentEventAction =:= Event of
        true ->
            lists:map(fun(X) -> list_to_existing_atom(X) end, NextEvents);
        false ->
            get_next_events(CurrentEvent, Rest)
    end.

-spec generate_current_event_list(PreviousEvent, MissingEventPayload, EventTable) -> EventList when
    PreviousEvent :: event:evm_event(),
    MissingEventPayload :: event:corrupt_event(),
    EventTable :: event_table_parser:event_table(),
    EventList :: [event:int_event()].
generate_current_event_list(PreviousEvent, MissingEventPayload, EventTable) ->
    PossibleActions = get_next_events(PreviousEvent, EventTable),
    case MissingEventPayload of
        {corrupt_payload, Pid, Item} ->
            lists:map(fun(Action) -> {Action, Pid, Item} end, PossibleActions)
    end.

-spec generate_missing_event(PreviousEventList, CurrentEvent, EventTable) ->
    {ok, Event} | {error, string()}
when
    PreviousEventList :: [event:int_event()],
    CurrentEvent :: event:evm_event(),
    Event :: event:evm_event(),
    EventTable :: event_table_parser:event_table().
generate_missing_event(PreviousEventList, CurrentEvent, EventTable) ->
    PreviousEventActionsFromCurrentEvent = get_previous_events(CurrentEvent, EventTable),

    case PreviousEventActionsFromCurrentEvent of
        ?EMPTY_EVENT ->
            {error, "Error: No previous events found."};
        _ ->
            GeneratedEventActionList = ordsets:intersection(
                ordsets:from_list(PreviousEventActionsFromCurrentEvent),
                ordsets:from_list(lists:map(fun(X) -> element(1, X) end, PreviousEventList))
            ),
            case length(GeneratedEventActionList) of
                1 -> 
                    event:to_evm_event(hd([GeneratedEvent || GeneratedEvent <- PreviousEventList, element(1, GeneratedEvent) =:= hd(GeneratedEventActionList)]));
                0 ->
                    {error, "Error: No common event found."};
                _ ->
                    {error, "Error: More than one common event found."}
            end
    end.
