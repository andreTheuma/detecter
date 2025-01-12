%%% ----------------------------------------------------------------------------
%%% @author André Theuma
%%%
%%% @doc Module to parse prior system information from a ".spec" file
%%%
%%% @end
%%% ----------------------------------------------------------------------------

-module(sys_info_parser).
-author("André Theuma").

-export([parse_file/1]).

parse_file(FilePath) ->
    {ok, Binary} = file:read_file(FilePath),
    Lines = binary:split(Binary, <<"\n">>, [global, trim]),
    lists:map(fun parse_transition_line/1, Lines).

parse_transition_line(Line) ->
    Trimmed = string:trim(string:trim(string:trim(Line, trailing, ";"), leading, "{"), trailing, "}"),
    [Source, Event, Destination] = string:split(Trimmed, ",", all),
    {
        parse_atom(Source),
        parse_event(Event),
        parse_atom(Destination)
    }.

parse_atom(StateText) ->
    State = string:trim(StateText),
    try list_to_atom(binary_to_list(State)) of
        Atom -> 
                Atom
    catch
        _:_ -> State
    end.

parse_event(EventText) ->
    EventTrimmed = string:trim(EventText),

    case parse_event_and_guard(EventTrimmed) of
        {ok, Parsed} -> 
                Parsed;
            {error,Reason} ->
                  try list_to_integer(binary_to_list(EventTrimmed)) of
                    Int -> 
                            {is_integer, Int}
                    catch
                        _:_ -> 
                            Atom = list_to_atom(binary_to_list(EventTrimmed)),
                            {atom, Atom}
                    end
                end.

parse_event_and_guard(Event)->
    [BaseEvent | Guard] = binary:split(Event, <<" ">>, [global]),

    case identify_event_condition(BaseEvent) of
        {ok, BaseCondition} ->
            {ok, {BaseCondition, parse_guard(Guard)}};
        _ ->{error, error}
    end.

%%% This is used to parse complex events, such as numerical ranges, and other conditions
%%% 
%%% ex: event N in {s2, N, s1};
%%%     event Z \ 0 in {s3, Z \ 0, s3};
%%% 
%%% 
identify_event_condition(Event) ->
    case Event of
        <<"NULL">> -> 
                {ok, {'fun', null}};
        <<"N">> ->
            {ok, {'fun', is_natural_integer}};
        <<"Z">> ->
            {ok, {'fun', is_any_integer}};
        <<"R">> ->
            {ok, {'fun', is_real_number}};
        _-> 
            {error, unsupported_condition}
    end.

parse_guard([]) -> [];
parse_guard([Operator | [Payload]]) ->
    case parse_operator(Operator) of
        {ok, ParsedOp} ->
            [ParsedOp | parse_payload(binary_to_list(Payload))];
        _ -> error
    end.

parse_operator(Guard) ->
    case binary_to_list(Guard) of
        %TODO: ADD MORE CASES... union, disjunction....
        "\\" ->
            {ok, setminus}
    end.


parse_payload(Payload) ->
    try list_to_integer(Payload) of
        Int -> 
                {is_integer, Int}
        catch
            %TODO: ADD CLAUSE TO SUPPORT STRING... WHEN DETECTING STRING IN CONDITION DO NOT PARSE AS ATOM BUT AS STRING
            _:_ -> 
                Atom = list_to_atom(Payload),
                {is_atom, Atom}
        end.

        