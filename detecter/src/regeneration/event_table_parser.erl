-module(event_table_parser).
-author("André Theuma").

-export([parse_table/1]).
-export([string_tokenizer/1]).
-export([file_format_checker/1]).

-export_type([event_table/0, event_atom/0]).

-import(event, [evm_event/0]).

-type event_table() :: list(event_relation()).
-type event_relation() :: {event_atom(), event_atom(), event_atom()}.
-type event_atom() :: atom().

%% @doc The entry function to parse the event table.
-spec parse_table(Filename) -> {ok, EventTable} when
    Filename :: string(),
    EventTable :: event_table().
parse_table(Filename) ->
    case file:open(Filename, [read]) of
        {ok, _} ->
            {ok, File} = file:read_file(Filename),
            Lines = string:tokens(binary_to_list(File), "\n"),
            case file_format_checker(Lines) of
                ok ->
                    EventTable = lists:map(fun string_tokenizer/1, Lines),
                    {ok, EventTable};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% @doc Tokenizes a string into a list of strings. The string takes the form of
%% <current_event>,[{previous_events},{next_events}] where <current_event> is the
%% current event, {previous_events} is a list of previous events in the form of {event1,event2,...} and {next_events} is a list of next events in the form of {event1,event2,...}
-spec string_tokenizer(Line) -> [event_relation()] when Line :: string().
string_tokenizer(Line) ->
    % Clean the line from any whitespace
    CleanLine = re:replace(Line, "\\s+", "", [global, {return, list}]),

    % Split the line into the current event and the rest of the events
    [CurrentEvent | Rest] = string:lexemes(CleanLine, "|"),
    % Clean the rest of the events from { and }
    CleanRest = re:replace(Rest, "[{}]", "", [global, {return, list}]),

    % Split the rest of the events into the previous and next events
    [PreviousEventsStrBin | NextEventsStrBin] = re:split(CleanRest, ";", [{parts, 2}]),

    PreviousEvents = string:tokens(binary_to_list(PreviousEventsStrBin), ","),
    NextEvents = string:tokens(binary_to_list(hd(lists:flatten(NextEventsStrBin))), ","),

    {CurrentEvent, PreviousEvents, NextEvents}.

-spec file_format_checker(Lines) -> [ok | {error, string()}] when Lines :: [string()].
file_format_checker(Lines) ->
    RegEx = "^[A-Za-z0-9]+ \\| \\{[^}]*\\}; \\{[^}]*\\}$",
    case
        lists:all(
            fun(Line) ->
                % if line starts with a comment, ignore it
                case re:run(Line, "^\\s*%") of
                    nomatch -> ok;
                    _ -> return
                end,

                case re:run(Line, RegEx) of
                    nomatch ->
                        io:format("Line ~p does not match the format.~n", [Line]),
                        false;
                    {match, _} ->
                        true
                end
            end,
            Lines
        )
    of
        true ->
            ok;
        false ->
            {error,
                "Invalid format. The format should be <current_event> | {<previous_events>}; {<next_events>"}
    end.
