-module(outline_simulator).
-author("André Theuma").

%%% Includes.
-include("log.hrl").

-export([post_event_to_log/2]).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

% -define(TRACE_FILE, "../../priv/trace_test_4.log").

-spec post_event_to_log(Filepath,Event) -> ok
    when Event :: event:int_event(),
            Filepath :: string().
post_event_to_log(Filepath,Event) ->

    % if file does not exist, create it
    case filelib:is_regular(Filepath) of
        false -> file:write_file(Filepath, "");
        true -> ok
    end,

    % open the file in append mode
    {ok, File} = file:open(Filepath, [append]),
    LineSep = io_lib:nl(),

    % convert the atom to a string 
    EventString = atom_to_list(Event) ++ LineSep,

    file:write(File, EventString),
    file:close(File).