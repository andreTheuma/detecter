-module(lin_eval).
-author("André Theuma").

% %%% Includes.
% -include_lib("stdlib/include/assert.hrl").
% -include("log.hrl").

% %%% Public API.
% -export([eval_string/2]).


% %%% ----------------------------------------------------------------------------
% %%% Type definitions.
% %%% ----------------------------------------------------------------------------

% -type line_num() :: pos_integer().
% %% Line number of parsed token in source log file.

% -type ev_atom() :: {atom, line_num(), atom()}.
% %% Atom token.

% -type ev_int() :: {int, line_num(), integer()}.
% %% Integer token.

% -type ev_float() :: {float, line_num(), float()}.
% %% Float token.

% -type ev_pid() :: {pid, line_num(), pid()}.
% %% PID token.

% -type ev_string() :: {string, line_num(), string()}.
% %% String token.

% -type ev_list() :: nil | {cons, ev_term(), nil} | {cons, ev_term(), ev_list()}.
% %% List AST node.

% -type ev_tuple() :: {tuple, [ev_term()]}.
% %% Tuple AST node.

% -type ev_term() :: ev_atom() | ev_int() | ev_float() | ev_pid() |
% ev_string() | ev_list() | ev_tuple().
% %% Term AST node.

% -type ev_mfa() :: {mfa, ev_atom(), ev_atom(), ev_list()}.
% %% MFA AST node.

% -type ev_fork() :: {fork, ev_pid(), ev_pid(), ev_mfa()}.
% %% Fork trace event AST node.

% -type ev_init() :: {init, ev_pid(), ev_pid(), ev_mfa()}.
% %% Init trace event AST node.

% -type ev_exit() :: {exit, ev_pid(), ev_atom()}.
% %% Exit trace event AST node.

% -type ev_send() :: {send, ev_pid(), ev_pid(), ev_term()}.
% %% Send trace event AST node.

% -type ev_recv() :: {recv, ev_pid(), ev_term()}.
% %% Receive trace event AST node.

% -type ev_event() :: ev_fork() | ev_init() | ev_exit() | ev_send() | ev_recv().
% %% Trace event AST node.

% -type ev_delay() :: {delay, Ms :: ev_int(), Event :: ev_event()}.


% %%% ----------------------------------------------------------------------------
% %%% Public API.
% %%% ----------------------------------------------------------------------------

% %% @doc Parses the specified trace event description into its intermediate
% %% representation in Erlang format.
% %%
% %% {@params
% %%   {@name String}
% %%   {@desc Trace event description string. See {@section Trace events}
% %%          for the trace event string format.
% %%   }
% %%   {@name LineNum}
% %%   {@desc Line number of the trace event description in source log file.}
% %% }
% %%
% %% {@returns Intermediate trace event representation in Erlang format.}
% -spec eval_string(String, LineNum) ->
%   {ok, skip} | {ok, log_tracer:event()} | no_return()
%   when
%   String :: string(),
%   LineNum :: line_num().
% eval_string(String, LineNum) when is_list(String) ->
%   case log_lexer:string(String) of
%     {ok, [], _} ->
%       {ok, skip};
%     {ok, Tokens, _} ->
%       case log_parser:parse(Tokens) of
%         {ok, Ast} ->
%           {ok, eval_delay(Ast)};
%         {error, {_, _, ErrorDesc}} ->
%           error({error, LineNum, log_parser:format_error(ErrorDesc)})
%       end;
%     {error, {_, _, ErrorDesc}, _} ->
%       error({error, LineNum, log_lexer:format_error(ErrorDesc)})
%   end.


% %%% ----------------------------------------------------------------------------
% %%% Private helper functions.
% %%% ----------------------------------------------------------------------------

% %% @private Evaluates the delayed trace event AST node.
% %%
% %% {@params
% %%   {@name Delay}
% %%   {@desc Delayed trace event AST node to evaluate.}
% %% }
% %%
% %% {@returns Intermediate trace event representation in Erlang format.}
% -spec eval_delay(Delay :: ev_delay() | ev_event()) -> log_tracer:event().
% eval_delay({delay, {int, _, Ms}, Event}) ->
%   {delay, Ms, eval_event(Event)};
% eval_delay(Event) ->
%   {delay, 0, eval_event(Event)}.

% %% @private Evaluates the trace event AST node.
% %%
% %%
% %% {@params
% %%   {@name Event}
% %%   {@desc Trace event AST node to evaluate.}
% %% }
% %%
% %% {@returns Intermediate trace event representation in Erlang format.}
% -spec eval_event(Event :: ev_event()) -> event:int_event().
% eval_event({fork, {pid, _, Pid}, {pid, _, Pid2}, Mfa}) ->
%   {fork, Pid, Pid2, eval_mfa(Mfa)};
% eval_event({init, {pid, _, Pid}, {pid, _, Pid2}, Mfa}) ->
%   {init, Pid, Pid2, eval_mfa(Mfa)};
% eval_event({exit, {pid, _, Pid}, {atom, _, Reason}}) ->
%   {exit, Pid, Reason};
% eval_event({send, {pid, _, Pid}, {pid, _, Pid2}, Item}) ->
%   {send, Pid, Pid2, eval_term(Item)};
% eval_event({recv, {pid, _, Pid}, Item}) ->
%   {recv, Pid, eval_term(Item)}.

% %% @private Evaluates the MFA AST node.
% %%
% %% {@params
% %%   {@name Mfa}
% %%   {@desc MFA AST node to evaluate.}
% %% }
% %%
% %% {@returns Intermediate MFA representation in Erlang format.}
% -spec eval_mfa(Mfa :: ev_mfa()) -> mfa().
% eval_mfa({mfa, {atom, _, M}, {atom, _, F}, Args}) ->
%   {M, F, eval_term(Args)}.


% %% @private Evaluates the term AST node.
% %%
% %% {@params
% %%   {@name Term}
% %%   {@desc Term AST node to evaluate.}
% %% }
% %%
% %% {@returns Simple or complex Erlang data type.}
% -spec eval_term(Term :: ev_term()) ->
%   pid() | atom() | integer() | float() | list() | tuple().
% eval_term({pid, _, Pid}) ->
%   Pid;
% eval_term({ref, _, Pid}) ->
%   Pid;
% eval_term({atom, _, Atom}) ->
%   Atom;
% eval_term({int, _, Int}) ->
%   Int;
% eval_term({float, _, Float}) ->
%   Float;
% eval_term({string, _, String}) ->
%   String;
% eval_term(nil) ->
%   [];
% eval_term({cons, Head, Tail}) ->
%   [eval_term(Head) | eval_term(Tail)];
% eval_term({tuple, []}) ->
%   {};
% eval_term({tuple, Terms}) ->
%   list_to_tuple(conv_list(Terms)).

% %% @private Evaluates a list of term AST nodes.
% %%
% %% {@params
% %%   {@name Terms}
% %%   {@desc List of term AST nodes.}
% %% }
% %%
% %% {@returns List of simple or complex Erlang data types.}
% -spec conv_list(Terms :: list(ev_delay())) -> list().
% conv_list([]) ->
%   [];
% conv_list([Term | Terms]) ->
%   [eval_term(Term) | conv_list(Terms)].