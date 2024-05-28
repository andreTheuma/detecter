%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Intermediate to Erlang Virtual Machine trace event translation.
%%%
%%% @end
%%% 
%%% Copyright (c) 2021, Duncan Paul Attard <duncanatt@gmail.com>
%%%
%%% This program is free software: you can redistribute it and/or modify it 
%%% under the terms of the GNU General Public License as published by the Free 
%%% Software Foundation, either version 3 of the License, or (at your option) 
%%% any later version.
%%%
%%% This program is distributed in the hope that it will be useful, but WITHOUT 
%%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
%%% FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
%%% more details.
%%%
%%% You should have received a copy of the GNU General Public License along with 
%%% this program. If not, see <https://www.gnu.org/licenses/>.
%%% ----------------------------------------------------------------------------
-module(event).
-author("Duncan Paul Attard").

-include("../include/event.hrl").

%%% Public API.
-export([to_evm_event/1, to_int_event/1]).

%%% Types.
-export_type([int_event/0, evm_event/0,evm_event_atom/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------
-type int_event_atom() ::
  fork | init | exit | send | recv.

-type evm_event_atom() ::
  spawn | spawned | exit | send | 'receive'.

-type int_event() ::
{fork, Parent :: pid(), Child :: pid(), Mfa :: mfa()} |
{init, Child :: pid(), Parent :: pid(), Mfa :: mfa()} |
{exit, Process :: pid(), Reason :: term()} |
{send, Sender :: pid(), Receiver :: pid(), Message :: term()} |
{recv, Receiver :: pid(), Message :: term()}.
%% Intermediate trace event format agnostic of the tracer implementation. See
%% {@link evm_event/0} for trace events specific to the Erlang Virtual Machine.

-type evm_event() ::
{trace, PidSrc :: pid(), spawn, PidTgt :: pid(), Mfa :: mfa()} |
{trace, PidSrc :: pid(), spawned, PidTgt :: pid(), Mfa :: mfa()} |
{trace, PidSrc :: pid(), exit, Reason :: term()} |
{trace, PidSrc :: pid(), send, Msg :: term(), PidTgt :: pid()} |
{trace, PidSrc :: pid(), 'receive', Msg :: term()}.
%% Trace event format issued by the Erlang Virtual Machine tracing
%% infrastructure. See {@link erlang:trace/3} for more information.

-type corrupt_event() ::
{corrupt_payload, PidSrc :: pid(), Msg :: term()} |
{corrupt_payload, PidSrc :: pid(), PidTgt :: pid(), Msg :: term()}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Translates the trace event in intermediate representation to Erlang
%% Virtual Machine tracer equivalent.
%%
%% {@params
%%   {@name Event}
%%   {@desc Intermediate trace event representation to translate.}
%% }
%%
%% {@returns Translated event.}
-spec to_evm_event(Event :: int_event()) -> evm_event().
to_evm_event({fork, Parent, Child, Mfa}) ->
  {trace, Parent, spawn, Child, Mfa};
to_evm_event({init, Child, Parent, Mfa}) ->
  {trace, Child, spawned, Parent, Mfa};
to_evm_event({exit, Process, Reason}) ->
  {trace, Process, exit, Reason};
to_evm_event({send, Sender, Receiver, Msg}) ->
  {trace, Sender, send, Msg, Receiver};
to_evm_event({recv, Receiver, Msg}) ->
  {trace, Receiver, 'receive', Msg}.

to_int_event({trace, Parent, spawn, Child, Mfa}) ->
  {fork, Parent, Child, Mfa};
to_int_event({trace, Child, spawned, Parent, Mfa}) ->
  {init, Child, Parent, Mfa};
to_int_event({trace, Process, exit, Reason}) ->
  {exit, Process, Reason};
to_int_event({trace, Sender, send, Msg, Receiver}) ->
  {send, Sender, Receiver, Msg};
to_int_event({trace, Receiver, 'receive', Msg}) ->
  {recv, Receiver, Msg}.