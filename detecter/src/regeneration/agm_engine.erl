%%% ----------------------------------------------------------------------------
%%% @doc Pure Automaton-Guided Monitoring regeneration operations.
%%% ----------------------------------------------------------------------------
-module(agm_engine).
-author("André Theuma").

-export([
    get_system_states/1,
    reachable_state/3,
    reachable_states_from_state/2,
    preceding_states_from_state/2,
    preceding_states_from_event/2,
    resolve_singleton_state/1,
    candidate_event_specs/3,
    recover_missing_event/3,
    resolve_monitoring_consequence/2,
    event_spec_membership/2
]).

-type state() :: term().
-type event_spec() ::
    {literal, term()} |
    {symbolic, natural_integer | any_integer | real_number} |
    {symbolic, any_integer_except, integer()}.
-type transition() ::
    {state(), state(), event_spec(), fun((term()) -> boolean())}.
-type recovery() :: #{
    source_state := state(),
    inferred_state := state(),
    event_specs := [event_spec()]
}.
-type consequence_signature() ::
    {verdict, yes | no} |
    {continue, atom(), [term()]}.
-type reduction() ::
    {consequence_signature(), fun((term()) -> term())}.

-spec get_system_states([transition()]) -> [state()].
get_system_states(Transitions) ->
    lists:usort(
        lists:flatmap(
            fun({Source, Destination, _EventSpec, _Condition}) ->
                [Source, Destination]
            end,
            Transitions
        )
    ).

-spec reachable_state([transition()], state(), term()) -> state() | [].
reachable_state(Transitions, State, Event) ->
    lists:foldl(
        fun({Source, Destination, _EventSpec, Condition}, Acc) ->
            case Source =:= State andalso Condition(Event) of
                true -> Destination;
                false -> Acc
            end
        end,
        [],
        Transitions
    ).

-spec reachable_states_from_state([transition()], state()) -> [state()].
reachable_states_from_state(Transitions, State) ->
    lists:usort([
        Destination
        || {Source, Destination, _EventSpec, _Condition} <- Transitions,
           Source =:= State
    ]).

-spec preceding_states_from_state([transition()], state()) -> [state()].
preceding_states_from_state(Transitions, State) ->
    lists:usort([
        Source
        || {Source, Destination, _EventSpec, _Condition} <- Transitions,
           Destination =:= State
    ]).

-spec preceding_states_from_event([transition()], term()) -> [state()].
preceding_states_from_event(Transitions, Event) ->
    [
        State
        || State <- get_system_states(Transitions),
           reachable_state(Transitions, State, Event) =/= []
    ].

-spec resolve_singleton_state([state()]) ->
    {ok, state()} |
    {withhold, impossible_recovery | ambiguous_state}.
resolve_singleton_state([State]) ->
    {ok, State};
resolve_singleton_state([]) ->
    {withhold, impossible_recovery};
resolve_singleton_state(_) ->
    {withhold, ambiguous_state}.

-spec candidate_event_specs([transition()], state(), state()) -> [event_spec()].
candidate_event_specs(Transitions, SourceState, DestinationState) ->
    lists:usort([
        EventSpec
        || {Source, Destination, EventSpec, _Condition} <- Transitions,
           Source =:= SourceState,
           Destination =:= DestinationState
    ]).

-spec recover_missing_event([transition()], state(), term()) ->
    {ok, recovery()} |
    {withhold, impossible_recovery | ambiguous_state}.
recover_missing_event(Transitions, SourceState, NextEvent) ->
    CompatibleStates = preceding_states_from_event(Transitions, NextEvent),
    ReachableStates = reachable_states_from_state(Transitions, SourceState),
    CandidateStates = lists:usort(
        sets:to_list(
            sets:intersection(
                sets:from_list(ReachableStates),
                sets:from_list(CompatibleStates)
            )
        )
    ),
    case resolve_singleton_state(CandidateStates) of
        {ok, InferredState} ->
            EventSpecs = candidate_event_specs(
                Transitions,
                SourceState,
                InferredState
            ),
            case EventSpecs of
                [] ->
                    {withhold, impossible_recovery};
                _ ->
                    {ok, #{
                        source_state => SourceState,
                        inferred_state => InferredState,
                        event_specs => EventSpecs
                    }}
            end;
        Withhold ->
            Withhold
    end.

-spec resolve_monitoring_consequence(
    recovery(),
    fun((event_spec()) ->
        {ok, [reduction()]} |
        {unknown, term()}
    )
) ->
    {ok, #{
        consequence := consequence_signature(),
        event := {known, term()} | unknown,
        continuation := fun((term()) -> term())
    }} |
    {withhold,
        impossible_recovery |
        ambiguous_consequence |
        unproven_consequence}.
resolve_monitoring_consequence(Recovery, ReductionFun) ->
    EventSpecs = maps:get(event_specs, Recovery),
    case collect_monitoring_consequences(EventSpecs, ReductionFun, []) of
        {ok, []} ->
            {withhold, impossible_recovery};
        {ok, [{Signature, Continuation}]} ->
            {ok, #{
                consequence => Signature,
                event => deduce_event_if_unique(EventSpecs),
                continuation => Continuation
            }};
        {ok, _Consequences} ->
            {withhold, ambiguous_consequence};
        {unknown, _Reason} ->
            {withhold, unproven_consequence}
    end.

-spec event_spec_membership(event_spec(), term()) -> boolean() | unknown.
event_spec_membership({literal, Event}, Value) ->
    Event =:= Value;
event_spec_membership({symbolic, natural_integer}, Value) ->
    is_integer(Value) andalso Value > 0;
event_spec_membership({symbolic, any_integer}, Value) ->
    is_integer(Value);
event_spec_membership({symbolic, real_number}, Value) ->
    is_number(Value);
event_spec_membership({symbolic, any_integer_except, Excluded}, Value) ->
    is_integer(Value) andalso Value =/= Excluded;
event_spec_membership(_, _) ->
    unknown.

collect_monitoring_consequences([], _ReductionFun, Acc) ->
    {ok, Acc};
collect_monitoring_consequences([EventSpec | Rest], ReductionFun, Acc) ->
    case ReductionFun(EventSpec) of
        {ok, Reductions} ->
            collect_monitoring_consequences(
                Rest,
                ReductionFun,
                merge_monitoring_consequences(Reductions, Acc)
            );
        {unknown, Reason} ->
            {unknown, Reason}
    end.

merge_monitoring_consequences([], Acc) ->
    Acc;
merge_monitoring_consequences(
    [Reduction = {Signature, _Continuation} | Rest],
    Acc
) ->
    % lists:keymember/3 compares with ==, which would merge signatures whose
    % bound values differ only in numeric type (1 vs 1.0). Consequence
    % identity requires exactly equal bound values, so compare with =:=.
    Merged = lists:any(
        fun({ExistingSignature, _}) -> ExistingSignature =:= Signature end,
        Acc
    ),
    case Merged of
        true ->
            merge_monitoring_consequences(Rest, Acc);
        false ->
            merge_monitoring_consequences(Rest, [Reduction | Acc])
    end.

deduce_event_if_unique([{literal, Event}]) ->
    {known, Event};
deduce_event_if_unique(_) ->
    unknown.
