-module(agm_engine_test).

-include_lib("eunit/include/eunit.hrl").

unique_recovery_test() ->
    Transitions = unique_transitions(),
    ?assertEqual(
        {ok, #{
            source_state => s1,
            inferred_state => s2,
            event_specs => [{literal, 0}]
        }},
        agm_engine:recover_missing_event(Transitions, s1, 9)
    ).

ambiguous_state_withholds_test() ->
    Transitions = [
        transition(s0, s1, {literal, 0}),
        transition(s0, s2, {literal, 1}),
        transition(s1, s3, {literal, 9}),
        transition(s2, s4, {literal, 9})
    ],
    ?assertEqual(
        {withhold, ambiguous_state},
        agm_engine:recover_missing_event(Transitions, s0, 9)
    ).

impossible_recovery_withholds_test() ->
    ?assertEqual(
        {withhold, impossible_recovery},
        agm_engine:recover_missing_event(unique_transitions(), s0, 99)
    ).

equal_consequences_proceed_without_exact_event_test() ->
    Recovery = #{
        source_state => s0,
        inferred_state => s1,
        event_specs => [{literal, 0}, {literal, 1}]
    },
    Continuation = fun() -> should_not_be_called end,
    ReductionFun = fun
        ({literal, Event}) when Event =:= 0; Event =:= 1 ->
            {ok, [{{continue, state2, [5]}, Continuation}]}
    end,
    ?assertMatch(
        {ok, #{
            consequence := {continue, state2, [5]},
            event := unknown,
            continuation := Continuation
        }},
        agm_engine:resolve_monitoring_consequence(Recovery, ReductionFun)
    ).

conflicting_consequences_withhold_test() ->
    Recovery = #{
        source_state => s0,
        inferred_state => s1,
        event_specs => [{literal, 0}, {literal, 1}]
    },
    ReductionFun = fun
        ({literal, 0}) ->
            {ok, [{{verdict, no}, fun() -> no end}]};
        ({literal, 1}) ->
            {ok, [{{continue, state2, [0]}, fun() -> continue end}]}
    end,
    ?assertEqual(
        {withhold, ambiguous_consequence},
        agm_engine:resolve_monitoring_consequence(Recovery, ReductionFun)
    ).

unknown_symbolic_proof_withholds_test() ->
    Recovery = #{
        source_state => s0,
        inferred_state => s1,
        event_specs => [{symbolic, natural_integer}]
    },
    ?assertEqual(
        {withhold, unproven_consequence},
        agm_engine:resolve_monitoring_consequence(
            Recovery,
            fun(_) -> {unknown, unsupported_symbolic_guard} end
        )
    ).

exact_literal_is_optional_metadata_test() ->
    Reduction = fun() -> no end,
    Recovery = #{
        source_state => s0,
        inferred_state => s1,
        event_specs => [{literal, 0}]
    },
    ?assertMatch(
        {ok, #{
            consequence := {verdict, no},
            event := {known, 0},
            continuation := Reduction
        }},
        agm_engine:resolve_monitoring_consequence(
            Recovery,
            fun({literal, 0}) ->
                {ok, [{{verdict, no}, Reduction}]}
            end
        )
    ).

event_spec_membership_test_() ->
    [
        ?_assert(agm_engine:event_spec_membership({literal, ok}, ok)),
        ?_assertNot(agm_engine:event_spec_membership({literal, ok}, error)),
        ?_assert(agm_engine:event_spec_membership(
            {symbolic, natural_integer},
            1
        )),
        ?_assertNot(agm_engine:event_spec_membership(
            {symbolic, natural_integer},
            0
        )),
        ?_assert(agm_engine:event_spec_membership(
            {symbolic, any_integer_except, 4},
            5
        )),
        ?_assertEqual(
            unknown,
            agm_engine:event_spec_membership({symbolic, unsupported}, 5)
        )
    ].
unique_transitions() ->
    [
        transition(s1, s2, {literal, 0}),
        transition(s2, s3, {literal, 9})
    ].

transition(Source, Destination, EventSpec = {literal, Event}) ->
    {
        Source,
        Destination,
        EventSpec,
        fun(Value) -> Value =:= Event end
    }.
