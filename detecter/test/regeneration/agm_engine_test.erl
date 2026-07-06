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
    FirstContinuation = fun(_Envelope) -> first_callback end,
    DuplicateContinuation = fun(_Envelope) -> duplicate_callback end,
    ReductionFun = fun
        ({literal, 0}) ->
            {ok, [{{continue, state2, [5]}, FirstContinuation}]};
        ({literal, 1}) ->
            {ok, [{{continue, state2, [5]}, DuplicateContinuation}]}
    end,
    ?assertMatch(
        {ok, #{
            consequence := {continue, state2, [5]},
            event := unknown,
            continuation := FirstContinuation
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
            {ok, [{{verdict, no}, fun(_Envelope) -> no end}]};
        ({literal, 1}) ->
            {ok, [
                {{continue, state2, [0]}, fun(_Envelope) -> continue end}
            ]}
    end,
    ?assertEqual(
        {withhold, ambiguous_consequence},
        agm_engine:resolve_monitoring_consequence(Recovery, ReductionFun)
    ).

numeric_bound_values_are_compared_exactly_test() ->
    Recovery = #{
        source_state => s0,
        inferred_state => s1,
        event_specs => [{literal, 0}, {literal, 1}]
    },
    ReductionFun = fun
        ({literal, 0}) ->
            {ok, [{{continue, state2, [1]}, fun(_Envelope) -> int end}]};
        ({literal, 1}) ->
            {ok, [{{continue, state2, [1.0]}, fun(_Envelope) -> float end}]}
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
    Reduction = fun(_Envelope) -> no end,
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

envelope_callback_is_returned_without_invocation_test() ->
    TestPid = self(),
    LookaheadEnvelope = {{trace, source, send, 9, destination}, TestPid},
    Continuation = fun(Envelope) ->
        TestPid ! {callback_invoked, Envelope},
        replayed
    end,
    Recovery = #{
        source_state => s0,
        inferred_state => s1,
        event_specs => [{literal, 0}]
    },
    {ok, Consequence} = agm_engine:resolve_monitoring_consequence(
        Recovery,
        fun({literal, 0}) ->
            {ok, [{{continue, state2, [5]}, Continuation}]}
        end
    ),
    ?assertEqual(
        {arity, 1},
        erlang:fun_info(maps:get(continuation, Consequence), arity)
    ),
    receive
        {callback_invoked, _} ->
            ?assert(false)
    after 0 ->
        ok
    end,
    ?assertEqual(
        replayed,
        (maps:get(continuation, Consequence))(LookaheadEnvelope)
    ),
    ?assertEqual(
        {callback_invoked, LookaheadEnvelope},
        receive Message -> Message after 0 -> callback_not_invoked end
    ).

reduction_type_is_envelope_aware_test() ->
    BeamPath = code:which(agm_engine),
    SourcePath = filename:join([
        filename:dirname(filename:dirname(BeamPath)),
        "src",
        "regeneration",
        "agm_engine.erl"
    ]),
    {ok, Source} = file:read_file(SourcePath),
    ?assertNotEqual(
        nomatch,
        binary:match(Source, <<"fun((term()) -> term())">>)
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
