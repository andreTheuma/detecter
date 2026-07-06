-module(generated_agm_recovery_test).
-author("Andre Theuma").

-include_lib("eunit/include/eunit.hrl").

%%% ----------------------------------------------------------------------------
%%%
%%% AGM Tests
%%%
%%% ----------------------------------------------------------------------------


generated_agm_recovery_test_() ->
%%    using EUnit test setup fixture here
%%    {setup, SetupFun, CleanupFun, InstantiatorFun}
%%
%%    param "Context" comes from return vals of [setup_suite/0]
%%
    {setup,
        fun setup_suite/0,
        fun cleanup_suite/1,
        fun(Context) ->
            [
                {"exact recovery complete and recovered traces both accept once",
                    fun() -> exact_yes_equivalence(Context) end},
                {"exact recovery complete and recovered traces both reject once",
                    fun() -> exact_no_equivalence(Context) end},
                {"multiple candidates with one consequence match the complete trace",
                    fun() -> multi_candidate_equivalence(Context) end},
                {"irrelevant lookahead preserves equivalent continuing observations",
                    fun() -> continued_no_verdict_equivalence(Context) end},
                {"retained replay is reconsidered after a newer state transition",
                    fun() -> retained_replay_equivalence(Context) end},
                {"recursive and max bridges reach the same receive state",
                    fun() -> recursive_max_continuation_equivalence(Context) end},
                {"unique concrete recovery has one consequence and known event metadata",
                    fun() -> unique_concrete_recovery(Context) end},
                {"recursive max replay retains the envelope and bound arguments",
                    fun() -> recursive_max_replay_retains_envelope(Context) end},
                {"ambiguous state recovery withholds",
                    fun() -> ambiguous_state_recovery(Context) end},
                {"multiple concrete events with one consequence proceed",
                    fun() -> equal_concrete_consequences(Context) end},
                {"multiple concrete events with conflicting consequences withhold",
                    fun() -> conflicting_concrete_consequences(Context) end},
                {"impossible recovery withholds",
                    fun() -> impossible_recovery(Context) end},
                {"uniform symbolic recovery proceeds without exact event metadata",
                    fun() -> uniform_symbolic_consequence(Context) end},
                {"symbolic recovery crossing guard boundaries withholds",
                    fun() -> conflicting_symbolic_consequence(Context) end},
                {"unsupported symbolic guard proof withholds",
                    fun() -> unproven_symbolic_consequence(Context) end},
                {"test-generated monitors expose tagged checkpoints",
                    fun() -> test_checkpoint_is_generated(Context) end},
                {"generated monitors declare the AGM engine dependency",
                    fun() -> explicit_engine_dependency(Context) end}
            ] ++ irrevocability_matrix_tests(Context)
        end}.

irrevocability_matrix_tests(Context) ->
    [
        {
            lists:flatten(
                io_lib:format(
                    "terminal irrevocability: ~p ~p path with ~p extensions",
                    [Expected, Encoding, Timing]
                )
            ),
            fun() ->
                assert_terminal_irrevocability(
                    maps:get(ModuleKey, Context),
                    Expected,
                    Encoding,
                    Timing
                )
            end
        }
        || {ModuleKey, Expected} <- [
            {exact_yes, yes},
            {exact_no, no}
        ],
        Encoding <- [complete, recovered],
        Timing <- [queued, post_verdict]
    ].

assert_terminal_irrevocability(Module, Expected, Encoding, queued) ->
    Messages =
        terminal_encoding(Encoding)
        ++ terminal_extensions(),
    {Result, Verdicts} = run_terminal_path(
        Module,
        composite_send_function(Module),
        9,
        Messages
    ),
    ?assertEqual(Expected, Result),
    ?assertEqual([Expected], Verdicts);
assert_terminal_irrevocability(Module, Expected, Encoding, post_verdict) ->
    with_sus_state(
        s1,
        fun() ->
            {Worker, MonitorRef} = start_monitor_function(
                Module,
                composite_send_function(Module),
                [9],
                terminal_encoding(Encoding)
            ),
            ?assertEqual(Expected, await_terminal_verdict()),
            lists:foreach(
                fun(Message) ->
                    Worker ! materialize_message(Message, Worker, self())
                end,
                terminal_extensions()
            ),
            ?assertEqual(Expected, await_worker_result(Worker, MonitorRef)),
            ?assertEqual([Expected], [Expected | collect_verdicts([])])
        end
    ).

terminal_encoding(Encoding) ->
    {Complete, Recovered} = equivalent_encodings(0, 9, []),
    case Encoding of
        complete -> Complete;
        recovered -> Recovered
    end.

terminal_extensions() ->
    [
        {trace, send, 0},
        {trace, send, 9}
    ].

exact_yes_equivalence(Context) ->
    Module = maps:get(exact_yes, Context),
    {Complete, Recovered} = equivalent_encodings(0, 9, []),
    ?assertEqual(
        {yes, [yes]},
        run_terminal_path(Module, composite_send_function(Module), 9, Complete)
    ),
    ?assertEqual(
        {yes, [yes]},
        run_terminal_path(Module, composite_send_function(Module), 9, Recovered)
    ).

exact_no_equivalence(Context) ->
    Module = maps:get(exact_no, Context),
    {Complete, Recovered} = equivalent_encodings(0, 9, []),
    ?assertEqual(
        {no, [no]},
        run_terminal_path(Module, composite_send_function(Module), 9, Complete)
    ),
    ?assertEqual(
        {no, [no]},
        run_terminal_path(Module, composite_send_function(Module), 9, Recovered)
    ).

multi_candidate_equivalence(Context) ->
    Module = maps:get(multi_no, Context),
    {Complete, Recovered} = equivalent_encodings(0, 9, []),
    ?assertEqual(
        {no, [no]},
        run_terminal_path(Module, composite_send_function(Module), 9, Complete)
    ),
    ?assertEqual(
        {no, [no]},
        run_terminal_path(Module, composite_send_function(Module), 9, Recovered)
    ),
    Recovery = with_sus_state(
        s1,
        fun() ->
            {ok, RecoveryMap, _Envelope} = recover_missing_event(Module, 9),
            ?assertEqual(
                [{literal, 0}, {literal, 1}],
                maps:get(event_specs, RecoveryMap)
            ),
            RecoveryMap
        end
    ),
    ReductionFun = fun
        ({literal, Event}) when Event =:= 0; Event =:= 1 ->
            {ok, [{{continue, x, [9, self()]}, fun(_Envelope) -> no end}]};
        (_) ->
            {unknown, unexpected_event_spec}
    end,
    ?assertMatch(
        {ok, #{
            consequence := {continue, x, [9, _]},
            event := unknown
        }},
        Module:resolve_monitoring_consequence(Recovery, ReductionFun)
    ).

continued_no_verdict_equivalence(Context) ->
    Module = maps:get(duplicate, Context),
    {Complete, Recovered} = equivalent_encodings(0, 9, []),
    CompleteObservation = run_checkpoint_path(Module, 5, Complete),
    RecoveredObservation = run_checkpoint_path(Module, 5, Recovered),
    ?assertEqual({recv_state, []}, CompleteObservation),
    ?assertEqual(CompleteObservation, RecoveredObservation).

retained_replay_equivalence(Context) ->
    Module = maps:get(duplicate, Context),
    {Complete, Recovered} = equivalent_encodings(0, 9, []),
    Suffix = [{trace, recv, {8, ignored}}],
    CompleteObservation = run_continuing_path(Module, 5, Complete, Suffix),
    RecoveredObservation = run_continuing_path(Module, 5, Recovered, Suffix),
    ?assertEqual(
        {recv_state, [], recv_state, []},
        CompleteObservation
    ),
    ?assertEqual(CompleteObservation, RecoveredObservation).

recursive_max_continuation_equivalence(Context) ->
    Module = maps:get(duplicate, Context),
    {Complete, Recovered} = equivalent_encodings(0, 9, []),
    ?assertEqual(
        {recv_state, []},
        run_checkpoint_path(Module, 5, Complete)
    ),
    ?assertEqual(
        {recv_state, []},
        run_checkpoint_path(Module, 5, Recovered)
    ),
    Source = generated_source(Module),
    ?assertNotEqual(
        nomatch,
        re:run(
            Source,
            <<"\\{continue,\\s*x,\\s*\\[OwnTok,\\s*From\\]\\}">>,
            [dotall, {capture, none}]
        )
    ),
    ?assertNotEqual(
        nomatch,
        re:run(
            Source,
            <<"replay_x\\(PendingEnvelope,\\s*OwnTok,\\s*From\\)">>,
            [dotall, {capture, none}]
        )
    ).

unique_concrete_recovery(Context) ->
    Module = maps:get(unique, Context),
    {Recovery, LookaheadEnvelope} = with_sus_state(
        s1,
        fun() ->
            {Result, OriginalEnvelope} =
                recover_missing_event_with_envelope(Module, 9),
            ?assertMatch(
                {ok, #{
                    source_state := s1,
                    inferred_state := s2,
                    event_specs := [{literal, 0}]
                }, OriginalEnvelope},
                Result
            ),
            ?assertEqual(s1, ets:lookup_element(sus_state, current_state, 2)),
            ?assertEqual(
                undefined,
                ets:lookup_element(sus_state, previous_state, 2)
            ),
            RecoveryMap = element(2, Result),
            ?assert(Module:commit_recovery_state(RecoveryMap)),
            ?assertEqual(s2, ets:lookup_element(sus_state, current_state, 2)),
            ?assertEqual(s1, ets:lookup_element(sus_state, previous_state, 2)),
            {RecoveryMap, element(3, Result)}
        end
    ),
    ReductionFun = fun
        ({literal, 0}) ->
            {ok, [{{verdict, no}, fun(_Envelope) -> no end}]};
        (_) ->
            {unknown, unexpected_event_spec}
    end,
    Resolution = Module:resolve_monitoring_consequence(Recovery, ReductionFun),
    ?assertMatch(
        {ok, #{
            consequence := {verdict, no},
            event := {known, 0}
        }},
        Resolution
    ),
    {ok, ResolutionMap} = Resolution,
    ?assert(is_function(maps:get(continuation, ResolutionMap), 1)),
    ?assertMatch({{trace, _, send, 9, _}, _}, LookaheadEnvelope),
    Source = generated_source(Module),
    ?assertNotEqual(
        nomatch,
        re:run(
            Source,
            <<
                "commit_recovery_state\\(Recovery\\).*"
                "ets:insert\\(sus_state,\\s*"
                "\\[\\{previous_state,\\s*SourceState\\},\\s*"
                "\\{current_state,\\s*InferredState\\}\\]\\)"
            >>,
            [dotall, {capture, none}]
        )
    ),

    with_sus_state(
        s1,
        fun() ->
            {Worker, MonitorRef, _StateFunction} =
                start_monitor_state(Module, 0, [
                    missing_event,
                    {trace, send, 9}
                ]),
            ?assertEqual(no, await_plain_verdict()),
            ?assertEqual(no, await_worker_result(Worker, MonitorRef)),
            ?assertEqual(
                s2,
                ets:lookup_element(sus_state, current_state, 2)
            ),
            ?assertEqual(
                s1,
                ets:lookup_element(sus_state, previous_state, 2)
            )
        end
    ).

recursive_max_replay_retains_envelope(Context) ->
    Module = maps:get(duplicate, Context),
    with_sus_state(
        s1,
        fun() ->
            {Worker, MonitorRef, _StateFunction} =
                start_monitor_state(Module, 5, [
                    missing_event,
                    {trace, send, 9},
                    {trace, recv, {8, ignored}},
                    {trace, send, 5}
                ]),
            ?assertEqual(recv_state, await_checkpoint(Module, Worker)),
            ?assertEqual(s4, ets:lookup_element(sus_state, current_state, 2)),
            ?assertEqual(s3, ets:lookup_element(sus_state, previous_state, 2)),
            assert_no_verdict(),
            stop_worker(Worker, MonitorRef)
        end
    ),
    Source = generated_source(Module),
    ?assertNotEqual(
        nomatch,
        re:run(
            Source,
            <<
                "\\{continue,\\s*x,\\s*"
                "\\[OwnTok,\\s*From\\]\\}"
            >>,
            [dotall, {capture, none}]
        )
    ),
    ?assertNotEqual(
        nomatch,
        re:run(
            Source,
            <<
                "replay_x\\(PendingEnvelope,\\s*"
                "OwnTok,\\s*From\\)"
            >>,
            [dotall, {capture, none}]
        )
    ).

ambiguous_state_recovery(Context) ->
    Module = maps:get(ambiguous, Context),
    with_sus_state(
        s0,
        fun() ->
            ?assertEqual(
                {withhold, ambiguous_state},
                recover_missing_event(Module, 9)
            ),
            ?assertEqual(s0, ets:lookup_element(sus_state, current_state, 2)),
            ?assertEqual(undefined, ets:lookup_element(sus_state, previous_state, 2))
        end
    ),
    assert_withholding_lifecycle(
        Module,
        s0,
        0,
        9,
        {trace, send, 0},
        ambiguous_state
    ).

equal_concrete_consequences(Context) ->
    Module = maps:get(duplicate, Context),
    Recovery = with_sus_state(
        s1,
        fun() ->
            {ok, Result, _Envelope} = recover_missing_event(Module, 9),
            ?assertEqual(
                [{literal, 0}, {literal, 1}],
                maps:get(event_specs, Result)
            ),
            Result
        end
    ),
    ReductionFun = fun
        ({literal, Event}) when Event =:= 0; Event =:= 1 ->
            {ok, [{{continue, x, [5]}, fun(_Envelope) -> continue end}]};
        (_) ->
            {unknown, unexpected_event_spec}
    end,
    ?assertMatch(
        {ok, #{
            consequence := {continue, x, [5]},
            event := unknown
        }},
        Module:resolve_monitoring_consequence(Recovery, ReductionFun)
    ),

    with_sus_state(
        s1,
        fun() ->
            {Worker, MonitorRef, _StateFunction} =
                start_monitor_state(Module, 5, [
                    missing_event,
                    {trace, send, 9}
                ]),
            ?assertEqual(recv_state, await_checkpoint(Module, Worker)),
            stop_worker(Worker, MonitorRef)
        end
    ).

conflicting_concrete_consequences(Context) ->
    Module = maps:get(duplicate, Context),
    Recovery = with_sus_state(
        s1,
        fun() ->
            {ok, Result, _Envelope} = recover_missing_event(Module, 9),
            Result
        end
    ),
    ReductionFun = fun
        ({literal, 0}) ->
            {ok, [{{verdict, no}, fun(_Envelope) -> no end}]};
        ({literal, 1}) ->
            {ok, [{{continue, x, [0]}, fun(_Envelope) -> continue end}]};
        (_) ->
            {unknown, unexpected_event_spec}
    end,
    ?assertEqual(
        {withhold, ambiguous_consequence},
        Module:resolve_monitoring_consequence(Recovery, ReductionFun)
    ),

    assert_withholding_lifecycle(
        Module,
        s1,
        0,
        9,
        {trace, send, 0},
        ambiguous_consequence
    ).

impossible_recovery(Context) ->
    Module = maps:get(impossible, Context),
    with_sus_state(
        s0,
        fun() ->
            ?assertEqual(
                {withhold, impossible_recovery},
                recover_missing_event(Module, 9)
            ),
            ?assertEqual(s0, ets:lookup_element(sus_state, current_state, 2)),
            ?assertEqual(undefined, ets:lookup_element(sus_state, previous_state, 2))
        end
    ),
    assert_withholding_lifecycle(
        Module,
        s0,
        0,
        9,
        {trace, send, 0},
        impossible_recovery
    ).

uniform_symbolic_consequence(Context) ->
    Module = maps:get(symbolic, Context),
    Recovery = with_sus_state(
        s2,
        fun() ->
            {ok, Result, _Envelope} = recover_missing_event(Module, 0),
            ?assertEqual(
                [{symbolic, natural_integer}],
                maps:get(event_specs, Result)
            ),
            Result
        end
    ),
    ReductionFun = fun
        ({symbolic, natural_integer}) ->
            {ok, [{{continue, x, [-1]}, fun(_Envelope) -> continue end}]};
        (_) ->
            {unknown, unexpected_event_spec}
    end,
    ?assertMatch(
        {ok, #{
            consequence := {continue, x, [-1]},
            event := unknown
        }},
        Module:resolve_monitoring_consequence(Recovery, ReductionFun)
    ),

    with_sus_state(
        s2,
        fun() ->
            {Worker, MonitorRef, _StateFunction} =
                start_monitor_state(Module, -1, [
                    missing_event,
                    {trace, send, 0}
                ]),
            ?assertEqual(recv_state, await_checkpoint(Module, Worker)),
            stop_worker(Worker, MonitorRef)
        end
    ).

conflicting_symbolic_consequence(Context) ->
    Module = maps:get(symbolic, Context),
    assert_withholding_lifecycle(
        Module,
        s2,
        5,
        0,
        {trace, send, 5},
        ambiguous_consequence
    ).

unproven_symbolic_consequence(Context) ->
    Module = maps:get(unproven, Context),
    assert_withholding_lifecycle(
        Module,
        s2,
        5,
        0,
        {trace, send, 5},
        unproven_consequence
    ).

assert_withholding_lifecycle(
    Module,
    CurrentState,
    OwnToken,
    Lookahead,
    Extension,
    ExpectedReason
) ->
    with_sus_state(
        CurrentState,
        fun() ->
            Snapshot = recovery_state_snapshot(),
            {Worker, MonitorRef, _StateFunction} =
                start_monitor_state(Module, OwnToken, [
                    missing_event,
                    {trace, send, Lookahead},
                    Extension
                ]),
            ?assertEqual(
                {withhold, ExpectedReason},
                await_worker_result(Worker, MonitorRef)
            ),
            ?assertEqual(Snapshot, recovery_state_snapshot()),
            ?assertEqual([], collect_verdicts([]))
        end
    ).

test_checkpoint_is_generated(Context) ->
    Source = generated_source(maps:get(unique, Context)),
    ?assertNotEqual(nomatch, binary:match(Source, <<"agm_checkpoint">>)).

explicit_engine_dependency(Context) ->
    Module = maps:get(unique, Context),
    Source = generated_source(Module),
    ?assertNotEqual(nomatch, binary:match(Source, <<"agm_engine:">>)),
    ?assertEqual(
        nomatch,
        binary:match(Source, <<"collect_monitoring_consequences(">>)
    ).


%%% ----------------------------------------------------------------------------
%%%
%%% Test helper functions
%%%
%%% ----------------------------------------------------------------------------


setup_suite() ->
    TmpDir = setup_tmp_dir(),
    {ok, StandardProperty} = file:read_file("test/props/prop_no_leak.hml"),
    try
        Modules = #{
            exact_yes => compile_generated_monitor(
                TmpDir,
                "agm_exact_yes",
                send_loop_property("tt"),
                unique_system_info()
            ),
            exact_no => compile_generated_monitor(
                TmpDir,
                "agm_exact_no",
                send_loop_property("ff"),
                unique_system_info()
            ),
            multi_no => compile_generated_monitor(
                TmpDir,
                "agm_multi_no",
                send_loop_property("ff"),
                duplicate_event_system_info()
            ),
            unique => compile_generated_monitor(
                TmpDir,
                "agm_unique",
                StandardProperty,
                unique_system_info()
            ),
            ambiguous => compile_generated_monitor(
                TmpDir,
                "agm_ambiguous",
                StandardProperty,
                ambiguous_system_info()
            ),
            duplicate => compile_generated_monitor(
                TmpDir,
                "agm_duplicate",
                StandardProperty,
                duplicate_event_system_info()
            ),
            impossible => compile_generated_monitor(
                TmpDir,
                "agm_impossible",
                StandardProperty,
                impossible_system_info()
            ),
            symbolic => compile_generated_monitor(
                TmpDir,
                "agm_symbolic",
                StandardProperty,
                symbolic_system_info()
            ),
            unproven => compile_generated_monitor(
                TmpDir,
                "agm_unproven",
                unsupported_guard_property(),
                symbolic_system_info()
            )
        },
        Modules#{tmp_dir => TmpDir}
    catch
        Class:Reason:Stacktrace ->
            remove_tmp_dir(TmpDir),
            erlang:raise(Class, Reason, Stacktrace)
    end.

cleanup_suite(Context) ->
    delete_sus_state(),
    maps:foreach(
        fun
            (tmp_dir, _) ->
                ok;
            (_, Module) ->
                code:purge(Module),
                code:delete(Module)
        end,
        Context
    ),
    remove_tmp_dir(maps:get(tmp_dir, Context)).

compile_generated_monitor(TmpDir, Name, PropertySource, SystemInfoSource) ->
    OutDir = filename:join(TmpDir, Name),
    ok = file:make_dir(OutDir),
    PropertyFile = filename:join(OutDir, Name ++ ".hml"),
    SystemInfoFile = filename:join(OutDir, Name ++ ".spec"),
    ok = file:write_file(PropertyFile, PropertySource),
    ok = file:write_file(SystemInfoFile, SystemInfoSource),
    ok = maxhml_eval:compile(
        PropertyFile,
        [{outdir, OutDir}, {mtab, SystemInfoFile}, erl]
    ),

    ErlFiles = lists:sort(filelib:wildcard(filename:join(OutDir, "*.erl"))),
    CompileResults = [
        compile:file(
            File,
            [
                export_all,
                {outdir, OutDir},
                {i, "include"},
                return_errors,
                return_warnings
            ]
        )
     || File <- ErlFiles
    ],
    CompileErrors = [Result || Result <- CompileResults, not is_compile_success(Result)],
    ?assertEqual([], CompileErrors),

    Module = list_to_atom(Name ++ "_flu"),
    code:purge(Module),
    code:delete(Module),
    {module, Module} = code:load_abs(filename:join(OutDir, atom_to_list(Module))),
    Module.

recover_missing_event(Module, NextEvent) ->
    element(1, recover_missing_event_with_envelope(Module, NextEvent)).

recover_missing_event_with_envelope(Module, NextEvent) ->
    From = self(),
    Envelope = {{trace, self(), send, NextEvent, self()}, From},
    self() ! Envelope,
    {Module:handle_missing_event(From), Envelope}.

with_sus_state(CurrentState, TestFun) ->
    delete_sus_state(),
    ets:new(sus_state, [named_table, public, set]),
    ets:insert(sus_state, {current_state, CurrentState}),
    ets:insert(sus_state, {previous_state, undefined}),
    try
        TestFun()
    after
        delete_sus_state()
    end.

delete_sus_state() ->
    case ets:whereis(sus_state) of
        undefined ->
            ok;
        _Table ->
            ets:delete(sus_state),
            ok
    end.

recovery_state_snapshot() ->
    {
        ets:lookup_element(sus_state, previous_state, 2),
        ets:lookup_element(sus_state, current_state, 2)
    }.

equivalent_encodings(MissingEvent, Lookahead, Suffix) ->
    {
        [{trace, send, MissingEvent}, {trace, send, Lookahead} | Suffix],
        [missing_event, {trace, send, Lookahead} | Suffix]
    }.

run_terminal_path(Module, Function, OwnToken, Messages) ->
    run_terminal_function(Module, Function, [OwnToken], Messages).

run_terminal_function(Module, Function, StateArgs, Messages) ->
    with_sus_state(
        s1,
        fun() ->
            {Worker, MonitorRef} =
                start_monitor_function(Module, Function, StateArgs, Messages),
            Result = await_worker_result(Worker, MonitorRef),
            {Result, collect_verdicts([])}
        end
    ).

run_checkpoint_path(Module, OwnToken, Messages) ->
    with_sus_state(
        s1,
        fun() ->
            {Worker, MonitorRef, _StateFunction} =
                start_monitor_state(Module, OwnToken, Messages),
            Observation = {await_checkpoint(Module, Worker), collect_verdicts([])},
            stop_worker(Worker, MonitorRef),
            Observation
        end
    ).

run_continuing_path(Module, OwnToken, Prefix, Suffix) ->
    with_sus_state(
        s1,
        fun() ->
            {Worker, MonitorRef, _StateFunction} =
                start_monitor_state(Module, OwnToken, Prefix),
            PrefixObservation = {
                await_checkpoint(Module, Worker),
                collect_verdicts([])
            },
            lists:foreach(
                fun(Message) ->
                    Worker ! materialize_message(Message, Worker, self())
                end,
                Suffix
            ),
            SuffixObservation = {
                await_checkpoint(Module, Worker),
                collect_verdicts([])
            },
            stop_worker(Worker, MonitorRef),
            {
                element(1, PrefixObservation),
                element(2, PrefixObservation),
                element(1, SuffixObservation),
                element(2, SuffixObservation)
            }
        end
    ).

start_monitor_state(Module, OwnToken, Messages) ->
    StateFunction = composite_send_function(Module),
    {Worker, MonitorRef} =
        start_monitor_function(Module, StateFunction, [OwnToken], Messages),
    {Worker, MonitorRef, StateFunction}.

start_monitor_function(Module, StateFunction, StateArgs, Messages) ->
    Parent = self(),
    StartRef = make_ref(),
    {Worker, MonitorRef} = spawn_monitor(fun() ->
        Parent ! {phase5_ready, StartRef, self()},
        receive
            {phase5_start, StartRef} ->
                Result = apply(Module, StateFunction, StateArgs ++ [Parent]),
                Parent ! {agm_worker_result, self(), Result}
        end
    end),
    receive
        {phase5_ready, StartRef, Worker} ->
            ok
    after 1000 ->
        erlang:error({worker_start_timeout, Worker})
    end,
    lists:foreach(
        fun(Message) ->
            Worker ! materialize_message(Message, Worker, Parent)
        end,
        Messages
    ),
    Worker ! {phase5_start, StartRef},
    {Worker, MonitorRef}.

materialize_message(missing_event, _Worker, Parent) ->
    {missing_event, Parent};
materialize_message({trace, send, Event}, Worker, Parent) ->
    {{trace, Worker, send, Event, Parent}, Parent};
materialize_message({trace, recv, Event}, Worker, Parent) ->
    {{trace, Worker, 'receive', Event}, Parent}.

await_plain_verdict() ->
    receive
        no ->
            no
    after 1000 ->
        erlang:error(verdict_timeout)
    end.

await_terminal_verdict() ->
    receive
        Verdict when Verdict =:= yes; Verdict =:= no ->
            Verdict
    after 1000 ->
        erlang:error(verdict_timeout)
    end.

await_worker_result(Worker, MonitorRef) ->
    Result =
        receive
            {agm_worker_result, Worker, Value} ->
                Value
        after 1000 ->
            erlang:error({worker_result_timeout, Worker})
        end,
    receive
        {'DOWN', MonitorRef, process, Worker, normal} ->
            Result;
        {'DOWN', MonitorRef, process, Worker, Reason} ->
            erlang:error({worker_failed, Worker, Reason})
    after 1000 ->
        erlang:error({worker_down_timeout, Worker})
    end.

assert_no_verdict() ->
    receive
        Verdict when Verdict =:= yes; Verdict =:= no ->
            erlang:error({unexpected_verdict, Verdict})
    after 0 ->
        ok
    end.

collect_verdicts(Acc) ->
    receive
        Verdict when Verdict =:= yes; Verdict =:= no ->
            collect_verdicts([Verdict | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

await_checkpoint(Module, Worker) ->
    ReceiveFunction = receive_state_function(Module),
    Ref = make_ref(),
    Worker ! {agm_checkpoint, Ref, self()},
    receive
        {agm_checkpoint, Ref, Worker, ReceiveFunction} ->
            recv_state
    after 1000 ->
        erlang:error({checkpoint_timeout, Worker, ReceiveFunction})
    end.

stop_worker(Worker, MonitorRef) ->
    exit(Worker, kill),
    receive
        {'DOWN', MonitorRef, process, Worker, killed} ->
            ok
    after 1000 ->
        erlang:error({worker_stop_timeout, Worker})
    end.

composite_send_function(Module) ->
    find_single_export(Module, "^send[0-9]+send[0-9]+$", 2).

receive_state_function(Module) ->
    find_single_export(Module, "^recv[0-9]+$", 2).

find_single_export(Module, Pattern, Arity) ->
    Matches = [
        Function
     || {Function, ExportArity} <- Module:module_info(exports),
        ExportArity =:= Arity,
        re:run(atom_to_list(Function), Pattern, [{capture, none}]) =:= match
    ],
    case Matches of
        [Function] ->
            Function;
        _ ->
            erlang:error({unexpected_generated_exports, Pattern, Arity, Matches})
    end.

unique_system_info() ->
    [
        "{s1, 0, s2};\n",
        "{s2, 9, s3};\n"
    ].

ambiguous_system_info() ->
    [
        "{s0, 0, s1};\n",
        "{s0, 1, s2};\n",
        "{s1, 9, s3};\n",
        "{s2, 9, s4};\n"
    ].

duplicate_event_system_info() ->
    [
        "{s1, 0, s2};\n",
        "{s1, 1, s2};\n",
        "{s2, 9, s3};\n",
        "{s2, 8, s3};\n",
        "{s3, 9, s4};\n"
    ].

impossible_system_info() ->
    [
        "{s0, 0, s1};\n",
        "{s2, 9, s3};\n"
    ].

symbolic_system_info() ->
    [
        "{s2, N, s1};\n",
        "{s1, 0, s3};\n"
    ].


%% PROPERTY: [x]max X.([z, x = z]TerminalVerdict ∧ [z, x != z]X)
%%                                                  where TerminalVerdict == tt | TerminalVerdict == ff
send_loop_property(TerminalVerdict) ->
    [
        "with\n",
        "  token_server:loop(_, _)\n",
        "check\n",
        "  [{_ <- _, token_server:loop(OwnTok, _)}]\n",
        "  max X.(\n",
        "    [{_:_ ! Tok when OwnTok =:= Tok}]",
        TerminalVerdict,
        "\n",
        "    and\n",
        "    [{_:_ ! Tok when OwnTok =/= Tok}]X\n",
        "  ).\n"
    ].

%% PROPERTY: [x]max X.([z, x = z]ff ∧ [z, z > x]X)
%%                                                  where x = OwnTok and z = Tok
unsupported_guard_property() ->
    [
        "with\n",
        "  token_server:loop(_, _)\n",
        "check\n",
        "  [{_ <- _, token_server:loop(OwnTok, _)}]\n",
        "  max X.(\n",
        "    [{_:_ ! Tok when OwnTok =:= Tok}]ff\n",
        "    and\n",
        "    [{_:_ ! Tok when Tok > OwnTok}]X\n",
        "  ).\n"
    ].

generated_source(Module) ->
    BeamPath = code:which(Module),
    SourcePath = filename:rootname(BeamPath) ++ ".erl",
    {ok, Source} = file:read_file(SourcePath),
    Source.

setup_tmp_dir() ->
    TmpBase =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            Dir -> Dir
        end,
    TmpDir = filename:join(
        TmpBase,
        "detecter_generated_agm_recovery_"
            ++ integer_to_list(erlang:system_time(nanosecond))
            ++ "_"
            ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    case file:make_dir(TmpDir) of
        ok ->
            TmpDir;
        {error, eexist} ->
            setup_tmp_dir();
        {error, Reason} ->
            erlang:error({tmp_dir_setup_failed, TmpDir, Reason})
    end.

remove_tmp_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(
                fun(File) ->
                    Path = filename:join(Dir, File),
                    case filelib:is_dir(Path) of
                        true -> remove_tmp_dir(Path);
                        false -> file:delete(Path)
                    end
                end,
                Files
            ),
            file:del_dir(Dir);
        _ ->
            ok
    end.

is_compile_success({ok, _Module}) -> true;
is_compile_success({ok, _Module, _Warnings}) -> true;
is_compile_success({ok, _Module, _Binary, _Warnings}) -> true;
is_compile_success(_) -> false.
