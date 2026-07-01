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
                {"unique concrete recovery has one consequence and known event metadata",
                    fun() -> unique_concrete_recovery(Context) end},
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
                {"generated monitors declare the AGM engine dependency",
                    fun() -> explicit_engine_dependency(Context) end}
            ]
        end}.

unique_concrete_recovery(Context) ->
    Module = maps:get(unique, Context),
    Recovery = with_sus_state(
        s1,
        fun() ->
            Result = recover_missing_event(Module, 9),
            ?assertMatch(
                {ok, #{
                    source_state := s1,
                    inferred_state := s2,
                    event_specs := [{literal, 0}]
                }},
                Result
            ),
            ?assertEqual(s2, ets:lookup_element(sus_state, current_state, 2)),
            element(2, Result)
        end
    ),
    ReductionFun = fun
        ({literal, 0}) ->
            {ok, [{{verdict, no}, fun() -> no end}]};
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
    ?assert(is_function(maps:get(continuation, ResolutionMap), 0)),

    with_sus_state(
        s1,
        fun() ->
            {Worker, _StateFunction} = start_monitor_state(Module, 0, 9),
            ?assertEqual(no, await_plain_verdict()),
            ?assertEqual(no, await_worker_result(Worker))
        end
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
    with_sus_state(
        s0,
        fun() ->
            {Worker, _StateFunction} = start_monitor_state(Module, 0, 9),
            ?assertEqual({withhold, ambiguous_state}, await_worker_result(Worker)),
            assert_no_verdict()
        end
    ).

equal_concrete_consequences(Context) ->
    Module = maps:get(duplicate, Context),
    Recovery = with_sus_state(
        s1,
        fun() ->
            {ok, Result} = recover_missing_event(Module, 9),
            ?assertEqual(
                [{literal, 0}, {literal, 1}],
                maps:get(event_specs, Result)
            ),
            Result
        end
    ),
    ReductionFun = fun
        ({literal, Event}) when Event =:= 0; Event =:= 1 ->
            {ok, [{{continue, x, [5]}, fun() -> continue end}]};
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
            {Worker, _StateFunction} = start_monitor_state(Module, 5, 9),
            assert_worker_reached_receive_state(Module, Worker),
            stop_worker(Worker)
        end
    ).

conflicting_concrete_consequences(Context) ->
    Module = maps:get(duplicate, Context),
    Recovery = with_sus_state(
        s1,
        fun() ->
            {ok, Result} = recover_missing_event(Module, 9),
            Result
        end
    ),
    ReductionFun = fun
        ({literal, 0}) ->
            {ok, [{{verdict, no}, fun() -> no end}]};
        ({literal, 1}) ->
            {ok, [{{continue, x, [0]}, fun() -> continue end}]};
        (_) ->
            {unknown, unexpected_event_spec}
    end,
    ?assertEqual(
        {withhold, ambiguous_consequence},
        Module:resolve_monitoring_consequence(Recovery, ReductionFun)
    ),

    with_sus_state(
        s1,
        fun() ->
            {Worker, _StateFunction} = start_monitor_state(Module, 0, 9),
            ?assertEqual(
                {withhold, ambiguous_consequence},
                await_worker_result(Worker)
            ),
            assert_no_verdict()
        end
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
    with_sus_state(
        s0,
        fun() ->
            {Worker, _StateFunction} = start_monitor_state(Module, 0, 9),
            ?assertEqual({withhold, impossible_recovery}, await_worker_result(Worker)),
            assert_no_verdict()
        end
    ).

uniform_symbolic_consequence(Context) ->
    Module = maps:get(symbolic, Context),
    Recovery = with_sus_state(
        s2,
        fun() ->
            {ok, Result} = recover_missing_event(Module, 0),
            ?assertEqual(
                [{symbolic, natural_integer}],
                maps:get(event_specs, Result)
            ),
            Result
        end
    ),
    ReductionFun = fun
        ({symbolic, natural_integer}) ->
            {ok, [{{continue, x, [-1]}, fun() -> continue end}]};
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
            {Worker, _StateFunction} = start_monitor_state(Module, -1, 0),
            assert_worker_reached_receive_state(Module, Worker),
            stop_worker(Worker)
        end
    ).

conflicting_symbolic_consequence(Context) ->
    Module = maps:get(symbolic, Context),
    with_sus_state(
        s2,
        fun() ->
            {Worker, _StateFunction} = start_monitor_state(Module, 5, 0),
            ?assertEqual(
                {withhold, ambiguous_consequence},
                await_worker_result(Worker)
            ),
            assert_no_verdict()
        end
    ).

unproven_symbolic_consequence(Context) ->
    Module = maps:get(unproven, Context),
    with_sus_state(
        s2,
        fun() ->
            {Worker, _StateFunction} = start_monitor_state(Module, 5, 0),
            ?assertEqual(
                {withhold, unproven_consequence},
                await_worker_result(Worker)
            ),
            assert_no_verdict()
        end
    ).

explicit_engine_dependency(Context) ->
    Module = maps:get(unique, Context),
    BeamPath = code:which(Module),
    SourcePath = filename:rootname(BeamPath) ++ ".erl",
    {ok, Source} = file:read_file(SourcePath),
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
    From = self(),
    self() ! {{trace, self(), send, NextEvent, self()}, From},
    Module:handle_missing_event(From).

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

start_monitor_state(Module, OwnToken, NextEvent) ->
    StateFunction = composite_send_function(Module),
    Parent = self(),
    Worker = spawn(fun() ->
        Parent ! {agm_worker_ready, self()},
        Result = apply(Module, StateFunction, [OwnToken, Parent]),
        Parent ! {agm_worker_result, self(), Result}
    end),
    receive
        {agm_worker_ready, Worker} ->
            ok
    after 1000 ->
        erlang:error({worker_start_timeout, Worker})
    end,
    Worker ! {missing_event, Parent},
    Worker ! {{trace, Worker, send, NextEvent, Parent}, Parent},
    {Worker, StateFunction}.

await_plain_verdict() ->
    receive
        no ->
            no
    after 1000 ->
        erlang:error(verdict_timeout)
    end.

await_worker_result(Worker) ->
    receive
        {agm_worker_result, Worker, Result} ->
            Result
    after 1000 ->
        erlang:error({worker_result_timeout, Worker})
    end.

assert_no_verdict() ->
    receive
        Verdict when Verdict =:= yes; Verdict =:= no ->
            erlang:error({unexpected_verdict, Verdict})
    after 0 ->
        ok
    end.

assert_worker_reached_receive_state(Module, Worker) ->
    ReceiveFunction = receive_state_function(Module),
    await_current_function(Module, Worker, ReceiveFunction, 100).

await_current_function(Module, Worker, Function, AttemptsLeft) ->
    case process_info(Worker, current_function) of
        {current_function, {Module, Function, 2}} ->
            ok;
        undefined ->
            erlang:error({worker_exited_before_continuation, Worker});
        _ when AttemptsLeft > 0 ->
            timer:sleep(10),
            await_current_function(Module, Worker, Function, AttemptsLeft - 1);
        CurrentFunction ->
            erlang:error({
                continuation_timeout,
                {expected, {Module, Function, 2}},
                {actual, CurrentFunction}
            })
    end.

stop_worker(Worker) ->
    MonitorRef = erlang:monitor(process, Worker),
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
        "{s2, 9, s3};\n"
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

setup_tmp_dir() ->
    TmpBase =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            Dir -> Dir
        end,
    TmpDir = filename:join(
        TmpBase,
        "detecter_generated_agm_recovery_"
            ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(TmpDir),
    TmpDir.

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
