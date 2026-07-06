%%% ----------------------------------------------------------------------------
%%% Synthesis-level regression tests for the modular maxHML generator.
%%%
%%% Covers the Phase 5.2 soundness gate:
%%% - unsupported property shapes are rejected before any code is generated
%%%   (audit finding C1), including multi-property files (M5);
%%% - generation state is compilation-scoped, so sequential compilations in
%%%   one VM cannot corrupt each other (C2);
%%% - the generated init block derives its initial state from the model's
%%%   START row and no longer feeds spawn arguments into
%%%   update_current_state/1 (H2), proven end-to-end through flu_spec/0.
%%% ----------------------------------------------------------------------------
-module(maxhml_synthesis_test).
-author("Andre Theuma").

-include_lib("eunit/include/eunit.hrl").

%%% ----------------------------------------------------------------------------
%%% Fragment validation (C1/M5).
%%% ----------------------------------------------------------------------------

chained_terminal_necessity_is_rejected_test() ->
    in_tmp_dir(fun(Dir) ->
        Property = [
            "with\n",
            "  token_server:loop(_, _)\n",
            "check\n",
            "  [{_ <- _, token_server:loop(OwnTok, _)}]\n",
            "  [{_:_ ! Tok when OwnTok =:= Tok}]ff.\n"
        ],
        Result = compile_property(Dir, "agm_reject_chain", Property),
        ?assertEqual(
            {error, {unsupported_property_fragment, necessity_into_verdict}},
            Result
        ),
        assert_no_generated_monitor(Dir)
    end).

recursion_wrapper_necessity_is_rejected_test() ->
    in_tmp_dir(fun(Dir) ->
        Property = [
            "with\n",
            "  token_server:loop(_, _)\n",
            "check\n",
            "  [{_ <- _, token_server:loop(OwnTok, _)}]\n",
            "  max X.(\n",
            "    [{_:_ ! Tok}]X\n",
            "  ).\n"
        ],
        Result = compile_property(Dir, "agm_reject_rec", Property),
        ?assertEqual(
            {error, {unsupported_property_fragment, necessity_into_recursion}},
            Result
        ),
        assert_no_generated_monitor(Dir)
    end).

multiple_properties_are_rejected_test() ->
    in_tmp_dir(fun(Dir) ->
        Property = [
            "with\n",
            "  token_server:loop(_, _)\n",
            "check\n",
            "  [{_ <- _, token_server:loop(OwnTok, _)}]ff,\n",
            "with\n",
            "  token_server:loop(_, _)\n",
            "check\n",
            "  [{_ <- _, token_server:loop(OwnTok, _)}]ff.\n"
        ],
        Result = compile_property(Dir, "agm_reject_multi", Property),
        ?assertEqual(
            {error, {unsupported_property_fragment, multiple_properties}},
            Result
        ),
        assert_no_generated_monitor(Dir)
    end).

necessity_chain_into_conjunction_is_accepted_test() ->
    in_tmp_dir(fun(Dir) ->
        % The tested fragment includes necessity chains whose continuation
        % owns a receive (prop_no_leak shape), so these must stay accepted.
        Property = [
            "with\n",
            "  token_server:loop(_, _)\n",
            "check\n",
            "  [{_ <- _, token_server:loop(OwnTok, _)}]\n",
            "  max X.(\n",
            "    [{_ ? {Var, _}}](\n",
            "      [{_:_ ! Tok when OwnTok =:= Tok}]ff\n",
            "      and\n",
            "      [{_:_ ! Tok when OwnTok =/= Tok}]X\n",
            "    )\n",
            "  ).\n"
        ],
        ?assertEqual(
            ok,
            compile_property(Dir, "agm_accept_chain", Property)
        ),
        ?assertMatch([_ | _], generated_monitor_sources(Dir))
    end).

%%% ----------------------------------------------------------------------------
%%% Compilation-scoped generation state (C2).
%%% ----------------------------------------------------------------------------

sequential_compilations_are_isolated_test() ->
    in_tmp_dir(fun(Dir) ->
        % Same property shape and line layout, different binder names. With
        % the old persistent_term memo the second monitor inherited the
        % first monitor's variable names and failed to compile.
        ?assertEqual(
            ok,
            compile_property(Dir, "agm_iso_a", send_loop_property("OwnTok"))
        ),
        ?assertEqual(
            ok,
            compile_property(Dir, "agm_iso_b", send_loop_property("BaseTok"))
        ),
        SecondSource = filename:join([Dir, "agm_iso_b", "agm_iso_b_flu.erl"]),
        {ok, Source} = file:read_file(SecondSource),
        ?assertEqual(nomatch, binary:match(Source, <<"OwnTok">>)),
        lists:foreach(
            fun(Name) ->
                ErlFile = filename:join([Dir, Name, Name ++ "_flu.erl"]),
                ?assertMatch(
                    {ok, _, _, _},
                    compile:file(ErlFile, [
                        binary, return_errors, return_warnings
                    ])
                )
            end,
            ["agm_iso_a", "agm_iso_b"]
        )
    end).

%%% ----------------------------------------------------------------------------
%%% Init block start-state derivation and end-to-end execution (H2).
%%% ----------------------------------------------------------------------------

init_start_state_end_to_end_test() ->
    in_tmp_dir(fun(Dir) ->
        Name = "agm_e2e",
        ok = compile_property(
            Dir,
            Name,
            send_loop_property("OwnTok"),
            start_row_system_info()
        ),
        Module = load_generated_monitor(Dir, Name),
        Source = generated_source(Dir, Name),

        % The init block derives its initial state from the START row and no
        % longer feeds the spawn argument into update_current_state/1.
        ?assertNotEqual(
            nomatch,
            binary:match(Source, <<"{current_state, s1}">>)
        ),
        ?assertEqual(
            nomatch,
            re:run(
                Source,
                <<"update_current_state\\(OwnTok\\)">>,
                [{capture, none}]
            )
        ),

        delete_sus_state(),
        Parent = self(),
        {Worker, MonitorRef} = spawn_monitor(fun() ->
            Result = Module:flu_spec(),
            Parent ! {monitor_result, self(), Result}
        end),

        % OwnTok is bound to 9 by the init event; event 0 recurses through
        % the inequality branch and advances the model s1 -> s2.
        Worker !
            {{trace, Worker, spawned, Worker,
                {token_server, loop, [9, extra]}}, Parent},
        Worker ! {{trace, Worker, send, 0, Parent}, Parent},
        CompositeState = composite_send_function(Module),
        ?assertEqual(
            CompositeState,
            checkpoint(Worker)
        ),
        ?assertEqual(s2, ets:lookup_element(sus_state, current_state, 2)),
        ?assertEqual(s1, ets:lookup_element(sus_state, previous_state, 2)),

        % Event 9 matches the equality branch and rejects exactly once.
        Worker ! {{trace, Worker, send, 9, Parent}, Parent},
        ?assertEqual(no, await_verdict()),
        receive
            {monitor_result, Worker, _} -> ok
        after 1000 ->
            erlang:error({monitor_result_timeout, Worker})
        end,
        receive
            {'DOWN', MonitorRef, process, Worker, normal} -> ok;
            {'DOWN', MonitorRef, process, Worker, Reason} ->
                erlang:error({monitor_failed, Reason})
        after 1000 ->
            erlang:error({monitor_down_timeout, Worker})
        end,
        ?assertEqual([], collect_verdicts([]))
    end).

%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

send_loop_property(Binder) ->
    [
        "with\n",
        "  token_server:loop(_, _)\n",
        "check\n",
        "  [{_ <- _, token_server:loop(", Binder, ", _)}]\n",
        "  max X.(\n",
        "    [{_:_ ! Tok when ", Binder, " =:= Tok}]ff\n",
        "    and\n",
        "    [{_:_ ! Tok when ", Binder, " =/= Tok}]X\n",
        "  ).\n"
    ].

default_system_info() ->
    [
        "{s1, 0, s2};\n",
        "{s2, 9, s3};\n"
    ].

start_row_system_info() ->
    [
        "{START, NULL, s1};\n",
        "{s1, 0, s2};\n",
        "{s2, 9, s3};\n"
    ].

compile_property(Dir, Name, PropertySource) ->
    compile_property(Dir, Name, PropertySource, default_system_info()).

compile_property(Dir, Name, PropertySource, SystemInfoSource) ->
    OutDir = filename:join(Dir, Name),
    ok = file:make_dir(OutDir),
    PropertyFile = filename:join(OutDir, Name ++ ".hml"),
    SystemInfoFile = filename:join(OutDir, Name ++ ".spec"),
    ok = file:write_file(PropertyFile, PropertySource),
    ok = file:write_file(SystemInfoFile, SystemInfoSource),
    maxhml_eval:compile(
        PropertyFile,
        [{outdir, OutDir}, {mtab, SystemInfoFile}, erl]
    ).

load_generated_monitor(Dir, Name) ->
    OutDir = filename:join(Dir, Name),
    ErlFile = filename:join(OutDir, Name ++ "_flu.erl"),
    {ok, Module, Binary, _Warnings} = compile:file(ErlFile, [
        binary, export_all, return_errors, return_warnings
    ]),
    code:purge(Module),
    {module, Module} = code:load_binary(Module, ErlFile, Binary),
    Module.

generated_source(Dir, Name) ->
    {ok, Source} = file:read_file(
        filename:join([Dir, Name, Name ++ "_flu.erl"])
    ),
    Source.

generated_monitor_sources(Dir) ->
    filelib:wildcard(filename:join([Dir, "*", "*_flu.erl"])).

assert_no_generated_monitor(Dir) ->
    ?assertEqual([], generated_monitor_sources(Dir)).

composite_send_function(Module) ->
    Matches = [
        Function
     || {Function, Arity} <- Module:module_info(exports),
        Arity =:= 2,
        re:run(atom_to_list(Function), "^send[0-9]+send[0-9]+$", [
            {capture, none}
        ]) =:= match
    ],
    case Matches of
        [Function] -> Function;
        _ -> erlang:error({unexpected_generated_exports, Matches})
    end.

checkpoint(Worker) ->
    Ref = make_ref(),
    Worker ! {agm_checkpoint, Ref, self()},
    receive
        {agm_checkpoint, Ref, Worker, StateName} -> StateName
    after 1000 ->
        erlang:error({checkpoint_timeout, Worker})
    end.

await_verdict() ->
    receive
        Verdict when Verdict =:= yes; Verdict =:= no -> Verdict
    after 1000 ->
        erlang:error(verdict_timeout)
    end.

collect_verdicts(Acc) ->
    receive
        Verdict when Verdict =:= yes; Verdict =:= no ->
            collect_verdicts([Verdict | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

delete_sus_state() ->
    case ets:whereis(sus_state) of
        undefined -> ok;
        _Table -> ets:delete(sus_state), ok
    end.

in_tmp_dir(TestFun) ->
    Dir = make_tmp_dir(),
    try
        TestFun(Dir)
    after
        remove_tmp_dir(Dir)
    end.

make_tmp_dir() ->
    TmpBase =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            Dir -> Dir
        end,
    TmpDir = filename:join(
        TmpBase,
        "detecter_maxhml_synthesis_"
            ++ integer_to_list(erlang:system_time(nanosecond))
            ++ "_"
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
