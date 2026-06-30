-module(generated_monitor_smoke_test).
-author("André Theuma").

-include_lib("eunit/include/eunit.hrl").

generated_monitor_compile_test_() ->
    {"Generated monitor source compiles for representative regeneration properties",
        {setup,
            fun setup_tmp_dir/0,
            fun cleanup_tmp_dir/1,
            fun(TmpDir) ->
                [
                    ?_test(compile_generated_monitor(PropFile, TmpDir))
                 || PropFile <- generated_monitor_properties()
                ]
            end}}.

generated_monitor_properties() ->
    [
        "test/props/prop_no_leak.hml",
        "test/props/prop_no_failure.hml",
        "test/props/prop_correct_start.hml"
    ].

compile_generated_monitor(PropFile, TmpDir) ->
    PropOutDir = setup_prop_dir(PropFile, TmpDir),
    SysInfoFile = "priv/sys_info.spec",

    ?assertEqual(
        ok,
        maxhml_eval:compile(
            PropFile,
            [{outdir, PropOutDir}, {mtab, SysInfoFile}, erl]
        )
    ),

    ErlFiles = lists:sort(filelib:wildcard(filename:join(PropOutDir, "*.erl"))),
    ?assert(length(ErlFiles) >= 2),
    assert_no_zero_arity_state_update(ErlFiles),
    assert_transition_helpers_use_list_rows(ErlFiles),

    CompileResults = [
        compile:file(
            File,
            [{outdir, PropOutDir}, {i, "include"}, {i, "ebin"}, return_errors, return_warnings]
        )
     || File <- ErlFiles],
    CompileErrors = [Result || Result <- CompileResults, not is_compile_success(Result)],
    ?assertEqual([], CompileErrors).

setup_prop_dir(PropFile, TmpDir) ->
    BaseName = filename:basename(PropFile, ".hml"),
    PropOutDir = filename:join(TmpDir, BaseName),
    ok = file:make_dir(PropOutDir),
    PropOutDir.

assert_no_zero_arity_state_update(ErlFiles) ->
    lists:foreach(
        fun(File) ->
            {ok, Source} = file:read_file(File),
            ?assertEqual(nomatch, binary:match(Source, <<"update_current_state()">>))
        end,
        ErlFiles
    ).

assert_transition_helpers_use_list_rows(ErlFiles) ->
    lists:foreach(
        fun(File) ->
            {ok, Source} = file:read_file(File),
            ?assertEqual(nomatch, binary:match(Source, <<"maps:to_list(StateTransitionTable)">>)),
            ?assertEqual(nomatch, binary:match(Source, <<"maps:is_key">>))
        end,
        ErlFiles
    ).

setup_tmp_dir() ->
    TmpBase =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            Dir -> Dir
        end,
    TmpDir = filename:join(
        TmpBase,
        "detecter_generated_monitor_smoke_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(TmpDir),
    TmpDir.

cleanup_tmp_dir(TmpDir) ->
    remove_tmp_dir(TmpDir).

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
