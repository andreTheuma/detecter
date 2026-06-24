-module(generated_monitor_smoke_test).
-author("André Theuma").

-include_lib("eunit/include/eunit.hrl").

generated_monitor_compile_test_() ->
    {"Generated monitor source compiles for a recursive regeneration property",
        {setup,
            fun setup_tmp_dir/0,
            fun cleanup_tmp_dir/1,
            fun(TmpDir) ->
                [?_test(begin
                    PropFile = "test/props/prop_no_leak.hml",
                    SysInfoFile = "priv/sys_info.spec",

                    ?assertEqual(
                        ok,
                        maxhml_eval:compile(
                            PropFile,
                            [{outdir, TmpDir}, {mtab, SysInfoFile}, erl]
                        )
                    ),

                    ErlFiles = lists:sort(filelib:wildcard(filename:join(TmpDir, "*.erl"))),
                    ?assert(length(ErlFiles) >= 2),

                    CompileResults = [
                        compile:file(
                            File,
                            [{outdir, TmpDir}, {i, "include"}, {i, "ebin"}, return_errors, return_warnings]
                        )
                     || File <- ErlFiles],
                    CompileErrors = [Result || Result <- CompileResults, not is_compile_success(Result)],
                    ?assertEqual([], CompileErrors)
                end)]
            end}}.

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
    case file:list_dir(TmpDir) of
        {ok, Files} ->
            lists:foreach(
                fun(File) ->
                    file:delete(filename:join(TmpDir, File))
                end,
                Files
            ),
            file:del_dir(TmpDir);
        _ ->
            ok
    end.

is_compile_success({ok, _Module}) -> true;
is_compile_success({ok, _Module, _Warnings}) -> true;
is_compile_success({ok, _Module, _Binary, _Warnings}) -> true;
is_compile_success(_) -> false.
