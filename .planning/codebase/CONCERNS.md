# Codebase Concerns

**Analysis Date:** 2026-06-24

## Tech Debt

**Make-based build previously compiled generated test artifacts as source:**
- Status: Addressed by Phase 1 plan 01-01. `detecter/Makefile` now defines `TEST_SRC` with `! -path "*/ebin/*"` and compiles that filtered list during `compile-test`.
- Original issue: `detecter/Makefile` compiled every `*.erl` under `detecter/test`, including generated files in `detecter/test/regeneration/ebin`. `make test` failed during `compile-test` because `detecter/test/regeneration/ebin/prop_no_double_a_flu_orig.erl` declares module `prop_no_double_a_flu`, which does not match the file name.
- Files: `detecter/Makefile`, `detecter/test/regeneration/ebin/prop_no_double_a_flu_orig.erl`, `detecter/test/regeneration/ebin/*`
- Impact: The default test target now reaches EUnit and passes the active 24-test `log_tracer_test` suite. Phase 3 broadened generated-monitor compile coverage to representative properties. Remaining risk: generated regeneration fixtures still live in a confusing `ebin` path and need layout cleanup later.
- Follow-up: Decide in Phase 21 whether to move or rename generated regeneration fixtures.

**Manual AST construction duplicated across weavers:**
- Issue: `detecter/src/monitoring/weaver.erl` and `detecter/src/synthesis/lin_weaver.erl` both hand-build Erlang abstract forms with local helpers such as `abs_atom/2`, `abs_remote_call/4`, `abs_fun/2`, and `create_var/3`. Both files contain TODOs to replace this with `erl_parse`/structured APIs.
- Files: `detecter/src/monitoring/weaver.erl`, `detecter/src/synthesis/lin_weaver.erl`
- Impact: Instrumentation correctness depends on duplicated tuple shapes and assumptions such as `element(2, Expr)` being the line number. Any syntax-tree edge case can break generated code in two places.
- Fix approach: Centralize AST construction in a shared helper module using `erl_syntax` or `erl_parse` patterns, then migrate both weavers through tests that compile woven modules.

**Synthesis path carries incomplete state-machine assumptions:**
- Issue: `detecter/src/synthesis/maxhml_eval.erl` initializes a public named ETS table `sus_state` and always inserts `current_state = s0`, with comments noting the start state and init block need validation. Guard support and receive-clause support are also marked incomplete.
- Files: `detecter/src/synthesis/maxhml_eval.erl`
- Impact: Generated monitors can encode the wrong automaton state, fail for unsupported guards, or ignore non-send trace shapes. State table name reuse also prevents multiple generated monitors from safely coexisting in one Erlang node.
- Fix approach: Make state initialization explicit in the generated spec model, use monitor-scoped ETS table names or process-local state, and add generator tests for non-`s0` starts, each supported guard, and receive/init/send trace variants.

**Synthesis variable names mix atoms and strings:**
- Issue: `detecter/src/synthesis/maxhml_eval.erl` receives variable names from parsed patterns as atoms such as `'OwnTok'` and `'_'`, while helper paths such as verdict argument generation use strings such as `"From"`.
- Files: `detecter/src/synthesis/maxhml_eval.erl`
- Impact: Generator helpers need defensive checks for both atom and string forms, making state-update argument selection and continuation argument handling easier to get wrong.
- Fix approach: Phase 22 tracks normalizing synthesis variable names to one documented internal representation with regression tests across init, action, verdict, and recursive paths.

**Regeneration parser is partial and string handling is missing:**
- Issue: `detecter/src/regeneration/sys_info_parser.erl` supports only `NULL`, `N`, `Z`, `R`, and one set-minus guard. It marks unions/disjunctions and string parsing as TODOs.
- Files: `detecter/src/regeneration/sys_info_parser.erl`, `detecter/priv/sys_info.spec`, `detecter/test/regeneration/sys_info.spec`
- Impact: Valid-looking `.spec` inputs outside this narrow grammar either produce `unsupported_condition`, crash by pattern matching, or silently coerce payloads to atoms instead of strings.
- Fix approach: Define the accepted `.spec` grammar, parse it with a proper lexer/parser or structured clauses, return `{ok, Parsed} | {error, Reason}` instead of throwing match failures, and cover each grammar feature in `detecter/test/regeneration/sys_info_parser_test.erl`.

**Monitor attach/detach API is stubbed but not exported:**
- Issue: `online_attach/3` and `online_detach/1` in `detecter/src/monitoring/monitor.erl` return `ok` without attaching or detaching monitors. The functions are unused and not exported.
- Files: `detecter/src/monitoring/monitor.erl`
- Impact: Future callers could assume dynamic monitor attachment exists based on local APIs/comments, but no runtime behavior is implemented.
- Fix approach: Either remove the stubbed functions until designed, or export and implement them with tracer lifecycle tests that prove events are analyzed after attach and ignored after detach.

## Known Bugs

**Deleting unknown traced processes can crash despite the relaxed assertion:**
- Symptoms: `del_proc/2` comments out the existence assertion, then immediately calls `maps:get(PidS, Traced)` in a trace log. If `PidS` is absent, deletion raises `badkey` before `maps:remove/2`.
- Files: `detecter/src/monitoring/tracer.erl`
- Trigger: An exit/delete event for a process that is not present in the tracer's `Traced` map.
- Workaround: No safe code path is present. Keep `tracer_test` scenarios that exercise process death enabled when changing trace routing.

**Default test target does not run the main tracer suite:**
- Symptoms: `detecter/Makefile` runs `log_tracer_test`, `sys_info_parser_test`, and `generated_monitor_smoke_test`; `tracer_test` remains manual. `detecter/test/monitoring/tracer_test.erl` states the tests are time dependent and excluded from the main build.
- Files: `detecter/Makefile`, `detecter/test/monitoring/tracer_test.erl`
- Trigger: Running `make test`.
- Workaround: Run `make compile-test` and then `erl -noshell -pa ebin -eval 'case eunit:test(tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop` manually when changing tracer routing.

**Deprecated stacktrace API remains in compile path:**
- Symptoms: `make test` emits warnings for removed `erlang:get_stacktrace/0` calls in source files.
- Files: `detecter/src/synthesis/gen_eval.erl`, `detecter/src/synthesis/lin_weaver.erl`, `detecter/src/monitoring/hml_eval.erl`, `detecter/src/monitoring/weaver.erl`
- Trigger: Compiling on modern Erlang/OTP.
- Workaround: Current compilation continues with warnings, but future OTP versions may turn related syntax or semantics into hard failures.

## Security Considerations

**Unbounded atom creation from input files:**
- Risk: `list_to_atom/1` is called on parsed `.spec` state/event/payload text. Atoms are not garbage-collected in Erlang, so untrusted or large files can exhaust the atom table.
- Files: `detecter/src/regeneration/sys_info_parser.erl`
- Current mitigation: None detected.
- Recommendations: Use `binary_to_existing_atom/2` only for known enums, keep user-defined labels as binaries/strings, or intern through a bounded map owned by the parser.

**Public named ETS tables expose mutable monitor state:**
- Risk: Monitor/tracer internals use `public` `named_table` ETS tables. Any process on the node can read or mutate entries, which can corrupt routing, monitor information, or synthesized state.
- Files: `detecter/src/monitoring/tracer.erl`, `detecter/src/tracing/log_tracer.erl`, `detecter/src/synthesis/maxhml_eval.erl`
- Current mitigation: Table names are module macros or generated names, but access is still public.
- Recommendations: Prefer `protected` or private owner-mediated access, generate per-monitor table names where sharing is required, and add tests proving unrelated processes cannot alter monitor state.

## Performance Bottlenecks

**Process-state history is intentionally discarded to reduce memory:**
- Problem: `detecter/src/synthesis/lin_analyzer.erl` discards `PdList` by calling `analyze(Event, M, [])`, with TODOs noting this is a space-efficiency shortcut.
- Files: `detecter/src/synthesis/lin_analyzer.erl`
- Cause: Path/detail state can grow with event history, so the current implementation trades explainability for lower memory.
- Improvement path: Make history retention an option, keep bounded or sampled provenance for verdict explanation, and benchmark memory on long traces.

**Trace/debug paths format and store large event structures:**
- Problem: Tracers accumulate reversed traces and can dump routes, traced maps, stats, message queues, and trace lists. Debug mode in `lin_analyzer` forwards every event to `event_writer`.
- Files: `detecter/src/monitoring/tracer.erl`, `detecter/src/synthesis/lin_analyzer.erl`, `detecter/src/synthesis/event_writer.erl`
- Cause: Full trace retention and synchronous formatting scale with event volume.
- Improvement path: Gate trace retention behind options, store counters by default, and use bounded ring buffers for diagnostics.

## Fragile Areas

**Concurrent trace routing:**
- Files: `detecter/src/monitoring/tracer.erl`, `detecter/src/tracing/log_tracer.erl`, `detecter/test/monitoring/tracer_test.erl`, `detecter/test/tracing/log_tracer_test.erl`
- Why fragile: Routing depends on interleavings among process fork/exit events, ETS state, and asynchronous messages. The most detailed tracer tests use sleeps and are excluded from the default build.
- Safe modification: Preserve route/traced-map invariants, add deterministic synchronization to tests before changing `add_proc/3`, `del_proc/2`, `add_route/4`, `del_route/3`, or event forwarding paths, and run the tracer suite repeatedly.
- Test coverage: `log_tracer_test`, `sys_info_parser_test`, and `generated_monitor_smoke_test` are included by the make target; `tracer_test` is not included and currently depends on timing.

**Generated parsers and lexers are checked in beside source grammars:**
- Files: `detecter/priv/hml_parser.yrl`, `detecter/src/monitoring/hml_parser.erl`, `detecter/priv/shml_parser.yrl`, `detecter/src/monitoring/shml_parser.erl`, `detecter/priv/maxhml_parser.yrl`, `detecter/src/synthesis/maxhml_parser.erl`, `detecter/priv/*_lexer.xrl`, `detecter/src/**/*_lexer.erl`
- Why fragile: Large generated modules dominate the source tree, and changes to `.yrl`/`.xrl` files must be regenerated consistently.
- Safe modification: Edit grammar files in `detecter/priv`, regenerate parser/lexer outputs in the expected `detecter/src` locations, and compile with `make compile`.
- Test coverage: Parser-specific EUnit coverage is limited; most parser behavior is indirectly covered through synthesis/monitoring tests.

**Regeneration workflow has untracked helper code and generated artifacts:**
- Files: `detecter/test/regeneration/automated_event_streamer.erl`, `detecter/test/regeneration/ebin/*`, `detecter/test/regeneration/sys_info_parser_test.erl`
- Why fragile: The working tree contains an untracked helper and generated outputs under a path named `ebin`. `compile-test` now excludes `test/**/ebin/**`, but the fixture layout is still easy to misunderstand.
- Safe modification: Decide whether regeneration fixtures are source or build output, move them accordingly, and keep `.gitignore`/`Makefile` rules aligned so generated artifacts cannot enter source compilation accidentally.
- Test coverage: `sys_info_parser_test` now runs through `make test`; generated-monitor compile coverage includes `prop_no_leak`, `prop_no_failure`, and `prop_correct_start`.

## Scaling Limits

**Single-node global state prevents multiple independent monitor runs:**
- Current capacity: One active tracing backend in `trace_lib` via `persistent_term`, one set of named tracer/log-tracer ETS tables, and one generated `sus_state` table in synthesized monitors.
- Limit: Starting multiple monitoring sessions in the same VM can collide on global module state, named ETS tables, registered processes, or persistent term keys.
- Scaling path: Thread monitor session IDs through tracer/analyzer/synthesis APIs, scope ETS table names per session, and avoid global registration except for supervised singleton services.

## Dependencies at Risk

**No dependency manager metadata for the Erlang app:**
- Risk: The Erlang project is built with `detecter/Makefile` only; there is no `rebar.config` detected for dependency/version management, Dialyzer PLT setup, or standard test profiles.
- Impact: OTP compatibility and CI reproducibility depend on local Erlang installations and manual make targets.
- Migration plan: Add a minimal `rebar3` configuration or document exact OTP/make requirements, then map existing `make compile`, `make test`, and `make analyze` behavior to reproducible profiles.

**Tutorial Python virtualenv exists inside the repo workspace:**
- Risk: `examples/python/venv` is present and contains third-party packages, even though `.gitignore` ignores `venv/`.
- Impact: Local scans and line counts are noisy, security tools may inspect vendored dependencies as first-party code, and accidental staging is easy if ignore rules change.
- Migration plan: Keep virtualenvs outside the repository or under ignored local-only paths; use `tutorial/requirements.txt` or example-specific requirements to recreate environments.

## Missing Critical Features

**Dynamic online monitor attach/detach is not implemented:**
- Problem: Runtime attachment/detachment is marked as future work and does not perform tracing changes.
- Blocks: Long-running systems cannot add or remove monitoring without starting through `monitor:start_online/3`.

**Regeneration parser lacks complete condition language support:**
- Problem: Union, disjunction, string conditions, and broader clause/event support are not implemented.
- Blocks: Regenerating monitors from richer system-information specs.

## Test Coverage Gaps

**Tracer routing concurrency is under-tested by the default workflow:**
- What's not tested: The detailed `tracer_test` scenarios for process fork/exit routing and monitor attachment relationships are excluded from `make test`.
- Files: `detecter/Makefile`, `detecter/test/monitoring/tracer_test.erl`, `detecter/src/monitoring/tracer.erl`
- Risk: Regressions in event routing, route cleanup, and process deletion can ship unnoticed.
- Priority: High

**Regeneration parser edge cases are not fully covered:**
- What's not tested: Unsupported conditions, malformed lines, string payloads, multiple guards, and atom exhaustion protections.
- Files: `detecter/src/regeneration/sys_info_parser.erl`, `detecter/test/regeneration/sys_info_parser_test.erl`
- Risk: `.spec` parsing failures appear at runtime or during generated-monitor workflows.
- Priority: High

**Synthesis generated-code behavior lacks focused regression tests:**
- What's not tested: Non-`s0` start states, public ETS table collisions, additional guard combinations, receive/event shapes beyond the currently handled cases, and multiple monitors in one VM.
- Files: `detecter/src/synthesis/maxhml_eval.erl`, `detecter/src/synthesis/gen_eval.erl`, `detecter/src/synthesis/lin_analyzer.erl`
- Risk: Generated monitors compile but encode incorrect state transitions or fail under concurrent sessions.
- Priority: Medium

---

*Concerns audit: 2026-06-24*
