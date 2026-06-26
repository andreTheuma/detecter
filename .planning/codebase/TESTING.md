# Testing Patterns

**Analysis Date:** 2026-06-24

## Test Framework

**Runner:**
- EUnit from Erlang/OTP.
- Config: `detecter/Makefile` defines `compile-test`, `test`, and `test-loop`; there is no `rebar.config`, Common Test config, pytest config, or JavaScript test config detected. `compile-test` compiles hand-written test sources from `detecter/test` while excluding generated `test/**/ebin/**` artifacts.

**Assertion Library:**
- EUnit macros from `-include_lib("eunit/include/eunit.hrl").` in `detecter/test/tracing/log_tracer_test.erl`, `detecter/test/monitoring/tracer_test.erl`, and `detecter/test/regeneration/sys_info_parser_test.erl`.

**Run Commands:**
```bash
cd detecter && make test        # Compile with -DTEST and run log_tracer_test, sys_info_parser_test, and generated_monitor_smoke_test
cd detecter && make test-loop   # Re-run log_tracer_test 100 times for flake detection
cd detecter && make analyze     # Run Dialyzer over source modules after compile
cd detecter && make compile-test && erl -noshell -pa ebin -eval 'case eunit:test(tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop
```

## Test File Organization

**Location:**
- First-party Erlang tests are under `detecter/test/<area>/`: `detecter/test/tracing/log_tracer_test.erl`, `detecter/test/monitoring/tracer_test.erl`, `detecter/test/regeneration/sys_info_parser_test.erl`.
- Test property/specification fixtures live under `detecter/test/props/*.hml`, with matching example properties under `examples/*/props/*.hml`.
- Generated or regenerated Erlang artifacts exist under `detecter/test/regeneration/ebin/*.erl`; treat these as generated/regeneration fixtures rather than the primary hand-written test style. The Makefile excludes this path from hand-written test compilation.

**Naming:**
- Test module filenames and module names use `_test` suffix: `log_tracer_test` in `detecter/test/tracing/log_tracer_test.erl`.
- EUnit generator suites use function names ending in `_test_()`: `tracer_allocation_test_()` and `event_dispatch_test_()` in `detecter/test/tracing/log_tracer_test.erl`.
- Single direct EUnit tests can use `_test()` suffix: `parse_transition_file_test()` in `detecter/test/regeneration/sys_info_parser_test.erl`.

**Structure:**
```text
detecter/test/
├── tracing/             # EUnit tests for tracing modules
│   └── log_tracer_test.erl
├── monitoring/          # EUnit tests for monitor/tracer behavior
│   └── tracer_test.erl
├── regeneration/        # Parser/regeneration tests and generated fixtures
│   ├── sys_info_parser_test.erl
│   └── ebin/
└── props/               # HML property fixtures consumed by tests/examples
```

## Test Structure

**Suite Organization:**
```erlang
-module(log_tracer_test).
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

tracer_allocation_test_() -> {"Tracer allocation test",
  {foreach,
    fun() ->
      log_tracer:start("")
    end,
    fun(_) ->
      log_tracer:stop()
    end,
    [
      fun(_) ->
        {"Subscribing to a tracee once succeeds", ?_test(
          begin
            Result = log_tracer:trace(?P1),
            ?assert(Result),
            ?assertEqual(self(), log_tracer:get_tracer(?P1))
          end)}
      end
    ]}
}.
```

**Patterns:**
- Use EUnit `{foreach, Setup, Cleanup, Tests}` when every case shares one setup value, as in `tracer_allocation_test_()` in `detecter/test/tracing/log_tracer_test.erl`.
- Use EUnit `{foreachx, Setup, Cleanup, Tests}` when each case carries fixture data such as trace event lists, as in `event_dispatch_test_()` in `detecter/test/tracing/log_tracer_test.erl` and `problem_solve_test_()` in `detecter/test/monitoring/tracer_test.erl`.
- Wrap assertions in named test descriptions with `{"description", ?_test(begin ... end)}` so verbose EUnit output is readable.
- Start and stop long-lived processes in setup/cleanup functions. Examples: `log_tracer:start("")` and `log_tracer:stop()` in `detecter/test/tracing/log_tracer_test.erl`; `monitor:start_offline(...)` and `monitor:stop()` in `detecter/test/monitoring/tracer_test.erl`.
- Define test PIDs and timing helpers as macros at the top of the test module: `?P1`, `?P2`, and `?small_wait` in `detecter/test/tracing/log_tracer_test.erl`.

## Mocking

**Framework:** No mocking framework detected.

**Patterns:**
```erlang
-define(P1, c:pid(0, 11, 0)).
-define(P2, c:pid(0, 13, 0)).

Trace = [
  {delay, 0, {send, ?P1, ?P2, msg}},
  {delay, 0, {recv, ?P2, msg}}
],
log_tracer:post_events(Trace),
?assertEqual(Trace, log_tracer:get_backlog()).
```

**What to Mock:**
- Use synthetic Erlang PIDs with `c:pid/3` for tracee identities, as in `detecter/test/tracing/log_tracer_test.erl`.
- Use trace event lists as fixtures instead of live Erlang VM tracing when testing offline routing behavior, as in `detecter/test/monitoring/tracer_test.erl`.
- Use anonymous monitor/MFA spec functions as lightweight test doubles, as in `problem_solve_test_()` in `detecter/test/monitoring/tracer_test.erl`.
- Use temporary files for parser tests where the production API reads files, as in `detecter/test/regeneration/sys_info_parser_test.erl`.

**What NOT to Mock:**
- Do not mock `log_tracer` or `monitor` when testing their process lifecycle; the existing tests start real processes and assert state through public or `-ifdef(TEST)` accessors in `detecter/src/tracing/log_tracer.erl`.
- Do not rely on generated parser internals for hand-written unit tests; test through public parsing APIs such as `sys_info_parser:parse_file/1` in `detecter/test/regeneration/sys_info_parser_test.erl`.

## Fixtures and Factories

**Test Data:**
```erlang
FileContent = [
    "{start, NULL, s0};\n",
    "{s0, -1, s3};\n",
    "{s0, 1, s1};\n"
],
TempFile = "../test_transitions.txt",
file:write_file(TempFile, list_to_binary(FileContent)),
Transitions = sys_info_parser:parse_file(TempFile),
?assertEqual(Expected, Transitions),
file:delete(TempFile).
```

**Location:**
- Inline trace fixtures are declared directly in EUnit generator cases in `detecter/test/tracing/log_tracer_test.erl` and `detecter/test/monitoring/tracer_test.erl`.
- HML property fixtures are stored as files in `detecter/test/props/*.hml`.
- Regeneration tests use local temporary files from within `detecter/test/regeneration/sys_info_parser_test.erl`; new tests should clean up temp files explicitly with `file:delete/1`.

## Coverage

**Requirements:** None enforced. No coverage target, cover config, or CI coverage command is detected.

**View Coverage:**
```bash
# Not configured. Add Erlang cover integration or rebar3 coverage before relying on coverage metrics.
```

## Test Types

**Unit Tests:**
- EUnit module tests cover tracing allocation, event dispatch, backlog behavior, and parser behavior in `detecter/test/tracing/log_tracer_test.erl` and `detecter/test/regeneration/sys_info_parser_test.erl`.
- Some test support depends on `-DTEST` conditional exports from production modules, especially `get_tracer/1` and `get_backlog/0` in `detecter/src/tracing/log_tracer.erl`.
- The default `make test` target currently invokes `log_tracer_test`, `sys_info_parser_test`, and `generated_monitor_smoke_test`.
- `generated_monitor_smoke_test` compiles generated Erlang source for `test/props/prop_no_leak.hml`, `test/props/prop_no_failure.hml`, and `test/props/prop_correct_start.hml`; the matrix covers recursive regeneration plus negative and positive init branches.
- `generated_monitor_smoke_test` also asserts generated sources do not contain the old invalid `update_current_state()` call before compiling them.
- `sys_info_parser_test` contains focused contract tests for START/NULL, integer events, symbolic ranges, set-minus guards, and combined complex events.

**Integration Tests:**
- `detecter/test/monitoring/tracer_test.erl` exercises offline monitor/tracer interaction with real monitor processes and synthetic event streams.
- The active `detecter/Makefile` `test` target documents `tracer_test` as a manual suite, so these integration-style tests are present but not run by default.
- `tracer_test` is intentionally manual because the suite uses `timer:sleep/1` waits to inspect concurrent trace-routing interleavings. Run it manually when changing tracer routing, process deletion, or monitor attachment behavior.

**E2E Tests:**
- No automated end-to-end test framework is detected.
- Example programs in `examples/erlang/`, `examples/elixir/`, and `examples/python/` plus HML properties in `examples/*/props/` serve as manual/demo scenarios rather than configured automated E2E tests.

## Common Patterns

**Async Testing:**
```erlang
{Pid, Ref} = util:promise(fun() -> log_tracer:preempt(?P1) end),
?assert(util:then(Ref)),
?assertEqual(Pid, log_tracer:get_tracer(?P1)).
```

**Error Testing:**
```erlang
Result = log_tracer:preempt(?P1),
?assertNot(Result).
```

**Process Cleanup:**
```erlang
{foreach,
  fun() -> log_tracer:start("") end,
  fun(_) -> log_tracer:stop() end,
  Tests}
```

**Timing-Dependent Tests:**
- Timing waits exist through `?small_wait` in `detecter/test/tracing/log_tracer_test.erl` and `detecter/test/monitoring/tracer_test.erl`.
- `detecter/test/monitoring/tracer_test.erl` documents that the tracer tests are timing-dependent and excluded from the main build in `detecter/Makefile`. New concurrency tests should prefer deterministic synchronization through messages or helper functions such as `util:syn/1`, `util:syn_ack/1`, `util:promise/1`, and `util:then/1` in `detecter/src/util.erl`.
- Before moving `tracer_test` into the default test target, rewrite the assertions to check coarser final monitored state or add deterministic synchronization so the suite does not depend on scheduler timing.

---

*Testing analysis: 2026-06-24; updated 2026-06-26 after Phase 3 completion.*
