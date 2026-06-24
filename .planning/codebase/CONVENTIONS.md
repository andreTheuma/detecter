# Coding Conventions

**Analysis Date:** 2026-06-24

## Naming Patterns

**Files:**
- Use lowercase Erlang module filenames with underscores matching `-module(...)`: `detecter/src/tracing/log_tracer.erl`, `detecter/src/monitoring/shml_eval.erl`, `detecter/src/synthesis/maxhml_eval.erl`.
- Keep tests in `detecter/test/<area>/` with `_test.erl` suffix for EUnit modules: `detecter/test/tracing/log_tracer_test.erl`, `detecter/test/monitoring/tracer_test.erl`, `detecter/test/regeneration/sys_info_parser_test.erl`.
- Generated parser and lexer artifacts live under source directories with conventional Yecc/Leex names: `detecter/src/monitoring/hml_parser.erl`, `detecter/src/tracing/log_lexer.erl`; source grammar files live in `detecter/priv/*.yrl` and `detecter/priv/*.xrl`.
- Python example files use lowercase snake_case module names: `examples/python/src/demo/calc_server.py`, `examples/python/src/demo/calc_server_bug.py`.

**Functions:**
- Use snake_case atoms for Erlang functions: `start_offline/4` in `detecter/src/monitoring/monitor.erl`, `rpc_yield/2` in `detecter/src/util.erl`, `post_events/1` in `detecter/src/tracing/log_tracer.erl`.
- Public APIs are grouped with `-export(...)` declarations before callback/internal exports. Follow the grouped export pattern in `detecter/src/tracing/log_tracer.erl`.
- Callback functions keep OTP names when implementing behaviours: `init/1`, `terminate/2`, `handle_call/3`, `handle_cast/2` in `detecter/src/tracing/log_tracer.erl`.
- Test generator functions use EUnit names ending in `_test_()` for fixture-backed suites, or `_test()` for single tests: `tracer_allocation_test_()` in `detecter/test/tracing/log_tracer_test.erl`, `parse_transition_file_test()` in `detecter/test/regeneration/sys_info_parser_test.erl`.

**Variables:**
- Erlang variables use PascalCase or short domain abbreviations: `Tracee`, `Backlog`, `MfaSpec`, `Opts`, `PidS` in `detecter/src/tracing/log_tracer.erl` and `detecter/src/monitoring/monitor.erl`.
- Ignored variables use leading underscores when a value is intentionally not used: `_E` in dispatch clauses in `detecter/src/tracing/log_tracer.erl`.
- Python constants are uppercase at module scope: `HOST`, `PORT`, `ENC`, `P_ID` in `examples/python/src/demo/calc_server.py`.

**Types:**
- Erlang type names use lowercase snake_case atoms and are exported with `-export_type(...)`: `option/0`, `options/0` in `detecter/src/monitoring/monitor.erl`; `event/0` in `detecter/src/tracing/log_tracer.erl`.
- Records use lowercase names and typed fields: `#alloc{tracee :: pid(), tracer :: pid()}` in `detecter/src/tracing/log_tracer.erl`.
- Macros for constants use uppercase semantic names or lower-case config names according to existing file style: `?ETS_ALLOC_NAME` in `detecter/src/tracing/log_tracer.erl`, `?OPT_PARENT` in `detecter/src/monitoring/monitor.erl`, `?log_level` in `detecter/include/log.hrl`.

## Code Style

**Formatting:**
- No formatter configuration is detected. There is no `.prettierrc`, `eslint.config.*`, `biome.json`, `.erlfmt`, `rebar.config`, or equivalent formatting config in the repository root.
- Erlang source in the core modules uses 2-space indentation for function bodies and aligned multi-line specs, as in `detecter/src/util.erl` and `detecter/src/tracing/log_tracer.erl`.
- New Erlang code should follow the established section header layout: module header, `-module`, `-author`, includes, public API exports, callback/internal exports, type exports, behaviour declarations, macro/record definitions, type definitions, public API, callbacks, private helpers. See `detecter/src/tracing/log_tracer.erl`.
- Python examples use 4-space indentation, module docstrings, lowercase snake_case functions, uppercase constants, and standard library logging, as in `examples/python/src/demo/calc_server.py`.

**Linting:**
- No dedicated lint configuration is detected. Static analysis is available through `make analyze` in `detecter/Makefile`, which runs `dialyzer -pa ebin -I include` over `detecter/src/**/*.erl`.
- Compilation uses `erlc` via `detecter/Makefile`. Production compile uses `-W0`, while test compile omits `-W0` and defines `TEST`.
- Specs are the primary machine-checkable convention. Add `-spec` for exported public APIs and non-trivial private helpers, following `detecter/src/util.erl`, `detecter/src/log.erl`, and `detecter/src/tracing/log_tracer.erl`.

## Import Organization

**Order:**
1. Module metadata: `-module(...)`, `-author(...)`.
2. Includes: standard library include first, project include second, e.g. `-include_lib("stdlib/include/assert.hrl").` then `-include("log.hrl").` in `detecter/src/monitoring/monitor.erl`.
3. Public API exports: grouped by responsibility with multiple `-export(...)` declarations, as in `detecter/src/tracing/log_tracer.erl`.
4. Conditional test-only exports under `-ifdef(TEST).` for internals used by EUnit, as in `detecter/src/tracing/log_tracer.erl`.
5. Callback/internal exports, `-export_type(...)`, and `-behavior(...)`.

**Path Aliases:**
- Not applicable for Erlang. Include paths are passed by `detecter/Makefile` with `-I include`; project headers are referenced as `-include("log.hrl").`.
- Python examples do not define package path aliases. Imports are standard library modules in `examples/python/src/demo/calc_server.py`.

## Error Handling

**Patterns:**
- Prefer Erlang tagged return values for expected operational states: `stop/0 -> ok | {error, not_started}` in `detecter/src/monitoring/monitor.erl`, `start/1 -> {ok, Pid} | {error, {already_started, Pid}}` in `detecter/src/tracing/log_tracer.erl`.
- Use pattern matching to fail fast on unrecoverable file or process setup errors: `{ok, Binary} = file:read_file(FilePath)` in `detecter/src/regeneration/sys_info_parser.erl`, `{ok, Log} = file:open(File, [write])` in `detecter/src/log.erl`.
- Use guards to validate function inputs at clause boundaries: `trace(Tracee) when is_pid(Tracee)` in `detecter/src/tracing/log_tracer.erl`, `start_offline(..., MfaSpec, ...) when is_function(MfaSpec, 1)` in `detecter/src/monitoring/monitor.erl`.
- Use `try ... catch` only around parsing/conversion fallbacks where malformed input is expected: `parse_atom/1`, `parse_event/1`, and `parse_payload/1` in `detecter/src/regeneration/sys_info_parser.erl`.
- Generated Yecc/Leex modules use `throw({error, ...})` and `erlang:error(...)`; avoid copying generated parser error style into hand-written modules unless working inside generated grammar output such as `detecter/src/monitoring/shml_parser.erl`.

## Logging

**Framework:** custom Erlang macros plus Python standard logging.

**Patterns:**
- In Erlang modules, include `detecter/include/log.hrl` and use `?TRACE`, `?DEBUG`, `?INFO`, `?WARN`, or `?ERROR`; the macros call `log:write/4` or `log:write/5` in `detecter/src/log.erl`.
- Logging level is controlled by `?log_level` in `detecter/include/log.hrl`; default is trace-level output when logging is enabled.
- Use format strings and parameter lists rather than preformatted strings in Erlang logs: `?TRACE("Analyzer undefined; discarding trace event ~w.", [Event])` in `detecter/src/monitoring/analyzer.erl`.
- Use direct `io:format` or `io:fwrite` sparingly for user-facing/demo output or logger implementation only, as in `detecter/src/log.erl` and `examples/erlang/src/demo/token_server.erl`.
- Python examples configure `logging.basicConfig(...)`, get `logging.getLogger(__name__)`, and write trace-log lines with `log.debug(...)` in `examples/python/src/demo/calc_server.py`.

## Comments

**When to Comment:**
- Keep EDoc module headers and public API comments for core Erlang modules. Use `%% @doc`, `{@params ...}`, and `{@returns ...}` blocks as shown in `detecter/src/util.erl` and `detecter/src/tracing/log_tracer.erl`.
- Use section banners (`%%% ----------------------------------------------------------------------------`) to separate major module regions in hand-written Erlang files such as `detecter/src/tracing/log_tracer.erl`.
- Explain concurrency, tracing, parser, or protocol assumptions near the relevant code. Examples include backlog dispatch comments in `detecter/src/tracing/log_tracer.erl` and handshake documentation in `detecter/src/util.erl`.
- TODO comments exist and are accepted for known gaps, but keep them specific and local: examples include `detecter/src/synthesis/lin_weaver.erl` and `detecter/src/regeneration/sys_info_parser.erl`.

**JSDoc/TSDoc:**
- Not applicable. Python examples use docstrings with `:param` and `:return:` fields in `examples/python/src/demo/calc_server.py`.

## Function Design

**Size:** Keep public wrappers small and push detailed logic into private helpers when possible. `detecter/src/tracing/log_tracer.erl` exposes compact APIs such as `start/1`, `trace/1`, and `post_events/1`, with dispatch and ETS logic in private helpers.

**Parameters:** Use explicit domain names and typed `-spec` annotations. Prefer option lists for extensible configuration: `start_offline(File, PidS, MfaSpec, Opts)` in `detecter/src/monitoring/monitor.erl`.

**Return Values:** Use stable Erlang return shapes: `ok`, booleans for simple success checks, tagged tuples for start/error cases, and PIDs for spawned processes. Document return values in `%% @doc` and `-spec` blocks.

## Module Design

**Exports:** Keep public API exports narrow. Export test-only internals under `-ifdef(TEST)` when EUnit needs white-box access, as with `get_tracer/1` and `get_backlog/0` in `detecter/src/tracing/log_tracer.erl`.

**Barrel Files:** Not used. Erlang modules are referenced directly by module atom and compiled together through `detecter/Makefile`.

---

*Convention analysis: 2026-06-24*
