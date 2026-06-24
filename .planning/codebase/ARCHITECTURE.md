<!-- refreshed: 2026-06-24 -->
# Architecture

**Analysis Date:** 2026-06-24

## System Overview

```text
┌─────────────────────────────────────────────────────────────┐
│                    Public Runtime Interface                  │
├──────────────────┬──────────────────┬───────────────────────┤
│ Online monitor   │ Offline monitor  │ Source instrumentation │
│ `detecter/src/monitoring/monitor.erl`                       │
│ `detecter/src/monitoring/weaver.erl`                        │
│ `detecter/src/synthesis/lin_weaver.erl`                     │
└────────┬─────────┴────────┬─────────┴──────────┬────────────┘
         │                  │                     │
         ▼                  ▼                     ▼
┌─────────────────────────────────────────────────────────────┐
│                    Tracing and Routing Layer                 │
│ `detecter/src/monitoring/tracer.erl`                         │
│ `detecter/src/tracing/trace_lib.erl`                         │
│ `detecter/src/tracing/evm_tracer.erl`                        │
│ `detecter/src/tracing/log_tracer.erl`                        │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                    Analyzer / Synthesis Layer                │
│ `detecter/src/monitoring/analyzer.erl`                       │
│ `detecter/src/monitoring/hml_eval.erl`                       │
│ `detecter/src/synthesis/maxhml_eval.erl`                     │
│ `detecter/src/synthesis/gen_eval.erl`                        │
│ `detecter/src/synthesis/lin_analyzer.erl`                    │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│ Lexer/parser specs, generated modules, examples, tests       │
│ `detecter/priv/*.xrl`, `detecter/priv/*.yrl`                 │
│ `detecter/src/**/*_lexer.erl`, `detecter/src/**/*_parser.erl`│
│ `examples/`, `detecter/test/`, `tutorial/`                   │
└─────────────────────────────────────────────────────────────┘
```

## Component Responsibilities

| Component | Responsibility | File |
|-----------|----------------|------|
| Monitor facade | Starts online or offline monitoring, selects trace backend, starts root tracer, and stops trace services. | `detecter/src/monitoring/monitor.erl` |
| Tracer process tree | Owns per-process trace routing, dynamic tracer spawning, direct/priority modes, detach routing, and analyzer dispatch. | `detecter/src/monitoring/tracer.erl` |
| Analyzer process | Stores a monitor continuation in the process dictionary, consumes normalized events, and reports irrevocable verdicts. | `detecter/src/monitoring/analyzer.erl` |
| Trace facade | Hides the active tracing backend behind `start/1`, `trace/1`, `clear/1`, and `preempt/1`; stores selected backend in `persistent_term`. | `detecter/src/tracing/trace_lib.erl` |
| EVM trace adapter | Wraps `erlang:trace/3`, trace patterns, and trace preemption for live Erlang systems. | `detecter/src/tracing/evm_tracer.erl` |
| Log trace adapter | Implements a `gen_server` that allocates log trace events to tracer processes and maintains delivery backlog. | `detecter/src/tracing/log_tracer.erl` |
| Log poller | Polls a trace file, parses each completed line, and posts parsed events to `log_tracer`. | `detecter/src/tracing/log_poller.erl` |
| Generic file poller | Provides a reusable polling behaviour with line callbacks and sys-compatible process control. | `detecter/src/behavior/gen_file_poller.erl` |
| Offline event parser | Converts textual log lines into internal trace events. | `detecter/src/tracing/log_eval.erl`, `detecter/src/tracing/log_lexer.erl`, `detecter/src/tracing/log_parser.erl` |
| Event normalizer | Converts internal fork/init/exit/send/recv tuples to monitor-level EVM events. | `detecter/src/tracing/event.erl` |
| HML compiler | Parses `.hml` property files and generates Erlang monitor code for the classic monitoring path. | `detecter/src/monitoring/hml_eval.erl` |
| maxHML compiler | Parses `.hml` maxHML specs and generates modular analyzer code for the synthesis path. | `detecter/src/synthesis/maxhml_eval.erl` |
| Shared generator | Provides parser/lexer compilation pipeline, Erlang syntax generation helpers, and generated module output. | `detecter/src/synthesis/gen_eval.erl` |
| Linear analyzer | Executes small-step monitor semantics and proof derivation tracking for synthesized monitor terms. | `detecter/src/synthesis/lin_analyzer.erl` |
| Weavers | Transform Erlang ASTs to inject monitor dispatch calls into source or generated linear-time instrumentation. | `detecter/src/monitoring/weaver.erl`, `detecter/src/synthesis/lin_weaver.erl` |
| Build helper | Generates lexer/parser Erlang modules from `priv` specifications and builds EDoc output. | `detecter/src/build.erl` |
| Utility functions | Provides RPC-style messaging, synchronization helpers, promises, and path helpers. | `detecter/src/util.erl` |
| Logging macros/runtime | Provides `?TRACE`, `?INFO`, `?WARN`, `?ERROR` logging macros and file logging helpers. | `detecter/include/log.hrl`, `detecter/src/log.erl` |
| Regeneration parser | Parses system information specs used by regeneration tests and generated monitor metadata. | `detecter/src/regeneration/sys_info_parser.erl` |

## Pattern Overview

**Overall:** Erlang actor pipeline with generated monitor code and pluggable trace backends.

**Key Characteristics:**
- Use process-per-tracer and optional process-per-analyzer; route trace events through Erlang mailboxes in `detecter/src/monitoring/tracer.erl`.
- Keep trace backend calls behind `trace_lib`; use `evm_tracer` for live systems and `log_tracer` plus `log_poller` for replay.
- Compile property specifications to Erlang modules, then pass generated monitor functions through `analyzer:mfa_spec()` mappings.
- Treat lexer/parser modules in `detecter/src/monitoring/`, `detecter/src/synthesis/`, and `detecter/src/tracing/` as generated from `detecter/priv/*.xrl` and `detecter/priv/*.yrl`.

## Layers

**Runtime facade:**
- Purpose: Provide the main monitoring API and hide online/offline startup mechanics.
- Location: `detecter/src/monitoring/monitor.erl`
- Contains: `start_online/3`, `start_offline/4`, `stop/0`, option extraction.
- Depends on: `trace_lib`, `tracer`, `util`, `opts`, `log.hrl`.
- Used by: Examples, tests, and clients that monitor a system under test.

**Tracer routing layer:**
- Purpose: Collect, route, and order trace events across dynamically created tracer processes.
- Location: `detecter/src/monitoring/tracer.erl`
- Contains: root tracer startup, child tracer startup, direct/priority loops, routing maps, process maps, test-only ETS mapping tables.
- Depends on: `trace_lib`, `analyzer`, `event`, `util`, `log.hrl`.
- Used by: `monitor:start_online/3`, `monitor:start_offline/4`, and child tracer spawns within `tracer`.

**Trace backend layer:**
- Purpose: Provide a uniform tracing API for live EVM tracing and file-backed trace replay.
- Location: `detecter/src/tracing/`
- Contains: `trace_lib.erl`, `evm_tracer.erl`, `log_tracer.erl`, `log_poller.erl`, `log_eval.erl`, generated log lexer/parser modules.
- Depends on: Erlang VM tracing, `gen_server`, `gen_file_poller`, `persistent_term`, ETS.
- Used by: `monitor` and `tracer`.

**Analyzer layer:**
- Purpose: Apply monitor continuations to normalized events and detect verdict states.
- Location: `detecter/src/monitoring/analyzer.erl`, `detecter/src/synthesis/lin_analyzer.erl`
- Contains: process dictionary monitor storage, dispatch functions, verdict handling, small-step reduction for linear synthesis.
- Depends on: `event:to_evm_event/1`, generated monitor functions, `log.hrl`.
- Used by: `tracer` and AST weavers.

**Specification compiler layer:**
- Purpose: Parse HML/maxHML specifications and produce Erlang monitor modules and MFA lookup functions.
- Location: `detecter/src/monitoring/hml_eval.erl`, `detecter/src/synthesis/maxhml_eval.erl`, `detecter/src/synthesis/gen_eval.erl`
- Contains: parser invocations, AST visitors, Erlang syntax tree generation, compiler option handling.
- Depends on: generated lexers/parsers, `syntax_tools`, Erlang compiler APIs.
- Used by: example Makefiles, tests, and users synthesizing analyzers from `.hml` files.

**Instrumentation layer:**
- Purpose: Rewrite Erlang ASTs to inject event dispatch and monitor logic around selected remote function calls or events.
- Location: `detecter/src/monitoring/weaver.erl`, `detecter/src/synthesis/lin_weaver.erl`
- Contains: recursive file discovery, parse transform callbacks, compile/load output handling.
- Depends on: `opts`, `analyzer`, `lin_analyzer`, compiler forms.
- Used by: examples and workflows that generate instrumented source or beam files.

**Generated parser source layer:**
- Purpose: Define grammar and lexer specs for trace logs, HML, SHML, maxHML, and system info.
- Location: `detecter/priv/`
- Contains: `*.xrl`, `*.yrl`, `sys_info.spec`, EDoc CSS, trace fixture logs.
- Depends on: Erlang `leex` and `yecc`.
- Used by: `build:leex/0`, `build:yecc/0`, generated parser modules, tests.

## Data Flow

### Online Monitoring Path

1. Client calls `monitor:start_online({Mod, Fun, Args}, MfaSpec, Opts)` (`detecter/src/monitoring/monitor.erl:83`).
2. `monitor` starts live tracing through `trace_lib:start(evm)` (`detecter/src/tracing/trace_lib.erl:115`).
3. `monitor` spawns a bootstrap process that applies `{Mod, Fun, Args}` only after synchronization (`detecter/src/monitoring/monitor.erl:91`).
4. `tracer:start/4` spawns a root tracer (`detecter/src/monitoring/tracer.erl:167`).
5. `tracer:root/5` calls `trace_lib:trace(PidS)` and enters direct routing mode (`detecter/src/monitoring/tracer.erl:232`).
6. Trace events arrive as Erlang messages; `tracer:loop/4` dispatches direct or routed events (`detecter/src/monitoring/tracer.erl:386`, `detecter/src/monitoring/tracer.erl:417`).
7. Spawn events that match `MfaSpec` create child tracers and analyzers through `tracer:tracer/6` (`detecter/src/monitoring/tracer.erl:296`).
8. Events are sent to `analyzer:dispatch/1` or an analyzer process, which normalizes via `event:to_evm_event/1` and updates monitor state (`detecter/src/monitoring/analyzer.erl:157`, `detecter/src/monitoring/analyzer.erl:193`).

### Offline Log Replay Path

1. Client calls `monitor:start_offline(File, PidS, MfaSpec, Opts)` (`detecter/src/monitoring/monitor.erl:135`).
2. `monitor` starts file-backed tracing through `trace_lib:start({log, File})` (`detecter/src/tracing/trace_lib.erl:117`).
3. `log_tracer:start/1` starts the registered `gen_server` (`detecter/src/tracing/log_tracer.erl:215`).
4. `log_poller:handle_line/3` parses each line with `log_eval:eval_string/2` and calls `log_tracer:post_event/1` (`detecter/src/tracing/log_poller.erl:139`).
5. `log_tracer` allocates tracees to tracer PIDs through `trace/1`, `preempt/1`, and event dispatch (`detecter/src/tracing/log_tracer.erl:251`, `detecter/src/tracing/log_tracer.erl:281`, `detecter/src/tracing/log_tracer.erl:762`).
6. `tracer` receives replayed events using the same routing/analyzer path as online monitoring.

### Specification Compilation Path

1. A property file in `detecter/test/props/` or `examples/*/props/` is compiled by `hml_eval:compile/2` or `maxhml_eval:compile/2` (`detecter/src/monitoring/hml_eval.erl:265`, `detecter/src/synthesis/maxhml_eval.erl:149`).
2. `gen_eval:compile/5` coordinates lexer/parser modules, Erlang syntax generation, and output compilation (`detecter/src/synthesis/gen_eval.erl:299`).
3. Generated monitor modules expose lookup functions such as `mfa_spec`, referenced by monitor startup and weavers (`detecter/src/synthesis/gen_eval.erl`).
4. Generated beams or Erlang source are written to configured output directories such as `examples/erlang/ebin/` or `detecter/test/regeneration/ebin/`.

### Instrumentation Path

1. Call `weaver:weave/3`, `weaver:weave_file/3`, `lin_weaver:weave/3`, or `lin_weaver:weave_file/3` (`detecter/src/monitoring/weaver.erl:149`, `detecter/src/monitoring/weaver.erl:211`, `detecter/src/synthesis/lin_weaver.erl:144`, `detecter/src/synthesis/lin_weaver.erl:190`).
2. The weaver recursively discovers `.erl` files, transforms abstract forms, and writes compiled output.
3. Injected calls target `analyzer:dispatch/1` for classic monitoring or `lin_analyzer:dispatch/1` for linear-time analysis (`detecter/src/monitoring/weaver.erl`, `detecter/src/synthesis/lin_weaver.erl`).
4. Successful output modules are loaded into the Erlang code path for execution.

**State Management:**
- `trace_lib` stores the selected tracing module in `persistent_term` under a module key (`detecter/src/tracing/trace_lib.erl`).
- `tracer` keeps per-process `routes`, `traced`, `trace`, and `stats` in a local record state (`detecter/src/monitoring/tracer.erl`).
- Analyzer monitor continuations are stored in the process dictionary under `'$monitor'` (`detecter/src/monitoring/analyzer.erl`, `detecter/src/synthesis/lin_analyzer.erl`).
- `log_tracer` uses ETS for tracee-to-tracer allocation (`detecter/src/tracing/log_tracer.erl`).
- Test builds use ETS tables for tracer/process mappings under `-DTEST` (`detecter/src/monitoring/tracer.erl`).

## Key Abstractions

**`analyzer:mfa_spec()` mapping:**
- Purpose: Decide whether a spawned `{Module, Function, Args}` gets a monitor and return `{ok, MonitorFun}` or `undefined`.
- Examples: `detecter/src/monitoring/analyzer.erl`, generated modules in `examples/erlang/ebin/*.erl`, generated modules in `detecter/test/regeneration/ebin/*.erl`.
- Pattern: Pass a function into `monitor:start_online/3`, `monitor:start_offline/4`, and weaver functions rather than hard-coding monitored MFAs.

**Trace backend facade:**
- Purpose: Swap live EVM tracing and log replay without changing tracer routing code.
- Examples: `detecter/src/tracing/trace_lib.erl`, `detecter/src/tracing/evm_tracer.erl`, `detecter/src/tracing/log_tracer.erl`.
- Pattern: Add new backends by implementing `start`, `stop`, `trace`, `clear`, and `preempt`, then registering the module in `trace_lib`.

**Tracer modes:**
- Purpose: Preserve causal event ordering while child tracers detach from ancestor tracers.
- Examples: `detecter/src/monitoring/tracer.erl`.
- Pattern: Use `direct` mode for tracers consuming direct trace events; use `priority` mode for child tracers that must process routed ancestor events before direct events.

**Generated lexers/parsers:**
- Purpose: Keep grammar specs in `priv` and generated Erlang modules in source directories where they are compiled with the project.
- Examples: `detecter/priv/hml_lexer.xrl`, `detecter/priv/hml_parser.yrl`, `detecter/src/monitoring/hml_lexer.erl`, `detecter/src/monitoring/hml_parser.erl`.
- Pattern: Edit `detecter/priv/*.xrl` or `detecter/priv/*.yrl`, then regenerate with `build:leex/0` or `build:yecc/0`.

**Generic file poller behaviour:**
- Purpose: Reusable tail-like file polling with line callbacks.
- Examples: `detecter/src/behavior/gen_file_poller.erl`, `detecter/src/tracing/log_poller.erl`, `detecter/src/behavior/gen_file_poller_impl.erl`.
- Pattern: Implement `init/1`, `handle_line/3`, and optionally `terminate/2` in a module that declares `-behavior(gen_file_poller)`.

## Entry Points

**Online monitoring:**
- Location: `detecter/src/monitoring/monitor.erl:83`
- Triggers: Direct Erlang calls to `monitor:start_online/3`.
- Responsibilities: Start EVM tracing, bootstrap target MFA, start root tracer, coordinate startup synchronization, return `{ok, Root, Return}`.

**Offline monitoring:**
- Location: `detecter/src/monitoring/monitor.erl:135`
- Triggers: Direct Erlang calls to `monitor:start_offline/4`.
- Responsibilities: Start log tracing, attach root tracer to top-level process PID, synchronize startup.

**Trace backend API:**
- Location: `detecter/src/tracing/trace_lib.erl:115`
- Triggers: Called by `monitor`.
- Responsibilities: Start selected backend, route trace/clear/preempt calls to active backend, prevent double-starts.

**Root tracer process:**
- Location: `detecter/src/monitoring/tracer.erl:232`
- Triggers: Spawned by `tracer:start/4`.
- Responsibilities: Become tracer for the root process, initialize state, enter direct loop.

**Child tracer process:**
- Location: `detecter/src/monitoring/tracer.erl:296`
- Triggers: Spawned on matching process lifecycle events.
- Responsibilities: Create or embed analyzer, detach from router tracer, enter priority loop.

**HML compilation:**
- Location: `detecter/src/monitoring/hml_eval.erl:265`
- Triggers: Direct Erlang calls or example/test Makefiles.
- Responsibilities: Generate classic monitor modules from HML property files.

**maxHML compilation:**
- Location: `detecter/src/synthesis/maxhml_eval.erl:149`
- Triggers: Direct Erlang calls or synthesis workflows.
- Responsibilities: Generate modular monitor/analyzer code from maxHML property files.

**Build-time parser generation:**
- Location: `detecter/src/build.erl:110`, `detecter/src/build.erl:120`
- Triggers: Erlang shell calls to `build:leex/0` and `build:yecc/0`.
- Responsibilities: Regenerate selected lexer/parser modules from `detecter/priv/`.

**EUnit test entry:**
- Location: `detecter/Makefile`
- Triggers: `make test` from `detecter/`.
- Responsibilities: Compile source and tests with `-DTEST`, then run `log_tracer_test`.

## Architectural Constraints

- **Threading:** The system uses Erlang lightweight processes and mailbox ordering; tracer correctness depends on direct and routed mailbox receive ordering in `detecter/src/monitoring/tracer.erl`.
- **Global state:** `trace_lib` uses `persistent_term` for active trace backend; `maxhml_eval` and generated synthesis code use `persistent_term` for generated function metadata; analyzers use process dictionaries under `'$monitor'`.
- **Registered processes:** `log_tracer` registers a local `gen_server` under its module name; `log_poller` registers under its module name through `gen_file_poller`.
- **Test-only state:** `tracer` creates ETS tables only when compiled with `-DTEST`; do not rely on `get_mon_info/*` or related APIs in production builds.
- **Generated source:** Parser and lexer modules in `detecter/src/**` are generated; modify their `detecter/priv/*.xrl` or `detecter/priv/*.yrl` sources instead.
- **Build model:** There is no OTP application/supervision tree; `detecter/Makefile` compiles all `detecter/src/**/*.erl` files into `detecter/ebin`.
- **Circular imports:** No explicit circular module import system exists in Erlang, but runtime call cycles exist between `tracer`, `trace_lib`, trace backends, and `analyzer`; keep APIs small and message protocols stable.
- **Scope of examples:** `examples/erlang/`, `examples/elixir/`, and `examples/python/` are demonstration clients and generated outputs, not core runtime code.

## Anti-Patterns

### Editing Generated Lexer/Parser Modules

**What happens:** Code is changed directly in generated modules such as `detecter/src/monitoring/hml_lexer.erl`, `detecter/src/tracing/log_parser.erl`, or `detecter/src/synthesis/maxhml_parser.erl`.
**Why it's wrong:** Regeneration from `detecter/priv/*.xrl` or `detecter/priv/*.yrl` can overwrite manual edits and diverge from the grammar source.
**Do this instead:** Change the grammar files in `detecter/priv/` and regenerate through `detecter/src/build.erl`.

### Bypassing `trace_lib`

**What happens:** Code calls `evm_tracer` or `log_tracer` directly from monitoring or analyzer modules.
**Why it's wrong:** It couples monitoring logic to one trace backend and bypasses the active-backend state in `trace_lib`.
**Do this instead:** Route backend operations through `trace_lib:start/1`, `trace_lib:trace/1`, `trace_lib:clear/1`, and `trace_lib:preempt/1`.

### Adding Monitor State Outside Analyzer Continuations

**What happens:** New monitor logic stores state in global ETS tables or process globals instead of the analyzer continuation.
**Why it's wrong:** The analyzer API expects the current monitor continuation to live under `'$monitor'` in the analyzer/tracer process dictionary.
**Do this instead:** Preserve the continuation-passing pattern in `detecter/src/monitoring/analyzer.erl` and `detecter/src/synthesis/lin_analyzer.erl`.

### Skipping Startup Synchronization

**What happens:** New online monitoring startup executes target code before `tracer:root/5` is tracing the root process.
**Why it's wrong:** Early events can be lost before the root tracer is installed.
**Do this instead:** Use `util:syn/1` and `util:syn_ack/1` as in `monitor:start_online/3` and `tracer:root/5`.

## Error Handling

**Strategy:** Erlang fail-fast assertions and process exits are used for internal invariants; public API functions generally return `ok`, `{ok, Pid}`, `{error, Reason}`, or booleans depending on the subsystem.

**Patterns:**
- Use `?assert` and `?assertEqual` for tracer/analyzer invariants in `detecter/src/monitoring/tracer.erl` and `detecter/src/monitoring/analyzer.erl`.
- Catch `badarg` around EVM tracing operations and log warnings in `detecter/src/tracing/evm_tracer.erl`.
- Return parser/compiler tuples from compile pipelines in `detecter/src/synthesis/gen_eval.erl`.
- Raise on unrecoverable setup failures such as output directory creation in `detecter/src/monitoring/weaver.erl`.

## Cross-Cutting Concerns

**Logging:** Use macros from `detecter/include/log.hrl`; runtime write helpers live in `detecter/src/log.erl`. Existing modules use `?TRACE`, `?INFO`, `?WARN`, and `?ERROR`.

**Validation:** Use EUnit assertions in tests, Erlang guard clauses in public functions, and internal `?assert` checks in tracer/analyzer loops.

**Authentication:** Not applicable; this is a local runtime verification tool with no network authentication layer.

---

*Architecture analysis: 2026-06-24*
