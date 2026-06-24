# Codebase Structure

**Analysis Date:** 2026-06-24

## Directory Layout

```
detecter/
├── README.md                    # Project overview and release links
├── LICENSE                      # GPL license
├── detecter/                    # Erlang runtime verification tool
│   ├── Makefile                 # Compile, test, clean, Dialyzer targets
│   ├── include/                 # Shared Erlang headers and macros
│   ├── priv/                    # Lexer/parser specs, fixtures, EDoc assets
│   ├── src/                     # Core Erlang implementation
│   │   ├── behavior/            # Custom generic behaviours
│   │   ├── examples/            # Minimal Erlang example module
│   │   ├── monitoring/          # Classic runtime monitoring pipeline
│   │   ├── regeneration/        # System-info parser utilities
│   │   ├── synthesis/           # maxHML and linear-time synthesis pipeline
│   │   └── tracing/             # EVM/log tracing backends and parsers
│   ├── test/                    # EUnit tests, HML fixtures, generated test output
│   └── ebin/                    # Compiled beam output
├── examples/                    # Erlang, Elixir, and Python demo systems
│   ├── erlang/
│   ├── elixir/
│   └── python/
└── tutorial/                    # MkDocs tutorial site and Python lexer helpers
    ├── docs/
    ├── includes/
    ├── maxhml_lexer/
    └── shml_lexer/
```

## Directory Purposes

**`detecter/src/`:**
- Purpose: Core Erlang code compiled by `detecter/Makefile`.
- Contains: Runtime modules, tracing modules, generated lexers/parsers, synthesis modules, utility modules.
- Key files: `detecter/src/monitoring/monitor.erl`, `detecter/src/monitoring/tracer.erl`, `detecter/src/tracing/trace_lib.erl`, `detecter/src/synthesis/gen_eval.erl`.

**`detecter/src/monitoring/`:**
- Purpose: Classic runtime monitoring flow.
- Contains: Monitor facade, tracer process tree, analyzer process, AST weaver, HML/SHML generated parsers and evaluators, option helpers.
- Key files: `detecter/src/monitoring/monitor.erl`, `detecter/src/monitoring/tracer.erl`, `detecter/src/monitoring/analyzer.erl`, `detecter/src/monitoring/weaver.erl`, `detecter/src/monitoring/hml_eval.erl`.

**`detecter/src/tracing/`:**
- Purpose: Trace backend abstraction and event parsing.
- Contains: Live EVM tracer, file-backed log tracer, log poller, log parser/evaluator, event conversion.
- Key files: `detecter/src/tracing/trace_lib.erl`, `detecter/src/tracing/evm_tracer.erl`, `detecter/src/tracing/log_tracer.erl`, `detecter/src/tracing/log_poller.erl`, `detecter/src/tracing/event.erl`.

**`detecter/src/synthesis/`:**
- Purpose: maxHML monitor synthesis and linear-time analyzer generation.
- Contains: maxHML generated parser/lexer, shared code generator, linear analyzer, linear weaver, event writer.
- Key files: `detecter/src/synthesis/maxhml_eval.erl`, `detecter/src/synthesis/gen_eval.erl`, `detecter/src/synthesis/lin_analyzer.erl`, `detecter/src/synthesis/lin_weaver.erl`.

**`detecter/src/behavior/`:**
- Purpose: Reusable Erlang behaviour implementations.
- Contains: Generic file poller behaviour and a simple implementation module.
- Key files: `detecter/src/behavior/gen_file_poller.erl`, `detecter/src/behavior/gen_file_poller_impl.erl`.

**`detecter/src/regeneration/`:**
- Purpose: Parser support for system information specs used by regeneration workflows.
- Contains: System info parser.
- Key files: `detecter/src/regeneration/sys_info_parser.erl`.

**`detecter/src/examples/`:**
- Purpose: Small in-core demonstration module used with the runtime.
- Contains: Simple Erlang example.
- Key files: `detecter/src/examples/simple.erl`.

**`detecter/include/`:**
- Purpose: Shared Erlang include files.
- Contains: Logging and development macros.
- Key files: `detecter/include/log.hrl`, `detecter/include/dev.hrl`.

**`detecter/priv/`:**
- Purpose: Non-source runtime/build assets.
- Contains: Leex lexer specs, Yecc parser specs, system info spec, EDoc CSS, trace fixture logs.
- Key files: `detecter/priv/hml_lexer.xrl`, `detecter/priv/hml_parser.yrl`, `detecter/priv/log_lexer.xrl`, `detecter/priv/log_parser.yrl`, `detecter/priv/maxhml_lexer.xrl`, `detecter/priv/maxhml_parser.yrl`, `detecter/priv/sys_info.spec`.

**`detecter/test/`:**
- Purpose: EUnit tests, property fixtures, and generated regeneration outputs.
- Contains: Test modules under feature directories, `.hml` property fixtures, generated `.erl` and `.beam` outputs.
- Key files: `detecter/test/tracing/log_tracer_test.erl`, `detecter/test/monitoring/tracer_test.erl`, `detecter/test/regeneration/sys_info_parser_test.erl`, `detecter/test/props/prop_add.hml`.

**`examples/`:**
- Purpose: End-user sample systems for Erlang, Elixir, and Python.
- Contains: Demo source modules, property files, Makefiles, generated beams.
- Key files: `examples/erlang/Makefile`, `examples/erlang/src/demo/calc_server.erl`, `examples/elixir/lib/demo/calc_server.ex`, `examples/python/src/demo/calc_server.py`.

**`tutorial/`:**
- Purpose: Documentation site and tutorial support tooling.
- Contains: MkDocs config, Markdown docs, includes, stylesheet, Python lexer package setup files.
- Key files: `tutorial/mkdocs.yml`, `tutorial/docs/index.md`, `tutorial/setup.py`, `tutorial/setup-maxhml.py`.

**`.planning/codebase/`:**
- Purpose: GSD-generated codebase intelligence.
- Contains: Architecture, structure, stack, testing, conventions, concerns documents.
- Key files: `.planning/codebase/ARCHITECTURE.md`, `.planning/codebase/STRUCTURE.md`.

## Key File Locations

**Entry Points:**
- `detecter/src/monitoring/monitor.erl`: Main online/offline monitoring API.
- `detecter/src/monitoring/weaver.erl`: Classic instrumentation API for Erlang modules.
- `detecter/src/synthesis/lin_weaver.erl`: Linear-time instrumentation API.
- `detecter/src/monitoring/hml_eval.erl`: Classic HML property compiler.
- `detecter/src/synthesis/maxhml_eval.erl`: maxHML synthesis compiler.
- `detecter/src/build.erl`: EDoc and lexer/parser generation helpers.
- `detecter/Makefile`: Shell entry for compile/test/analyze tasks.

**Configuration:**
- `detecter/Makefile`: Defines `BIN=ebin`, `INCLUDE=include`, `SRC=src`, and `TEST=test`.
- `detecter/include/log.hrl`: Logging macro behavior and compile-time logging controls.
- `detecter/include/dev.hrl`: Development/test macro helpers.
- `tutorial/mkdocs.yml`: Tutorial site configuration.
- `tutorial/requirements.txt`: Python documentation dependencies.

**Core Logic:**
- `detecter/src/monitoring/tracer.erl`: Trace routing state machine.
- `detecter/src/monitoring/analyzer.erl`: Classic monitor event dispatch and verdict detection.
- `detecter/src/tracing/trace_lib.erl`: Active trace backend facade.
- `detecter/src/tracing/evm_tracer.erl`: EVM tracing implementation.
- `detecter/src/tracing/log_tracer.erl`: Log replay tracing implementation.
- `detecter/src/tracing/log_poller.erl`: File polling bridge into log replay.
- `detecter/src/synthesis/lin_analyzer.erl`: Linear-time small-step analysis engine.
- `detecter/src/synthesis/gen_eval.erl`: Shared specification-to-Erlang code generation.

**Generated Parser Sources:**
- `detecter/priv/hml_lexer.xrl` and `detecter/priv/hml_parser.yrl`: Source specs for `detecter/src/monitoring/hml_lexer.erl` and `detecter/src/monitoring/hml_parser.erl`.
- `detecter/priv/shml_lexer.xrl` and `detecter/priv/shml_parser.yrl`: Source specs for `detecter/src/monitoring/shml_lexer.erl` and `detecter/src/monitoring/shml_parser.erl`.
- `detecter/priv/log_lexer.xrl` and `detecter/priv/log_parser.yrl`: Source specs for `detecter/src/tracing/log_lexer.erl` and `detecter/src/tracing/log_parser.erl`.
- `detecter/priv/maxhml_lexer.xrl` and `detecter/priv/maxhml_parser.yrl`: Source specs for `detecter/src/synthesis/maxhml_lexer.erl` and `detecter/src/synthesis/maxhml_parser.erl`.

**Testing:**
- `detecter/test/tracing/log_tracer_test.erl`: EUnit tests currently run by `make test`.
- `detecter/test/monitoring/tracer_test.erl`: Tracer tests present but not enabled in the default `make test` target.
- `detecter/test/regeneration/sys_info_parser_test.erl`: Regeneration parser tests.
- `detecter/test/props/*.hml`: HML property fixtures.
- `detecter/priv/trace_test*.log`: Offline trace fixtures.

**Examples and Docs:**
- `examples/erlang/src/demo/*.erl`: Erlang demo system modules.
- `examples/erlang/props/*.hml`: Erlang demo properties.
- `examples/elixir/lib/demo/*.ex`: Elixir demo system modules.
- `examples/python/src/demo/*.py`: Python demo system modules.
- `tutorial/docs/**/*.md`: Tutorial pages.

## Naming Conventions

**Files:**
- Erlang modules use snake_case filenames matching module names: `detecter/src/monitoring/log_tracer.erl`, `detecter/src/synthesis/lin_analyzer.erl`.
- Erlang headers use `.hrl`: `detecter/include/log.hrl`.
- Lexer specs use `*_lexer.xrl` and generated lexer modules use `*_lexer.erl`: `detecter/priv/log_lexer.xrl`, `detecter/src/tracing/log_lexer.erl`.
- Parser specs use `*_parser.yrl` and generated parser modules use `*_parser.erl`: `detecter/priv/maxhml_parser.yrl`, `detecter/src/synthesis/maxhml_parser.erl`.
- Property files use `prop_*.hml`: `detecter/test/props/prop_no_failure.hml`.
- Tests use `*_test.erl`: `detecter/test/tracing/log_tracer_test.erl`.

**Directories:**
- Core Erlang feature areas are grouped by architectural role under `detecter/src/<area>/`: `monitoring`, `tracing`, `synthesis`, `behavior`, `regeneration`.
- Example languages are grouped under `examples/<language>/`: `examples/erlang`, `examples/elixir`, `examples/python`.
- Tutorial content follows MkDocs conventions under `tutorial/docs/` and reusable snippets under `tutorial/includes/`.

## Where to Add New Code

**New monitoring API:**
- Primary code: `detecter/src/monitoring/monitor.erl`
- Supporting tracer changes: `detecter/src/monitoring/tracer.erl`
- Tests: `detecter/test/monitoring/*_test.erl`

**New trace backend:**
- Primary code: `detecter/src/tracing/<backend>_tracer.erl`
- Backend registration: `detecter/src/tracing/trace_lib.erl`
- Tests: `detecter/test/tracing/*_test.erl`
- Required API: implement `start`, `stop`, `trace`, `clear`, and `preempt` functions compatible with `trace_lib`.

**New trace log syntax:**
- Grammar source: `detecter/priv/log_lexer.xrl`, `detecter/priv/log_parser.yrl`
- Evaluator changes: `detecter/src/tracing/log_eval.erl`
- Generated modules: `detecter/src/tracing/log_lexer.erl`, `detecter/src/tracing/log_parser.erl`
- Tests/fixtures: `detecter/priv/trace_test*.log`, `detecter/test/tracing/*_test.erl`

**New HML or maxHML language feature:**
- Classic HML grammar: `detecter/priv/hml_lexer.xrl`, `detecter/priv/hml_parser.yrl`
- Classic evaluator: `detecter/src/monitoring/hml_eval.erl`
- maxHML grammar: `detecter/priv/maxhml_lexer.xrl`, `detecter/priv/maxhml_parser.yrl`
- maxHML evaluator: `detecter/src/synthesis/maxhml_eval.erl`
- Shared generator helpers: `detecter/src/synthesis/gen_eval.erl`
- Property fixtures: `detecter/test/props/*.hml`, `examples/*/props/*.hml`

**New analyzer semantics:**
- Classic continuation analyzer: `detecter/src/monitoring/analyzer.erl`
- Linear-time semantics: `detecter/src/synthesis/lin_analyzer.erl`
- Tests: `detecter/test/monitoring/*_test.erl`, `detecter/test/tracing/*_test.erl`

**New AST instrumentation:**
- Classic instrumentation: `detecter/src/monitoring/weaver.erl`
- Linear-time instrumentation: `detecter/src/synthesis/lin_weaver.erl`
- Option parsing: `detecter/src/monitoring/opts.erl`
- Tests: add EUnit tests under `detecter/test/monitoring/` or regeneration fixtures under `detecter/test/regeneration/`.

**New reusable polling behavior user:**
- Implementation: `detecter/src/<area>/<name>_poller.erl`
- Behaviour dependency: `detecter/src/behavior/gen_file_poller.erl`
- Pattern reference: `detecter/src/tracing/log_poller.erl`

**New utility:**
- Shared Erlang helper: `detecter/src/util.erl`
- Logging helper: `detecter/src/log.erl`, `detecter/include/log.hrl`
- Keep feature-specific helpers inside the relevant area module when they are not reused.

**New Erlang example:**
- Source: `examples/erlang/src/demo/<name>.erl`
- Properties: `examples/erlang/props/prop_<name>.hml`
- Build wiring: `examples/erlang/Makefile`

**New tutorial page:**
- Markdown page: `tutorial/docs/<section>/<page>.md`
- Reusable include: `tutorial/includes/<name>.md`
- Navigation/config: `tutorial/mkdocs.yml`

## Special Directories

**`detecter/ebin/`:**
- Purpose: Erlang beam output from `detecter/Makefile`.
- Generated: Yes.
- Committed: Present in the working tree.

**`examples/*/ebin/`:**
- Purpose: Compiled demo modules and generated monitor modules for examples.
- Generated: Yes.
- Committed: Present in the working tree.

**`detecter/test/regeneration/ebin/`:**
- Purpose: Generated regeneration test modules and beams.
- Generated: Yes.
- Committed: Present in the working tree.

**`detecter/src/**/*_lexer.erl` and `detecter/src/**/*_parser.erl`:**
- Purpose: Generated Erlang scanner/parser modules consumed by evaluators.
- Generated: Yes, from `detecter/priv/*.xrl` and `detecter/priv/*.yrl`.
- Committed: Yes.

**`detecter/priv/`:**
- Purpose: Source grammar specs, parser specs, fixtures, and static EDoc assets.
- Generated: No for grammar/spec files; fixture logs are data files.
- Committed: Yes.

**`tutorial/`:**
- Purpose: Documentation site source and Python lexer package helpers.
- Generated: No for docs; site output is not present in the scanned tree.
- Committed: Yes.

**`examples/python/venv/`:**
- Purpose: Python virtual environment for the Python example.
- Generated: Yes.
- Committed: Present in the working tree; do not add new source code here.

**`.planning/`:**
- Purpose: GSD planning and codebase intelligence artifacts.
- Generated: Yes.
- Committed: Project-dependent; only mapper documents should be edited by this task.

---

*Structure analysis: 2026-06-24*
