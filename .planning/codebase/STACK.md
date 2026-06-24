# Technology Stack

**Analysis Date:** 2026-06-24

## Languages

**Primary:**
- Erlang/OTP - Core runtime verification tool under `detecter/src/**/*.erl`, headers in `detecter/include/*.hrl`, tests in `detecter/test/**/*.erl`, examples in `examples/erlang/src/**/*.erl`

**Secondary:**
- Elixir - Example actor-style calculator service under `examples/elixir/lib/**/*.ex`, compiled by `examples/elixir/Makefile`
- Python - Tutorial support packages and socket demo under `tutorial/*.py`, `tutorial/*_lexer/*.py`, and `examples/python/src/demo/*.py`
- Markdown/YAML - MkDocs tutorial content in `tutorial/docs/**/*.md` and `tutorial/mkdocs.yml`
- Leex/Yecc grammars - Lexer/parser source grammars in `detecter/priv/*.xrl` and `detecter/priv/*.yrl`

## Runtime

**Environment:**
- Erlang/OTP 22.1.5 in CI via `.github/workflows/build.yml` container image `erlang:22.1.5`
- Local Erlang/OTP runtime with `erl`, `erlc`, `dialyzer`, `edoc`, `leex`, and `yecc` available on PATH for `detecter/Makefile`
- Python 3 for tutorial tooling and examples; `examples/python/venv/lib/python3.9/...` indicates a committed Python 3.9 virtualenv, but new work should use `tutorial/requirements.txt` rather than the vendored environment

**Package Manager:**
- Erlang: no Rebar3/Mix package manager detected for the main `detecter/` project; builds use Make plus `erlc`
- Python: `pip` with `tutorial/requirements.txt`
- Lockfile: missing for Erlang, Elixir, and Python tutorial dependencies

## Frameworks

**Core:**
- Erlang/OTP standard libraries - Process messaging, `gen_server`, ETS, file I/O, tracing, and parser tooling used across `detecter/src/**/*.erl`
- Custom `gen_file_poller` behavior - File polling framework in `detecter/src/behavior/gen_file_poller.erl` and implementation adapter in `detecter/src/behavior/gen_file_poller_impl.erl`
- Erlang VM tracing - Runtime tracing wrapper in `detecter/src/tracing/evm_tracer.erl` using `erlang:trace/3`, `erlang:trace_pattern/3`, and process trace flags
- Log-based tracing - File-backed trace ingestion in `detecter/src/tracing/log_tracer.erl` and `detecter/src/tracing/log_poller.erl`

**Testing:**
- EUnit - Test modules include `eunit/include/eunit.hrl` in `detecter/test/tracing/log_tracer_test.erl`, `detecter/test/monitoring/tracer_test.erl`, and `detecter/test/regeneration/sys_info_parser_test.erl`
- Make-driven test runner - `detecter/Makefile` runs `erl -noshell -pa ebin -eval 'case eunit:test(log_tracer_test, [verbose]) ...'`

**Build/Dev:**
- GNU Make - Main build/test/analyze commands in `detecter/Makefile`; example builds in `examples/erlang/Makefile`, `examples/elixir/Makefile`, and `examples/python/Makefile`
- `erlc` - Compiles Erlang modules from `detecter/src/**/*.erl` and hand-written `detecter/test/**/*.erl` into `detecter/ebin`, excluding generated `detecter/test/**/ebin/**` fixtures during test compilation.
- Dialyzer - Static analysis command in `detecter/Makefile` target `analyze`
- EDoc - API documentation generation in `detecter/src/build.erl` through `edoc:application/3`
- Leex/Yecc - Lexer and parser generation in `detecter/src/build.erl`, sourced from `detecter/priv/*.xrl` and `detecter/priv/*.yrl`
- MkDocs Material - Tutorial site configured in `tutorial/mkdocs.yml` with packages from `tutorial/requirements.txt`

## Key Dependencies

**Critical:**
- Erlang `erts`, `kernel`, `stdlib` - Required for process model, message passing, `gen_server`, `ets`, `timer`, `file`, `io`, and runtime tracing used across `detecter/src/**/*.erl`
- Erlang `syntax_tools` - Required by `detecter/src/monitoring/hml_eval.erl` and `detecter/src/synthesis/gen_eval.erl` via `syntax_tools/include/merl.hrl`
- Erlang `parsetools` - Required for Leex/Yecc lexer/parser generation from `detecter/priv/*.xrl` and `detecter/priv/*.yrl`
- Erlang `eunit` - Required for test compilation and execution in `detecter/test/**/*.erl`

**Infrastructure:**
- `mkdocs` - Builds the tutorial site from `tutorial/mkdocs.yml`
- `mkdocs-material` - Provides the Material theme in `tutorial/mkdocs.yml`
- `mkdocs-macros-plugin` - Enables the `macros` plugin configured in `tutorial/mkdocs.yml`
- `Pygments>=2.3.1` - Runtime dependency for custom SHML and MaxHML lexers in `tutorial/setup.py` and `tutorial/setup-maxhml.py`
- Python standard library `socket`, `re`, `logging`, `threading`, `argparse` - Used by calculator TCP demos in `examples/python/src/demo/calc_server.py` and `examples/python/src/demo/calc_server_bug.py`

## Configuration

**Environment:**
- No `.env` files detected at repo root or one level down
- No application config file (`sys.config`, `.app.src`, Rebar config, Mix config) detected for the main Erlang project
- Build paths are Make variables in `detecter/Makefile`: `BIN=ebin`, `INCLUDE=include`, `SRC=src`, `TEST=test`
- CI duplicates build path configuration in `.github/workflows/build.yml` environment variables: `BIN=detecter/ebin`, `INCLUDE=detecter/include`, `SRC=detecter/src`, `TEST=detecter/test`
- Logging is compile-time macro configured in `detecter/include/log.hrl`; logging is enabled by `-define(log, log)` and default log level is `1`
- Test/profile code paths are compile-time macros in `detecter/include/dev.hrl`; `compile-test` passes `-DTEST`

**Build:**
- Main build config: `detecter/Makefile`
- CI build config: `.github/workflows/build.yml`
- Tutorial docs config: `tutorial/mkdocs.yml`
- Tutorial Python dependencies: `tutorial/requirements.txt`
- Python lexer package metadata: `tutorial/setup.py` and `tutorial/setup-maxhml.py`
- Example build configs: `examples/erlang/Makefile`, `examples/elixir/Makefile`, `examples/python/Makefile`
- Generated artifacts are committed under `detecter/ebin/*.beam` and `detecter/doc/*.html`; regenerate from `detecter/src/**/*.erl` with `make compile` and `build:edoc/0` rather than editing generated files

## Platform Requirements

**Development:**
- Erlang/OTP matching CI as closely as possible, preferably OTP 22.1.5 because `.github/workflows/build.yml` pins `erlang:22.1.5`
- GNU Make and shell utilities used by Make recipes: `find`, `rm`, `mkdir`, and `sort`
- Dialyzer for `make analyze` in `detecter/Makefile`
- Python and `pip install -r tutorial/requirements.txt` for tutorial site work
- Elixir compiler `elixirc` only for `examples/elixir/Makefile`

**Production:**
- Deployment target is not packaged as an OTP release; compile Erlang modules to BEAM files in `detecter/ebin` and run them on an Erlang VM
- Public tutorial is intended for GitHub Pages based on README link `https://duncanatt.github.io/detecter` and `tutorial/mkdocs.yml` repository metadata, but no deployment workflow is present

---

*Stack analysis: 2026-06-24*
