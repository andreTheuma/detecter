# External Integrations

**Analysis Date:** 2026-06-24

## APIs & External Services

**Runtime monitoring and tracing:**
- Erlang VM tracing API - Core runtime integration for observing process lifecycle and messaging
  - SDK/Client: Erlang built-ins `erlang:trace/3`, `erlang:trace_pattern/3`, `erlang:trace_info/2` in `detecter/src/tracing/evm_tracer.erl`
  - Auth: Not applicable
- Local log files - Offline/forward-only trace source for log-based monitoring
  - SDK/Client: Erlang `file` module through `detecter/src/behavior/gen_file_poller.erl`, `detecter/src/tracing/log_poller.erl`, and `detecter/src/tracing/log_tracer.erl`
  - Auth: Filesystem permissions only
- Local TCP socket demo - Python calculator example binds to `0.0.0.0:8080` and accepts simple text commands
  - SDK/Client: Python standard library `socket` in `examples/python/src/demo/calc_server.py` and `examples/python/src/demo/calc_server_bug.py`
  - Auth: None

**Documentation/public metadata:**
- GitHub repository links - Tutorial and README point to GitHub repository metadata
  - SDK/Client: Markdown badges in `README.md` and `repo_url`/`repo_name` in `tutorial/mkdocs.yml`
  - Auth: Not applicable
- GitHub Pages tutorial - README links to `https://duncanatt.github.io/detecter`
  - SDK/Client: MkDocs site in `tutorial/mkdocs.yml`
  - Auth: Not detected
- Zenodo DOI badge - README includes a Zenodo release DOI badge
  - SDK/Client: Markdown badge in `README.md`
  - Auth: Not applicable
- External CDN JavaScript for tutorial math rendering
  - SDK/Client: `https://polyfill.io/v3/polyfill.min.js?features=es6` and `https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js` in `tutorial/mkdocs.yml`
  - Auth: None
- Social/profile links - Tutorial config links to GitHub, Twitter, profile URL, and mailto contact
  - SDK/Client: `extra.social` entries in `tutorial/mkdocs.yml`
  - Auth: None

## Data Storage

**Databases:**
- Not detected
  - Connection: Not applicable
  - Client: No database client, ORM, or persistence service detected in `detecter/src/**/*.erl`, `examples/**/*.erl`, `examples/**/*.ex`, or `examples/python/src/**/*.py`

**File Storage:**
- Local filesystem only
  - Trace input logs: `detecter/priv/trace_test*.log`, `examples/python/trace.log`, and caller-provided file paths consumed by `detecter/src/tracing/log_tracer.erl`
  - Generated BEAM files: `detecter/ebin/*.beam`
  - Generated documentation: `detecter/doc/*.html`
  - Generated analyzer/source artifacts: file writes in `detecter/src/monitoring/hml_eval.erl`, `detecter/src/synthesis/gen_eval.erl`, and `detecter/src/synthesis/event_writer.erl`

**Caching:**
- In-memory ETS allocation table in `detecter/src/tracing/log_tracer.erl` for tracee-to-tracer allocation state
- No external cache service detected

## Authentication & Identity

**Auth Provider:**
- None detected
  - Implementation: Runtime interactions are Erlang process messages, VM tracing calls, local file reads/writes, and unauthenticated example TCP socket traffic

## Monitoring & Observability

**Error Tracking:**
- None detected

**Logs:**
- Custom Erlang logging macros in `detecter/include/log.hrl` call `log:write/5`
- Log output can be redirected to a local file through `detecter/src/log.erl` `log_to_file/1`
- Python socket examples use Python `logging` and can write traces to caller-provided files in `examples/python/src/demo/calc_server.py`
- Runtime verification trace events are represented as Erlang terms in local log files parsed by `detecter/src/tracing/log_eval.erl` and posted by `detecter/src/tracing/log_poller.erl`

## CI/CD & Deployment

**Hosting:**
- CI runs on GitHub Actions using Ubuntu with container image `erlang:22.1.5` in `.github/workflows/build.yml`
- Tutorial public URL is GitHub Pages based on `README.md` link `https://duncanatt.github.io/detecter`, but no GitHub Pages deployment workflow is present in `.github/workflows/`

**CI Pipeline:**
- GitHub Actions workflow `.github/workflows/build.yml`
- Trigger: pushes to `master`
- Steps: checkout with `actions/checkout@v1`, compile Erlang source/tests using `erlc`, list `detecter/ebin`, run `eunit:test(log_tracer_test, [verbose])`
- Disabled CI test: `tracer_test` is commented out in `.github/workflows/build.yml` due to timing issues

## Environment Configuration

**Required env vars:**
- None detected for runtime application behavior
- CI-only path variables in `.github/workflows/build.yml`: `BIN`, `INCLUDE`, `SRC`, `TEST`

**Secrets location:**
- No `.env` files detected
- No repository secret files detected during stack/integration scan
- GitHub Actions workflow does not reference `${{ secrets.* }}` in `.github/workflows/build.yml`

## Webhooks & Callbacks

**Incoming:**
- None for production application
- Local demo TCP server accepts client socket commands `/add`, `/mul`, and `/stp` on port `8080` in `examples/python/src/demo/calc_server.py`
- Erlang and Elixir examples receive process messages directly in `examples/erlang/src/demo/*.erl` and `examples/elixir/lib/demo/*.ex`

**Outgoing:**
- No outgoing HTTP/API callbacks detected
- Runtime output consists of Erlang process messages, local trace events, file writes, and example socket replies in `examples/python/src/demo/calc_server.py`

---

*Integration audit: 2026-06-24*
