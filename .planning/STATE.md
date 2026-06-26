---
gsd_state_version: '1.0'
status: in_progress
progress:
  total_phases: 22
  completed_phases: 3
  total_plans: 48
  completed_plans: 7
  percent: 15
---

# Project State

## Project Reference

See: `.planning/PROJECT.md` (updated 2026-06-26)

**Core value:** Generated monitors must only emit verdicts that are sound and irrevocable for the traced system model, especially when traces contain missing events.
**Current focus:** Phase 4: Sound AGM State Regeneration

## Current Position

Phase: 4 of 22 (Sound AGM State Regeneration)
Plan: 0 of 3 in current phase
Status: Ready for Phase 4 planning
Last activity: 2026-06-26 - Phase 3 completed. `generated_monitor_smoke_test` now compiles generated Erlang for `prop_no_leak`, `prop_no_failure`, and `prop_correct_start`, and asserts generated sources do not contain `update_current_state()`.

Progress: [#---------] 15%

## Performance Metrics

**Velocity:**
- Total plans completed: 7
- Average duration: not tracked yet
- Total execution time: not tracked yet

**By Phase:**

| Phase | Plans | Total | Avg/Plan |
|-------|-------|-------|----------|
| 1. Test Harness Baseline | 3/3 | not tracked | not tracked |
| 2. Parser Contract Alignment | 2/2 | not tracked | not tracked |
| 3. Generated Monitor Compile Correctness | 2/2 | not tracked | not tracked |
| 4. Sound AGM State Regeneration | 0/3 | not tracked | not tracked |

## Accumulated Context

### Decisions

Decisions are logged in `.planning/PROJECT.md` Key Decisions table.

- Phase 1: Generated regeneration artifacts under `detecter/test/**/ebin/**` are treated as generated fixtures, not hand-written test source.
- Phase 1: `tracer_test` remains excluded from the default Makefile test target until timing-sensitive behavior is stabilized or documented.
- Phase 1/3: The initial generated-monitor compile smoke test started with `prop_no_leak`; Phase 3 broadened coverage to representative properties that exercise the reproduced generator arity bug and the positive init branch.
- Phase 1: The `tracer_test` exclusion is now documented beside the Makefile `test` target with a manual command.
- Phase 2: Parser expectations now match the symbolic condition tuples consumed by synthesis.
- Phase 2: Parser coverage is split into focused tests so failures identify which part of the accepted `.spec` contract changed.
- Phase 3: Keep this phase limited to generated Erlang compile correctness. Sound AGM singleton/withhold behavior remains Phase 4, and verdict equivalence/irrevocability remains Phase 5.
- Phase 3: Thesis alignment requires generated calls to match the documented `update_current_state(Event)` contract; no generated path should call `update_current_state/0`.
- Phase 3: Reproduction command generated `/tmp/detecter-phase3.VzF8Oi/prop_no_failure_flu.erl`; line 13 called `update_current_state()` and `erlc` reported `function update_current_state/0 undefined`.
- Phase 3: Defensive filters may handle both atom and string variable names locally; generator-wide normalization is now tracked separately as Phase 22.
- Phase 3: The implemented fix uses `generate_state_update_calls/1` and `generate_state_update_args/1` to derive state-update arguments from the current action pattern, excluding non-event variables through `?STATE_UPDATE_EXCLUDED_VARS`.
- Phase 3: The reproduced generated module now emits `update_current_state(OwnTok)`, and focused generated compilation exits successfully with warnings only.
- Phase 3: `make compile-test` and `make test` both pass after the state-update arity fix.
- Phase 3: `generated_monitor_smoke_test` now uses separate temporary output directories for `prop_no_leak`, `prop_no_failure`, and `prop_correct_start` so generated module names cannot collide across properties.
- Phase 3: Regression coverage asserts generated sources do not contain the old invalid `update_current_state()` call before compiling them.

### Pending Todos

- Phase 4 plan 04-01: Preserve transition multiplicity in generated system-information data.
- Phase 4 plan 04-02: Replace current transition validation with singleton deduction.
- Phase 4 plan 04-03: Add tests for unique, ambiguous, impossible, and symbolic missing-event cases.

### Blockers/Concerns

- GSD helper runtime previously failed to load `../../../package.json`; planning docs are currently maintained directly.
- `detecter/test/regeneration/automated_event_streamer.erl` is untracked and needs an ownership decision before cleanup.

## Deferred Items

| Category | Item | Status | Deferred At |
|----------|------|--------|-------------|
| Parser robustness | Full `.spec` grammar and structured parser errors | Planned for v0.2 | v0.1 setup |
| Runtime hardening | ETS isolation, tracer deletion safety, deprecated stacktrace APIs | Planned for v0.3 | v0.1 setup |
| Cleanup | Dynamic attach/detach decision, AST helper consolidation, fixture layout cleanup | Backlog v0.5 | v0.1 setup |
| Synthesis cleanup | Normalize generator variable names to one internal representation | Backlog Phase 22 | Phase 3 arity-fix discussion |

## Session Continuity

Last session: 2026-06-26
Stopped at: Session resumed; awaiting selection for Phase 4 sound AGM state-regeneration discussion or planning.
Resume file: None
