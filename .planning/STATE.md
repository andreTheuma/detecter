---
gsd_state_version: '1.0'
status: in_progress
progress:
  total_phases: 22
  completed_phases: 3
  total_plans: 48
  completed_plans: 8
  percent: 17
---

# Project State

## Project Reference

See: `.planning/PROJECT.md` (updated 2026-06-29)

**Core value:** Generated monitors must only emit verdicts that are sound and irrevocable for the traced system model, especially when traces contain missing events.
**Current focus:** Phase 4: Sound AGM State Regeneration

## Current Position

Phase: 4 of 22 (Sound AGM State Regeneration)
Plan: 2 of 3 in current phase
Status: In progress - ready for co-op execution of 04-02
Last activity: 2026-07-01 - Resumed Plan 04-02 and corrected its state notation to match the thesis: `X0` is the known state before the missing event and `X1` is the inferred state after it.

Progress: [##--------] 17%

## Performance Metrics

**Velocity:**
- Total plans completed: 8
- Average duration: not tracked yet
- Total execution time: not tracked yet

**By Phase:**

| Phase | Plans | Total | Avg/Plan |
|-------|-------|-------|----------|
| 1. Test Harness Baseline | 3/3 | not tracked | not tracked |
| 2. Parser Contract Alignment | 2/2 | not tracked | not tracked |
| 3. Generated Monitor Compile Correctness | 2/2 | not tracked | not tracked |
| 4. Sound AGM State Regeneration | 1/3 | not tracked | not tracked |

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
- Phase 4: Generated `init_transitions/0` must preserve duplicate `{Src, Dst}` rows, so AGM can detect ambiguous event recovery.
- Phase 4: `handle_missing_event/1` should return `{ok, Recovery}` only for singleton state and singleton concrete event recovery.
- Phase 4: Ambiguous, impossible, or symbolic/ranged recovery must return `{withhold, Reason}` rather than rejection.
- Phase 4: Phase 5 remains responsible for complete-trace versus recovered-trace verdict equivalence and irrevocability regression tests.
- Phase 4 plan 04-01: Generated transitions are ordered four-field rows containing source, destination, event descriptor, and condition predicate; no active AGM helper converts from a map.
- Phase 4 plan 04-01: The obsolete `parse_sys_info_event/1` name and commented map-based deduction prototype were removed.
- Phase 4 plan 04-01: The generated `NULL` condition now returns the boolean result of `Event =:= null`, matching its literal descriptor.
- Phase 4 documentation: The thesis now defines recovery using singleton state sets and a singleton concrete event-candidate set; symbolic, ambiguous, and impossible recovery explicitly withhold.
- Phase 4 notation: The thesis uses `X0` for the known source state immediately before the missing event and `X1` for the inferred destination immediately after it. Plan 04-02 generated names must preserve this distinction.
- Phase 4 documentation: Each AGM implementation step now includes an anchored trace snapshot; blue emphasis identifies only the information introduced by that step.

### Pending Todos

- Phase 4 plan 04-02: Replace current transition validation with singleton deduction.
- Phase 4 plan 04-03: Add tests for unique, ambiguous, impossible, and symbolic missing-event cases.
- Re-run and update the thesis AGM terminal listings after plans 04-02 and 04-03 produce verified generated output.

### Blockers/Concerns

- GSD helper runtime previously failed to load `../../../package.json`; planning docs are currently maintained directly.
- `detecter/test/regeneration/automated_event_streamer.erl` is untracked and needs an ownership decision before cleanup.
- The thesis now documents the intended 04-02 `{ok, Recovery}` / `{withhold, Reason}` behavior; current generated recovery code does not yet fully satisfy it.
- Current generated `handle_missing_event/1` calls its post-missing candidate list `S_X0`; Plan 04-02 must rename or replace this misleading variable so it corresponds to thesis state `X1`.

## Deferred Items

| Category | Item | Status | Deferred At |
|----------|------|--------|-------------|
| Parser robustness | Full `.spec` grammar and structured parser errors | Planned for v0.2 | v0.1 setup |
| Runtime hardening | ETS isolation, tracer deletion safety, deprecated stacktrace APIs | Planned for v0.3 | v0.1 setup |
| Cleanup | Dynamic attach/detach decision, AST helper consolidation, fixture layout cleanup | Backlog v0.5 | v0.1 setup |
| Synthesis cleanup | Normalize generator variable names to one internal representation | Backlog Phase 22 | Phase 3 arity-fix discussion |

## Session Continuity

Last session: 2026-07-01
Stopped at: Plan 04-02 resumed in co-op mode; next task is generating the singleton candidate-state helper.
Resume file: `.planning/phases/04-sound-agm-state-regeneration/04-02-PLAN.md`
