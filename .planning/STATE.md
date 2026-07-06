---
gsd_state_version: '1.0'
status: awaiting_manual_review
progress:
  total_phases: 23
  completed_phases: 5
  total_plans: 51
  completed_plans: 16
  percent: 31
---

# Project State

## Project Reference

See: `.planning/PROJECT.md` (updated 2026-07-01)

**Core value:** Generated monitors must only emit verdicts that are sound and irrevocable for the traced system model, especially when traces contain missing events.
**Current focus:** Phase 5: Irrevocability and Verdict Semantics

## Current Position

Phase: 5 of 23 (Irrevocability and Verdict Semantics)
Plan: 3 of 3
Status: All three Phase 5 plans executed; awaiting manual review of unstaged, uncommitted changes
Last activity: 2026-07-02 - Passed 72 code checks, production checkpoint exclusion, the nine-source thesis audit, and a 134-page thesis build.

Progress: [###-------] 31%

## Performance Metrics

**Velocity:**
- Total plans completed: 16
- Average duration: not tracked yet
- Total execution time: not tracked yet

**By Phase:**

| Phase | Plans | Total | Avg/Plan |
|-------|-------|-------|----------|
| 1. Test Harness Baseline | 3/3 | not tracked | not tracked |
| 2. Parser Contract Alignment | 2/2 | not tracked | not tracked |
| 3. Generated Monitor Compile Correctness | 2/2 | not tracked | not tracked |
| 4. Sound AGM State Regeneration | 3/3 | not tracked | not tracked |
| 4.1 AGM Engine Extraction and Thesis Reconciliation | 3/3 | not tracked | not tracked |
| 5. Irrevocability and Verdict Semantics | 3/3 | 32 min | 11 min |

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
- Phase 4: Generated `init_transitions/0` must preserve duplicate `{Src, Dst}` rows so AGM can compare every candidate event's monitoring consequence.
- Phase 4: `handle_missing_event/1` returns `{ok, Recovery}` after singleton state recovery while preserving every event descriptor compatible with the inferred transition.
- Phase 4: Generated monitor branches proceed only when every candidate event produces the same continuation or terminal verdict, including identical bound values.
- Phase 4: Event multiplicity and symbolic descriptors do not automatically require withholding; conflicting consequences or an inability to prove symbolic uniformity do.
- Phase 4: Exact event recovery is optional metadata and does not gate an otherwise deterministic monitoring consequence.
- Phase 4: Phase 5 remains responsible for complete-trace versus recovered-trace verdict equivalence and irrevocability regression tests.
- Phase 4 plan 04-01: Generated transitions are ordered four-field rows containing source, destination, event descriptor, and condition predicate; no active AGM helper converts from a map.
- Phase 4 plan 04-01: The obsolete `parse_sys_info_event/1` name and commented map-based deduction prototype were removed.
- Phase 4 plan 04-01: The generated `NULL` condition now returns the boolean result of `Event =:= null`, matching its literal descriptor.
- Phase 4 plan 04-02: Generated `candidate_event_specs/2` preserves distinct descriptors for `X0 -> X1`, and `handle_missing_event/1` stores scalar `X1`.
- Phase 4 plan 04-02: Consequences are compared by `{verdict, Verdict}` or `{continue, Function, BoundValues}`; unsupported symbolic proofs return `unproven_consequence`.
- Phase 4 documentation: The thesis defines recovery using singleton state sets and a singleton monitoring-consequence set; exact event recovery is an optional refinement.
- Phase 4 notation: The thesis uses `X0` for the known source state immediately before the missing event and `X1` for the inferred destination immediately after it. Plan 04-02 generated names must preserve this distinction.
- Phase 4 documentation: Each AGM implementation step now includes an anchored trace snapshot; blue emphasis identifies only the information introduced by that step.
- Phase 4 plan 04-03: `generated_agm_recovery_test` compiles generated modules with test-only `export_all` and verifies both generated AGM helpers and actual generated monitor-state behavior.
- Phase 4 plan 04-03: A single literal descriptor yields `{known, Event}` metadata; multiple literals and symbolic descriptors retain `unknown` metadata without blocking a uniform consequence.
- Phase 4 plan 04-03: Concrete candidates with one signature and a symbolic domain outside the equality boundary continue; conflicting concrete/symbolic signatures and unsupported symbolic guard proofs withhold.
- Phase 4 plan 04-03: The default test target now runs 24 tracing, 5 parser, 3 generated compile-smoke, and 8 generated AGM recovery tests.
- Phase 4.1 architecture: Complete Phase 4 characterization tests before extracting a pure AGM runtime engine and a separate AGM code-generation module.
- Phase 4.1 academic gate: Review every supplied thesis chapter after the refactor and record `amended` or `verified unchanged` with code and test evidence.
- Phase 4.1 plan 04.1-01: `agm_engine` is pure and receives transition data and reduction functions explicitly; 13 direct tests exercise its contract.
- Phase 4.1 plan 04.1-02: `maxhml_agm_codegen` owns transition/reduction AST and generated effect boundaries; `maxhml_eval` delegates through four callbacks.
- Phase 4.1 plan 04.1-02: The eight Phase 4 semantic cases remain; one architecture check proves generated source calls `agm_engine` and omits the old collector.
- Phase 4.1 plan 04.1-03: Four thesis chapters were amended and four verified unchanged; the complete 129-page thesis build passes.
- Phase 5 plan 05-01: Recovery preserves the exact lookahead as an explicit pending envelope and commits both recovery-state keys in one ETS insert only after consequence agreement.
- Phase 5 plan 05-01: Unchanged recursive/max consequence identities route through generated replay bridges to actual receive-state pending dispatchers with complete bound arguments.
- Phase 5 plan 05-02: Complete and recovered executions derive from one logical event sequence and compare exact terminal verdict lists or reference-tagged continuation checkpoints.
- Phase 5 plan 05-02: Pending replay retention is proven by advancing on a newer receive event and observing the older send envelope after the state transition.
- Phase 5 plan 05-02: All four withholding reasons use queued-before-start inputs, normal worker termination, unchanged two-entry recovery state, and an exact empty verdict list.
- Phase 5 plan 05-03: Eight named terminal rows cross complete/recovered paths, yes/no verdicts, and queued/post-verdict extensions; every row emits one exact verdict and exits normally.
- Phase 5 plan 05-03: The abstract and all eight active chapter sources have evidence-backed dispositions; four were amended and five verified unchanged.
- Phase 5 plan 05-03: v0.1 stops after Phase 5 pending manual review; Phases 6 through 22 remain future milestones.

### Roadmap Evolution

- Phase 4.1 inserted after Phase 4: extract the AGM engine and reconcile the thesis chapter by chapter before Phase 5 verdict work.

### Pending Todos

- Manually review the complete unstaged Phase 5 code, test, thesis, and planning changes before committing.
- Do not start Phase 6 or later until the user explicitly opens the next milestone.

### Blockers/Concerns

- GSD helper runtime previously failed to load `../../../package.json`; planning docs are currently maintained directly.
- `detecter/test/regeneration/automated_event_streamer.erl` is untracked and needs an ownership decision before cleanup.
- The full thesis build succeeds; existing layout, font, and acronym-reference warnings remain.
- Phase 5 is technically verified but intentionally uncommitted pending manual review.

## Deferred Items

| Category | Item | Status | Deferred At |
|----------|------|--------|-------------|
| Parser robustness | Full `.spec` grammar and structured parser errors | Planned for v0.2 | v0.1 setup |
| Runtime hardening | ETS isolation, tracer deletion safety, deprecated stacktrace APIs | Planned for v0.3 | v0.1 setup |
| Cleanup | Dynamic attach/detach decision, AST helper consolidation, fixture layout cleanup | Backlog v0.5 | v0.1 setup |
| Synthesis cleanup | Normalize generator variable names to one internal representation | Backlog Phase 22 | Phase 3 arity-fix discussion |

## Session Continuity

Last session: 2026-07-02
Stopped at: Completed Plan 05-03 and final v0.1 evidence gate; awaiting manual review with all changes unstaged.
Resume file: `.planning/phases/05-irrevocability-and-verdict-semantics/05-03-SUMMARY.md`
