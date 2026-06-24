# detectEr Thesis Stabilization

## What This Is

detectEr is an Erlang runtime-verification research codebase used to synthesize monitors from HML/maxHML-style specifications and evaluate traces from live or replayed systems. The current work stabilizes the implementation behind the thesis claims: modular monitor synthesis, automaton-guided missing-event regeneration, sound verdicts, and irrevocability.

## Core Value

Generated monitors must only emit verdicts that are sound and irrevocable for the traced system model, especially when traces contain missing events.

## Requirements

### Validated

- [x] Existing log tracing tests run through `make test`.
- [x] Phase 1 plan 01-01: `make test` no longer compiles generated `detecter/test/**/ebin/**` Erlang artifacts as hand-written test source.
- [x] Phase 1 plan 01-02: `make test` now runs `sys_info_parser_test` and `generated_monitor_smoke_test`.
- [x] Phase 1 plan 01-03: timing-sensitive `tracer_test` exclusion is documented in the Makefile and testing notes.
- [x] Phase 2 plan 02-01: parser expectations match the symbolic system-information contract consumed by synthesis.
- [x] Phase 2 plan 02-02: parser contract coverage is split into focused START/NULL, integer, symbolic range, and set-minus tests.

### Active

- [ ] Broaden generated-monitor compile coverage beyond the currently smoke-tested recursive regeneration property.
- [ ] Fix generated monitor compile errors caused by invalid state-update calls.
- [ ] Rework AGM missing-event handling to deduce only singleton states/events and withhold otherwise.
- [ ] Verify complete-trace and recovered-trace verdict equivalence for deterministic recovery.

### Out of Scope

- General uncertainty handling beyond missing events - the thesis scope is data-restricted traces with missing events under a well-defined operational model.
- Probabilistic or quantitative verdicts - this work preserves deterministic, sound verdict production.
- Full parser language completion before thesis-critical fixes - richer `.spec` grammar work is tracked in v0.2.
- Runtime API cleanup unrelated to thesis soundness - attach/detach and broader cleanup are tracked in later milestones.

## Current Milestone: v0.1 Thesis Claim Stabilization

**Goal:** Make the implementation support the documentation claims about modular synthesis, automaton-guided regeneration, soundness, and irrevocability.

**Target features:**
- A reliable test baseline for synthesis and regeneration work.
- Parser behavior documented and tested against the generator's expected contract.
- Generated monitors compile cleanly for representative properties.
- AGM missing-event handling with deterministic singleton deduction and explicit withholding.
- Regression tests for soundness and irrevocability semantics.

## Context

- The codebase is Make-driven Erlang, with source under `detecter/src`, EUnit tests under `detecter/test`, and generated regeneration fixtures under `detecter/test/regeneration/ebin`.
- The thesis-critical implementation lives primarily in `detecter/src/synthesis/maxhml_eval.erl` and `detecter/src/regeneration/sys_info_parser.erl`.
- Codebase mapping docs live under `.planning/codebase/` and should be treated as audit context rather than source-of-truth API documentation.
- The first verified repair excludes generated regeneration artifacts from test-source compilation. `make test` now reaches EUnit and runs `log_tracer_test`, `sys_info_parser_test`, and `generated_monitor_smoke_test`.
- `tracer_test` remains manual because it relies on timing sleeps to exercise concurrent tracer routing; this boundary is now documented next to the Makefile test target.

## Constraints

- **Research scope:** Missing-event recovery only - avoid expanding the implementation into unrelated uncertainty models.
- **Soundness:** Missing-event inference must withhold when state/event reconstruction is not deterministic.
- **Irrevocability:** Emitted verdicts must not depend on guesses that could be retracted by later event recovery.
- **Build system:** Keep Makefile-based workflows working while the project has no `rebar.config`.
- **Generated code:** Treat checked-in generated parser/lexer modules carefully; edit grammar sources when changing parser generators.

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Track thesis fixes as v0.1 before broader cleanup | The thesis claims depend on AGM and generated-monitor correctness more than general runtime hardening. | Pending |
| Exclude `test/**/ebin/**` from test-source compilation | Generated regeneration fixtures can have source filenames that do not match module names and should not be compiled as hand-written tests. | Good |
| Keep the initial generated-monitor smoke test focused on `prop_no_leak` | It exercises recursive regeneration code that currently compiles, while the broader failing property matrix belongs to the generator correctness phase. | Pending |
| Keep `tracer_test` manual for now | The suite is timing-dependent and should be rewritten around coarser state assertions or deterministic synchronization before entering the default target. | Pending |
| Keep ambiguous missing-event traces as `withhold` cases | Sound verdicts require deterministic inference, not approximation. | Pending |

## Evolution

This document evolves at phase transitions and milestone boundaries.

**After each phase transition:**
1. Requirements invalidated? Move to Out of Scope with reason.
2. Requirements validated? Move to Validated with phase reference.
3. New requirements emerged? Add to Active.
4. Decisions to log? Add to Key Decisions.
5. "What This Is" still accurate? Update if drifted.

**After each milestone:**
1. Full review of all sections.
2. Core Value check - still the right priority?
3. Audit Out of Scope - reasons still valid?
4. Update Context with current state.

---
*Last updated: 2026-06-24 after Phase 2 completion.*
