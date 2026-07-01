---
phase: 04-sound-agm-state-regeneration
plan: 04-02
subsystem: synthesis
tags: [erlang, maxhml, agm, missing-events, soundness]
requires:
  - phase: 04-01
    provides: Multiplicity-preserving transition rows with event descriptors
provides:
  - Singleton post-missing state inference
  - Monitoring-consequence aggregation across compatible event descriptors
  - Conservative symbolic-domain proof and explicit withholding
affects: [04-03, 04.1-agm-engine-extraction, phase-5-verdict-semantics]
tech-stack:
  added: []
  patterns:
    - Complete consequence signatures include verdicts or continuation functions with bound values
    - Exact event recovery is optional metadata
key-files:
  created: []
  modified:
    - detecter/src/synthesis/maxhml_eval.erl
key-decisions:
  - "Require singleton state inference and singleton monitoring consequence, not singleton exact-event recovery."
  - "Withhold on conflicting consequences or unsupported symbolic proofs without converting uncertainty into rejection."
patterns-established:
  - "Generated reducers evaluate literal descriptors through the current monitor pattern and guard."
  - "Supported symbolic equality/inequality domains are analyzed without enumeration."
requirements-completed: [AGM-01, AGM-03, AGM-04, AGM-05]
duration: not tracked
completed: 2026-07-01
status: complete
---

# Phase 4 Plan 04-02: Singleton AGM Deduction and Withholding Summary

**Generated monitors now infer one post-missing state and proceed only when every compatible missing event has one complete monitoring consequence**

## Performance

- **Duration:** Not tracked
- **Completed:** 2026-07-01T13:30:22Z
- **Tasks:** 6
- **Files modified:** 1

## Accomplishments

- Replaced list-as-state handling with explicit `impossible_recovery` and `ambiguous_state` outcomes.
- Preserved all event descriptors for the inferred `X0 -> X1` transition and made exact-event recovery optional.
- Added literal and supported symbolic reduction logic that compares verdict or continuation signatures, including bound values.
- Removed recovery paths that mapped uncertainty to rejection.

## Task Commits

1. **Plan 04-02 implementation** - `abba504` (`[feature]: plan 04-02`)

## Files Created/Modified

- `detecter/src/synthesis/maxhml_eval.erl` - Generates singleton state recovery, event-descriptor collection, consequence reduction, symbolic membership, and withholding logic.

## Decisions Made

- Monitoring may continue with several possible events when all possibilities induce the same monitor continuation or terminal verdict.
- A symbolic descriptor is not itself a failure; it proceeds only where the generator can prove consequence uniformity over the represented domain.
- Continuation arguments are part of consequence identity because different bound values can change future monitor behavior.

## Deviations from Plan

The original plan required singleton concrete-event recovery. It was deliberately corrected during execution to match the thesis's stronger monitoring-consequence argument: exact event identity is optional when all model-compatible candidates are monitor-equivalent.

## Issues Encountered

- The generated AST construction expanded substantially inside `maxhml_eval.erl`; Phase 4.1 is inserted to extract a pure AGM runtime engine and dedicated AGM code-generation boundary after characterization tests.

## User Setup Required

None.

## Next Phase Readiness

- Plan 04-03 provides permanent generated-module characterization tests before Phase 4.1 refactoring.

## Self-Check: PASSED

- `make compile-test` passed.
- Focused generated modules compiled.
- Plan 04-03 subsequently exercised every recovery and consequence outcome at runtime.

---
*Phase: 04-sound-agm-state-regeneration*
*Completed: 2026-07-01*
