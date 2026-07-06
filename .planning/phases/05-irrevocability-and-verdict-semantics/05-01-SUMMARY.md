---
phase: 05-irrevocability-and-verdict-semantics
plan: "01"
subsystem: agm-generated-runtime
tags: [erlang, maxhml, selective-receive, ets, eunit]

requires:
  - phase: 04.1-agm-engine-extraction-and-thesis-reconciliation
    provides: Pure AGM inference/consequence engine and separated AGM code generator
provides:
  - Exact lookahead-envelope preservation and direct ordered replay
  - Post-agreement atomic recovery-state commit
  - Pending-envelope dispatch across receive, recursive, and max-node transitions
  - Test-only generated checkpoints and gated-worker synchronization
affects: [05-02, 05-03, verdict-equivalence, irrevocability]

tech-stack:
  added: []
  patterns:
    - Explicit pending-envelope propagation through generated replay bridges
    - One list-valued ETS insert after consequence agreement
    - Conditional test-only checkpoint clauses with a production no-op branch

key-files:
  created: []
  modified:
    - detecter/src/regeneration/agm_engine.erl
    - detecter/src/synthesis/maxhml_agm_codegen.erl
    - detecter/src/synthesis/maxhml_eval.erl
    - detecter/test/regeneration/agm_engine_test.erl
    - detecter/test/regeneration/generated_agm_recovery_test.erl

key-decisions:
  - "Keep consequence identity unchanged while routing execution through deterministic replay_<function> bridges."
  - "Represent an irrelevant consumed lookahead as an explicit pending argument and reconsider it before newer mailbox envelopes after every continuing transition."
  - "Generate agm_checkpoint clauses only when maxhml_agm_codegen is compiled with TEST; production generation takes an explicit no-op branch."

patterns-established:
  - "Replay bridge: recursive/max identities forward the original envelope and complete arguments until an actual pending receive dispatcher is reached."
  - "Atomic recovery boundary: handle_missing_event/1 is read-only; commit_recovery_state/1 runs only after consequence agreement."

requirements-completed: [VERD-01, VERD-02]

duration: 12 min
completed: 2026-07-02
status: complete
review_state: uncommitted_pending_manual_review
---

# Phase 5 Plan 1: Atomic Recovery and Ordered Replay Summary

**Generated monitors now preserve the exact consumed lookahead, commit recovery state atomically after consequence agreement, and replay through selective-receive-safe recursive/max bridges.**

## Performance

- **Duration:** 12 min
- **Started:** 2026-07-02T10:31:13Z
- **Completed:** 2026-07-02T10:42:52Z
- **Tasks:** 2
- **Files modified:** 5

## Accomplishments

- Made the pure AGM consequence callback contract unary without adding effects or callback invocation to `agm_engine`.
- Added generated `commit_recovery_state/1`, using one list-valued ETS insert only after complete consequence agreement.
- Generated ordinary and pending dispatch paths from shared branch descriptors so unmatched replay remains ordered across later transitions.
- Routed unchanged recursive/max consequence identities through deterministic replay bridges with complete bound arguments.
- Replaced scheduler polling with start-gated workers, process monitors, and reference-tagged generated checkpoints.
- Proved production-generated source contains no checkpoint control tag.

## Review State

Implemented and verified, but intentionally left unstaged and uncommitted for manual Phase 5 review.

1. **Task 1:** Defined and implemented the envelope-aware consequence contract.
2. **Task 2:** Specified and implemented ordered generated recovery replay.

## Files Created/Modified

- `detecter/src/regeneration/agm_engine.erl` - Declares unary envelope callbacks while retaining pure consequence aggregation.
- `detecter/src/synthesis/maxhml_agm_codegen.erl` - Generates read-only recovery, atomic commit, replay bridges, pending dispatchers, and conditional checkpoints.
- `detecter/src/synthesis/maxhml_eval.erl` - Supplies shared ordinary/pending branch descriptors and replay metadata during formula traversal.
- `detecter/test/regeneration/agm_engine_test.erl` - Verifies callback arity, identity deduplication, and non-invocation.
- `detecter/test/regeneration/generated_agm_recovery_test.erl` - Verifies exact envelope retention, no speculative mutation, bridge routing, pending priority, and production-safe checkpoints.

## Verification

- `make -C detecter compile-test` - passed.
- Focused `agm_engine_test` - 15/15 passed.
- Focused `generated_agm_recovery_test` - 11/11 passed.
- Production-clean representative generation - passed; no `agm_checkpoint` tag emitted.
- `make -C detecter test` - passed.
- Claim-critical polling scan - passed; no `timer:sleep` or `process_info`.
- Pure-engine boundary scan - passed with a token-boundary ETS expression.

## Decisions Made

- Continuing callbacks always enter a generated replay bridge; terminal callbacks ignore the envelope and preserve existing verdict behavior.
- A pending dispatcher first tests the retained envelope, then selectively receives ordinary matching clauses while carrying the older envelope through continuing transitions.
- Test checkpoints acknowledge the logical receive-state identity and recur in either ordinary or pending mode.

## Deviations from Plan

None - plan executed exactly as written.

## Issues Encountered

- The plan's literal purity command matched `ets:` inside the existing module name `sets:`. A token-boundary equivalent confirmed that `agm_engine.erl` contains no ETS calls or other orchestration effects.

## Known Stubs

- `detecter/src/synthesis/maxhml_eval.erl:107` - Pre-existing variable-normalization TODO, already deferred to Phase 22.
- `detecter/src/synthesis/maxhml_eval.erl:637` and `:676` - Pre-existing init-block refactor TODOs; unrelated to replay and unchanged by this plan.
- `detecter/src/synthesis/maxhml_eval.erl:638` - Pre-existing fixed-start-state TODO; retained because start-state redesign is outside Phase 5.

No new runtime stub or placeholder was introduced.

## User Setup Required

None - no external service configuration required.

## Next Phase Readiness

- Plan 05-02 can now compare complete and recovered executions over the same ordered envelope semantics.
- All four withholding reasons retain pre-recovery ETS state and remain non-verdicting.
- Wave 2 remains the next and only execution target; Phase 6 and later remain untouched.

## Self-Check: PASSED

All five owned implementation/test files and this summary were verified on disk. Repository history was intentionally left unchanged.

---
*Phase: 05-irrevocability-and-verdict-semantics*
*Completed: 2026-07-02*
