---
phase: 05-irrevocability-and-verdict-semantics
plan: "02"
subsystem: generated-monitor-verification
tags: [erlang, eunit, agm, verdict-equivalence, selective-receive]

requires:
  - phase: 05-irrevocability-and-verdict-semantics
    plan: "01"
    provides: Atomic recovery commit, exact lookahead replay, replay bridges, and test-only checkpoints
provides:
  - Complete/recovered observable equivalence for exact yes and no verdicts
  - Multi-candidate single-consequence equivalence with unknown event metadata
  - Causally synchronized continuation, retention, and recursive/max bridge evidence
  - Complete four-reason withholding lifecycle matrix
affects: [05-03, VERD-03, thesis-verdict-evidence]

tech-stack:
  added: []
  patterns:
    - Shared logical sequences encoded as complete envelopes or one missing marker plus preserved lookahead
    - Start-gated worker lifecycle with exact post-termination verdict collection
    - Byte-for-byte two-entry recovery-state snapshots around withholding

key-files:
  created: []
  modified:
    - detecter/test/regeneration/generated_agm_recovery_test.erl

key-decisions:
  - "Construct complete and recovered executions from one equivalent_encodings/3 helper so event values and envelope forms cannot drift."
  - "Use the supported recursive/conjunction fixture for retention evidence: a newer receive transition makes the older pending send envelope relevant."
  - "Queue the missing marker, lookahead, and extension before the phase5_start tag for every withholding lifecycle."

patterns-established:
  - "Observable equivalence: compare exact terminal verdict lists or tagged receive-state checkpoints with empty verdict lists."
  - "Withholding lifecycle: await the internal result and normal DOWN before comparing state snapshots and collecting verdicts."

requirements-completed: [VERD-01, VERD-02]

duration: 10 min
completed: 2026-07-02
status: complete
review_state: uncommitted_pending_manual_review
---

# Phase 5 Plan 2: Observable Equivalence and Conservative Withholding Summary

**Complete and recovered logical traces now have deterministic paired evidence for exact verdicts and continuations, while every withholding reason terminates normally without verdict or recovery-state mutation.**

## Performance

- **Duration:** 10 min
- **Started:** 2026-07-02T10:47:26Z
- **Completed:** 2026-07-02T10:57:23Z
- **Tasks:** 2
- **Files modified:** 1

## Accomplishments

- Added exact complete/recovered `yes` and `no` rows that assert one exact verdict after normal worker termination.
- Added multi-candidate recovery evidence with one consequence and `event => unknown`.
- Proved equivalent non-verdict checkpoints, pending-envelope retention across a newer transition, and unchanged recursive/max continuation identity.
- Standardized all four stable withholding reasons on queued-before-start inputs, normal termination, unchanged ETS snapshots, and exact empty verdict lists.
- Made repeated fresh-VM suite execution collision-safe.

## Review State

Implemented and verified, but intentionally left unstaged and uncommitted for manual Phase 5 review.

1. **Task 1:** Added complete-versus-recovered observable equivalence cases.
2. **Task 2:** Completed the withholding lifecycle matrix.
3. **Overall verification fix:** Made focused runs collision safe.

## Files Created/Modified

- `detecter/test/regeneration/generated_agm_recovery_test.erl` - Adds the bounded equivalence matrix, symmetric lifecycle helpers, four-reason withholding assertions, and collision-safe temporary fixture directories.

## Verification

- `make -C detecter compile-test` - passed.
- Focused `generated_agm_recovery_test` - 17/17 passed.
- Three consecutive fresh-VM focused executions - passed.
- `make -C detecter test` - passed.
- Claim-critical polling scan - passed; no `timer:sleep` or `process_info`.

## Decisions Made

- Complete and recovered message lists are derived from the same missing-event value, lookahead value, and suffix.
- Continuing comparisons use reference-tagged generated checkpoints before and after the common suffix rather than elapsed-time evidence.
- Withholding observations remain white-box results; no production verdict or protocol was added.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Made generated fixture directories unique across fresh Erlang VMs**
- **Found during:** Overall repeated-run verification
- **Issue:** `erlang:unique_integer/1` restarts in each VM, so a stale directory could make the second focused execution fail with `{error,eexist}`.
- **Fix:** Added nanosecond system time to each candidate name, retained the per-VM unique integer, and retry on the remaining collision race.
- **Files modified:** `detecter/test/regeneration/generated_agm_recovery_test.erl`
- **Verification:** Three consecutive fresh-VM focused executions and the full default suite pass.
- **Review state:** Included in the unstaged Phase 5 working tree.

---

**Total deviations:** 1 auto-fixed (1 bug).
**Impact on plan:** The fix is confined to the owned test helper and is required for the plan's deterministic repeated-run gate.

## Issues Encountered

- An initial nested retention fixture generated unsupported replay targets and unbound continuation arguments. The final test uses the existing supported recursive/conjunction fixture, where a newer receive event advances state and causes the retained older send envelope to be reconsidered.

## Known Stubs

None - no stub, placeholder, TODO, or mock data path was introduced.

## User Setup Required

None - no external service configuration required.

## Next Phase Readiness

- Plan 05-03 can use the exact verdict collector and gated lifecycle to prove terminal irrevocability.
- VERD-01 and VERD-02 have bounded deterministic evidence; VERD-03 remains for Wave 3.
- Phase 5 remains in progress. No Phase 6 or later work was started.

## Self-Check: PASSED

The owned test module and this summary were verified on disk. Repository history was intentionally left unchanged.

---
*Phase: 05-irrevocability-and-verdict-semantics*
*Completed: 2026-07-02*
