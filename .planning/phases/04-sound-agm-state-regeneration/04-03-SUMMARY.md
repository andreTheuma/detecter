---
phase: 04-sound-agm-state-regeneration
plan: 04-03
subsystem: testing
tags: [erlang, eunit, generated-code, agm, missing-events]
requires:
  - phase: 04-02
    provides: Singleton state and monitoring-consequence recovery contract
provides:
  - Permanent generated-module AGM characterization suite
  - Runtime evidence for concrete and symbolic consequence behavior
  - Default-test regression gate for Phase 4.1 extraction
affects: [04.1-agm-engine-extraction, phase-5-verdict-semantics, thesis-evidence]
tech-stack:
  added: []
  patterns:
    - Generated modules are compiled with test-only export_all into isolated temporary directories
    - Tests exercise generated helpers and actual generated monitor states
key-files:
  created:
    - detecter/test/regeneration/generated_agm_recovery_test.erl
  modified:
    - detecter/Makefile
key-decisions:
  - "Test event ambiguity separately from monitoring-consequence ambiguity."
  - "Characterize both supported symbolic proof and conservative unproven withholding."
patterns-established:
  - "A continuation test waits for the generated monitor to enter its next receive state instead of using arbitrary sleep-based success."
requirements-completed: [AGM-01, AGM-02, AGM-03, AGM-04, AGM-05]
duration: 35 min
completed: 2026-07-01
status: complete
---

# Phase 4 Plan 04-03: AGM Monitoring-Consequence Regression Tests Summary

**Eight generated-module tests now distinguish state ambiguity, event ambiguity, consequence ambiguity, and symbolic proof outcomes**

## Performance

- **Duration:** 35 min
- **Started:** 2026-07-01T13:12:00Z
- **Completed:** 2026-07-01T13:45:35Z
- **Tasks:** 9
- **Files modified:** 2

## Accomplishments

- Added runtime tests for unique, ambiguous, and impossible inferred state sets.
- Proved that multiple literal events proceed when their complete consequences agree and withhold when they conflict.
- Proved that a supported symbolic range proceeds when uniformity is established, withholds when it crosses a guard boundary, and withholds when the guard proof is unsupported.
- Verified `{known, Event}` only for one literal descriptor and `unknown` metadata for multi-event and symbolic recovery.
- Added the suite to the default `make test` target after generated-source smoke tests.

## Task Commits

No commit was created. The user requested that Plan 04-03 remain uncommitted for review.

## Files Created/Modified

- `detecter/test/regeneration/generated_agm_recovery_test.erl` - Generates isolated monitor modules, compiles them with test-only exports, seeds ETS state, supplies the next trace event, and asserts recovery/consequence behavior.
- `detecter/Makefile` - Runs `generated_agm_recovery_test` in the default test target.

## Decisions Made

- Tests call generated AGM helpers to inspect recovery maps and optional event metadata, then invoke actual generated monitor states to verify continuation, terminal verdict, and withholding behavior.
- The unsupported symbolic case uses a generated property with non-complementary guards, ensuring `unproven_consequence` is evidence from emitted monitor code rather than a hand-written stand-in.
- Continuation success is synchronized by observing the generated receive-state function; the suite does not classify a process as successful merely because it remained alive for a fixed delay.

## Deviations from Plan

One strengthening was added: the plan allowed either a conflicting or unproven symbolic test, while the suite covers both outcomes separately.

## Issues Encountered

- The local GSD helper still fails while loading `../../../package.json`, so tracking artifacts were updated directly.
- Generator trace logging makes the test output verbose but does not affect the test result.

## User Setup Required

None.

## Next Phase Readiness

- Phase 4's behavior is permanently characterized and ready to guard Phase 4.1 AGM engine extraction.
- Phase 5 remains blocked until extraction and chapter-by-chapter thesis reconciliation are complete.

## Self-Check: PASSED

- Focused `generated_agm_recovery_test`: 8/8 passed.
- Full `make test`: 24 tracing, 5 parser, 3 generated smoke, and 8 AGM recovery tests passed.
- Full thesis `latexmk` build succeeded and produced the 129-page PDF; existing duplicate-label and layout/font warnings remain.
- `git diff --check` passed before documentation close-out.

---
*Phase: 04-sound-agm-state-regeneration*
*Completed: 2026-07-01*
