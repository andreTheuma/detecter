---
phase: 5
slug: irrevocability-and-verdict-semantics
status: draft
nyquist_compliant: true
wave_0_complete: false
created: 2026-07-02
---

# Phase 5 - Validation Strategy

> Per-phase validation contract for feedback sampling during execution.

---

## Test Infrastructure

| Property | Value |
|----------|-------|
| **Framework** | EUnit bundled with Erlang/OTP |
| **Config file** | `detecter/Makefile` |
| **Quick run command** | `make -C detecter compile-test && cd detecter && erl -noshell -pa ebin -eval 'case eunit:test(generated_agm_recovery_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop` |
| **Full suite command** | `make -C detecter test` |
| **Estimated runtime** | ~20 seconds |

---

## Sampling Rate

- **After every task commit:** Run the focused EUnit module named in that task.
- **After every plan wave:** Run `make -C detecter test`.
- **Before `$gsd-verify-work`:** The full code suite and complete thesis build must pass.
- **Max feedback latency:** 30 seconds for code-only checks.

---

## Per-Task Verification Map

| Task ID | Plan | Wave | Requirement | Threat Ref | Secure Behavior | Test Type | Automated Command | File Exists | Status |
|---------|------|------|-------------|------------|-----------------|-----------|-------------------|-------------|--------|
| 05-01-01 | 01 | 1 | VERD-01, VERD-02 | Pure-engine boundary | Callback remains inert inside `agm_engine` | unit | focused `agm_engine_test` | yes | pending |
| 05-01-02 | 01 | 1 | VERD-01, VERD-02 | Envelope/ETS/checkpoint boundaries | Exact replay with pending-envelope retention; recursive/max bridge routing; post-agreement atomic commit; Wave 1 production checkpoint exclusion | generated integration | focused generated AGM suite plus clean production generation/source scan | yes | pending |
| 05-02-01 | 02 | 2 | VERD-01 | Complete/recovered correspondence | Same logical trace has the same observable consequence, including retention across a later transition and recursive/max continuation targets | generated integration | focused generated AGM suite, repeated three times | yes | pending |
| 05-02-02 | 02 | 2 | VERD-02 | Withholding lifecycle | Start-gated queued inputs; stable reason; unchanged state; no verdict; normal exit | generated integration | focused generated AGM suite | yes | pending |
| 05-03-01 | 03 | 3 | VERD-03 | Terminal output boundary | Tagged start gate proves queue residency; exactly one verdict despite queued and later extensions | generated integration | focused generated AGM suite, repeated three times | yes | pending |
| 05-03-02 | 03 | 3 | VERD-01, VERD-02, VERD-03 | Academic claim boundary | Thesis claims match bounded evidence and retained limitations | document build | `latexmk` full thesis build | yes | pending |
| 05-03-03 | 03 | 3 | VERD-01, VERD-02, VERD-03 | Milestone boundary | Full evidence recorded; no Phase 6+ execution | regression and artifact audit | `make test`, production generation scan, phase artifact checks | yes | pending |

---

## Wave 0 Requirements

- [ ] Generate a test-only reference-tagged checkpoint without changing the production monitor protocol.
- [ ] Generate an explicit production no-op checkpoint branch and prove production-generated source has no checkpoint control tag in Wave 1.
- [ ] Generate pending-envelope dispatchers that retain unmatched replay across state transitions and bridges from recursive/max consequence identities to actual receive states.
- [ ] Add monitored-worker and exact-verdict collection helpers.
- [ ] Add acceptance, rejection, continuing-prefix, duplicate-event, and withholding fixtures.
- [ ] Add a gated worker-start helper that proves decisive and extension messages are queued before execution.
- [ ] Add retention-sensitive and recursive/max bridge fixtures that fail on envelope discard or non-receiving callback targets.

These requirements are delivered by Plan 05-01 before the wider equivalence and irrevocability matrices depend on them.

---

## Manual-Only Verifications

All phase behaviors have automated verification. Chapter dispositions require human review of wording, but the thesis build and source-claim scans are automated.

---

## Validation Sign-Off

- [x] All tasks have automated verification or explicit Wave 0 dependencies.
- [x] Sampling continuity has no three consecutive tasks without automated verification.
- [x] Wave 0 covers all missing synchronization and fixture references.
- [x] Commands use no watch-mode flags.
- [x] Expected code feedback latency is below 30 seconds.
- [x] `nyquist_compliant: true` is set in frontmatter.

**Approval:** pending execution
