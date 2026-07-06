# Roadmap: detectEr Thesis Stabilization

## Overview

This roadmap tracks the remediation work needed to bring the detectEr codebase into alignment with the thesis claims around modular monitor synthesis, automaton-guided monitoring, deterministic state regeneration, sound verdicts, and irrevocability. The first milestone is thesis-critical. Later milestones capture robustness and cleanup work discovered during the audit so it is not lost.

## Milestones

- **v0.1 Thesis Claim Stabilization** - Phases 1-5, including inserted Phase 4.1. Current and final thesis-critical implementation milestone; stop for review after Phase 5.
- **v0.2 System Information Robustness** - Phases 6-9. Future milestone.
- **v0.3 Runtime Hardening** - Phases 10-13. Future milestone.
- **v0.4 Research Evidence and Thesis Traceability** - Phases 14-17. Future milestone.
- **v0.5 Deferred Cleanup** - Phases 18-22. Future milestone/backlog.

Phases 6 through 22 are preserved for future work and must not be entered automatically after completing `v0.1`.

## Phases

### v0.1 Thesis Claim Stabilization

**Milestone Goal:** Make the implementation support the documentation claims about modular synthesis, automaton-guided regeneration, soundness, and irrevocability.

#### Phase 1: Test Harness Baseline
**Goal:** Restore a trustworthy default test signal before changing synthesis semantics.
**Depends on:** Nothing.
**Success Criteria** (what must be TRUE):
  1. `make test` does not compile generated `test/**/ebin/**` artifacts as hand-written test source.
  2. Regeneration parser tests run through the default test target.
  3. A generated monitor compile smoke test exists for a representative recursive regeneration property.
**Plans:** TBD

Plans:
- [x] 01-01: Exclude generated regeneration artifacts from test-source compilation.
- [x] 01-02: Add parser and generated-monitor smoke tests to the default test flow.
- [x] 01-03: Document any intentionally excluded timing-sensitive tests.

#### Phase 2: Parser Contract Alignment
**Goal:** Make the system-information parser contract explicit and tested.
**Depends on:** Phase 1.
**Success Criteria** (what must be TRUE):
  1. `sys_info_parser_test` matches the parser's symbolic output contract.
  2. START/NULL handling is consistent with the thesis examples and `priv/sys_info.spec`.
  3. Parser tests clean up temporary files reliably.
**Plans:** TBD

Plans:
- [x] 02-01: Update stale parser expectations.
- [x] 02-02: Add focused tests for START, NULL, integers, symbolic ranges, and set-minus guards.

#### Phase 3: Generated Monitor Compile Correctness
**Goal:** Ensure modular synthesis emits Erlang modules that compile cleanly.
**Depends on:** Phase 2.
**Success Criteria** (what must be TRUE):
  1. Generated modules no longer contain invalid `update_current_state/0` calls.
  2. Representative regeneration properties compile from generated source.
  3. Regression coverage catches future generator arity mistakes.
**Plans:** TBD

Plans:
- [x] 03-01: Fix state-update arity generation in init and verdict paths.
- [x] 03-02: Add compile checks for representative generated monitors.

#### Phase 4: Sound AGM State Regeneration
**Goal:** Make missing-event regeneration depend on singleton state inference and a deterministic monitoring consequence.
**Depends on:** Phase 3.
**Success Criteria** (what must be TRUE):
  1. Missing-event handling never treats a list of candidate states as a single state.
  2. Duplicate state-pair transitions are preserved so every compatible event consequence is considered.
  3. The monitor proceeds only after singleton state inference and a singleton complete consequence signature.
  4. Several concrete or symbolic events may proceed only when all represented events have one proven monitoring consequence.
  5. Impossible or ambiguous state inference, conflicting consequences, and unproven symbolic consequences return `withhold` rather than a verdict.
**Plans:** TBD

Plans:
- [x] 04-01: Preserve transition multiplicity in generated system-information data.
- [x] 04-02: Replace current transition validation with singleton state and monitoring-consequence resolution.
- [x] 04-03: Add tests for unique, ambiguous, impossible, multi-event, and symbolic missing-event cases.

#### Phase 4.1: AGM Engine Extraction and Thesis Reconciliation
**Goal:** Extract reusable AGM runtime and code-generation responsibilities from `maxhml_eval.erl`, then reconcile every supplied thesis chapter with the post-refactor architecture and verified behavior.
**Depends on:** Phase 4, including Plan 04-03 characterization tests.
**Blocks:** Phase 5.
**Success Criteria** (what must be TRUE):
  1. Pure AGM state inference, descriptor membership, and consequence aggregation live behind an explicit runtime interface.
  2. Property-specific AGM code generation is separated from maxHML semantic traversal.
  3. Focused engine tests and all Phase 4 generated-monitor characterization tests pass unchanged.
  4. Every supplied thesis chapter is recorded as amended or verified unchanged with code and test evidence.
  5. Implementation listings and architectural claims describe the post-refactor modules and runtime dependency accurately.
**Plans:** 3

Planning scope: `.planning/phases/04.1-agm-engine-extraction-and-thesis-reconciliation/04.1-SCOPE.md`

Plans:
- [x] 04.1-01: Extract and directly test the pure AGM runtime engine.
- [x] 04.1-02: Extract AGM code generation and generated runtime adapters from `maxhml_eval`.
- [x] 04.1-03: Reconcile all supplied thesis chapters and regenerate verification evidence.

#### Phase 5: Irrevocability and Verdict Semantics
**Goal:** Verify that generated verdicts are sound and cannot be retracted after missing-event recovery.
**Depends on:** Phase 4.1.
**Success Criteria** (what must be TRUE):
  1. A recovered trace produces the same verdict as its corresponding complete trace when recovery is deterministic.
  2. Ambiguous missing traces do not produce eager rejection or acceptance verdicts.
  3. Once a verdict is emitted, later trace extension or recovered missing-event detail cannot retract it.
**Plans:** 3

Plans:

**Wave 1**
- [x] 05-01: Preserve and directly replay the lookahead after one atomic post-agreement recovery commit.

**Wave 2** *(blocked on Wave 1 completion)*
- [x] 05-02: Prove complete/recovered observable equivalence and conservative withholding with deterministic tests.

**Wave 3** *(blocked on Wave 2 completion)*
- [x] 05-03: Prove terminal irrevocability, reconcile every active thesis chapter, and close v0.1.

Cross-cutting constraints:
- Preserve the exact lookahead envelope and ordinary selective-receive behavior across replay and recursive bridge states.
- Ambiguous or unproven recovery emits no verdict and commits no recovery state.
- Claim-critical tests use causal synchronization rather than sleeps or process-state polling.
- Phase 5 ends the thesis-critical milestone; no Phase 6+ execution follows automatically.

#### Phase 5.2: Synthesis Soundness Gate
**Goal:** Close the two critical audit findings that let synthesis emit unsound or corrupt monitors, plus the mechanical fixes the audit bundled with them (`.planning/AUDIT-v0.1-manual-review.md` C1, C2, H1, H2, M3, M6, L5).
**Depends on:** Phase 5 (committed).
**Success Criteria** (what must be TRUE):
  1. Property shapes outside the supported fragment are rejected at synthesis with a structured error; no monitor file is emitted (audit C1; also rejects multi-property files, audit M5).
  2. Compiling multiple properties in one VM produces independent, compilable monitors; generation state is compilation-scoped, not `persistent_term` (audit C2).
  3. The generated init block derives the initial state from the model's START row (fallback `s0`) and no longer feeds spawn arguments into `update_current_state/1` (audit H2), proven by an end-to-end `flu_spec/0` test.
  4. No `erlang:get_stacktrace/0` call sites remain (audit H1).
  5. Consequence-signature deduplication uses `=:=` semantics (audit M3).
  6. `make compile` shows warnings (`-W0` removed) and compiles the synthesis/regeneration modules warning-free (audit M6).
**Plans:** 2

Plans:
- [ ] 5.2-01: Fragment validation and compilation-scoped generation state, with regression tests.
- [ ] 5.2-02: Init-state derivation, stacktrace/dedup/warning fixes, streamer relocation, thesis reconciliation.

### v0.2 System Information Robustness

**Milestone Goal:** Make `.spec` parsing safe, explicit, and maintainable beyond the thesis-critical examples.

#### Phase 6: Specification Grammar Definition
**Goal:** Define the accepted system-information grammar in one place.
**Depends on:** v0.1.
**Success Criteria** (what must be TRUE):
  1. Supported states, events, symbolic ranges, and guards are documented.
  2. Unsupported constructs are called out explicitly.
  3. Parser tests map directly to the documented grammar.
**Plans:** TBD

Plans:
- [ ] 06-01: Document the accepted `.spec` grammar.
- [ ] 06-02: Mark deferred grammar features with clear examples.

#### Phase 7: Parser Error Model
**Goal:** Replace crashy parser behavior with structured parse results.
**Depends on:** Phase 6.
**Success Criteria** (what must be TRUE):
  1. Valid files return parsed transitions through a stable success shape.
  2. Malformed files return structured errors rather than pattern-match crashes.
  3. Tests cover malformed lines, unsupported conditions, and invalid guards.
**Plans:** TBD

Plans:
- [ ] 07-01: Introduce structured parser success and error returns.
- [ ] 07-02: Update generator call sites to handle parser failures deliberately.
- [ ] 07-03: Add negative parser tests.

#### Phase 8: Condition Language Completion
**Goal:** Either support or deliberately reject richer event-condition syntax.
**Depends on:** Phase 7.
**Success Criteria** (what must be TRUE):
  1. Union, disjunction, strings, and multiple guards have explicit behavior.
  2. Unsupported syntax fails with a useful parser error.
  3. Supported syntax is tested through parser and generated-code paths.
**Plans:** TBD

Plans:
- [ ] 08-01: Decide which condition constructs belong in scope.
- [ ] 08-02: Implement or reject each construct with tests.

#### Phase 9: Atom Safety
**Goal:** Remove or constrain unbounded atom creation from system-information input.
**Depends on:** Phase 7.
**Success Criteria** (what must be TRUE):
  1. Untrusted or very large `.spec` files cannot exhaust the Erlang atom table through parser input.
  2. State/event identifiers have a documented representation.
  3. Tests cover rejected unknown atoms or safe binary/string identifiers.
**Plans:** TBD

Plans:
- [ ] 09-01: Replace unbounded `list_to_atom/1` where feasible.
- [ ] 09-02: Add parser safety tests.

### v0.3 Runtime Hardening

**Milestone Goal:** Reduce runtime risks that could undermine experiments but are not central to AGM correctness.

#### Phase 10: Unknown Process Deletion Safety
**Goal:** Prevent tracer crashes when deletion events reference unknown traced processes.
**Depends on:** v0.1.
**Success Criteria** (what must be TRUE):
  1. `tracer:del_proc/2` handles absent process IDs without `badkey`.
  2. Existing route cleanup behavior remains intact.
  3. Tests cover unknown delete events.
**Plans:** TBD

Plans:
- [ ] 10-01: Fix the unknown-process lookup path.
- [ ] 10-02: Add tracer regression tests.

#### Phase 11: ETS State Isolation
**Goal:** Reduce public mutable state risks in tracing and generated monitor state.
**Depends on:** Phase 10.
**Success Criteria** (what must be TRUE):
  1. Generated monitor state does not collide across independent monitor runs.
  2. ETS access mode is no broader than necessary.
  3. Tests prove unrelated processes cannot corrupt critical monitor state where feasible.
**Plans:** TBD

Plans:
- [ ] 11-01: Scope generated monitor state per monitor/module/session.
- [ ] 11-02: Review public ETS tables and tighten access where practical.

#### Phase 12: Deprecated Stacktrace API Replacement
**Goal:** Remove compilation warnings tied to `erlang:get_stacktrace/0`.
**Depends on:** Phase 10.
**Success Criteria** (what must be TRUE):
  1. Deprecated stacktrace calls are replaced with modern `try ... catch Class:Reason:Stacktrace` patterns.
  2. Compilation no longer emits those deprecation warnings.
  3. Error reporting behavior remains useful.
**Plans:** TBD

Plans:
- [ ] 12-01: Update stacktrace handling in synthesis and monitoring modules.
- [ ] 12-02: Re-run compile and targeted error-path tests.

#### Phase 13: Timing-Sensitive Tracer Test Stabilization
**Goal:** Make tracer routing tests reliable enough to run regularly.
**Depends on:** Phase 10.
**Success Criteria** (what must be TRUE):
  1. Timing-sensitive tests use deterministic synchronization where practical.
  2. The default test target includes only stable tests.
  3. Any intentionally manual/flaky tests are clearly documented.
**Plans:** TBD

Plans:
- [ ] 13-01: Replace sleeps with synchronization helpers.
- [ ] 13-02: Decide which tracer tests belong in `make test`.

### v0.4 Research Evidence and Thesis Traceability

**Milestone Goal:** Make the final implementation auditable against the thesis narrative.

#### Phase 14: Claim-to-Code Traceability
**Goal:** Link thesis claims to implementation points and tests.
**Depends on:** v0.1.
**Success Criteria** (what must be TRUE):
  1. Modular synthesis, AGM, soundness, and irrevocability each map to code and tests.
  2. Known limitations are documented beside the relevant implementation.
  3. The documentation avoids claims broader than the code supports.
**Plans:** TBD

Plans:
- [ ] 14-01: Create a traceability matrix for thesis claims.
- [ ] 14-02: Link each claim to code modules and tests.

#### Phase 15: AGM Experiment Suite
**Goal:** Provide reproducible examples that demonstrate deterministic recovery and withholding.
**Depends on:** Phase 5.
**Success Criteria** (what must be TRUE):
  1. Example traces cover complete, uniquely recoverable, ambiguous, and invalid missing-event cases.
  2. The expected verdict or withholding behavior is checked automatically.
  3. The suite can be run from a documented command.
**Plans:** TBD

Plans:
- [ ] 15-01: Build small AGM example traces.
- [ ] 15-02: Automate expected verdict checks.

#### Phase 16: Thesis Documentation Cross-Check
**Goal:** Reconcile thesis wording with the final code behavior.
**Depends on:** Phase 15.
**Success Criteria** (what must be TRUE):
  1. Thesis implementation sections match actual module names and behavior.
  2. Limitations around missing events, non-contiguous gaps, and operational models are stated precisely.
  3. Results and discussion do not overclaim beyond tested behavior.
**Plans:** TBD

Plans:
- [ ] 16-01: Review thesis files against final implementation.
- [ ] 16-02: Produce a short edit list for documentation updates.

#### Phase 17: Developer Notes for Future Readers
**Goal:** Capture how to work on modular synthesis and AGM without reverse-engineering the generator.
**Depends on:** Phase 15.
**Success Criteria** (what must be TRUE):
  1. A developer note explains generated monitor structure.
  2. A developer note explains system-information specs and missing-event recovery.
  3. Build and test commands are documented.
**Plans:** TBD

Plans:
- [ ] 17-01: Add synthesis/AGM developer documentation.
- [ ] 17-02: Add build and verification notes.

### v0.5 Deferred Cleanup

**Milestone Goal:** Track useful cleanup that should not distract from thesis stabilization.

#### Phase 18: Dynamic Attach/Detach Decision
**Goal:** Decide whether stubbed online attach/detach APIs should be implemented or removed.
**Depends on:** v0.1.
**Success Criteria** (what must be TRUE):
  1. The public API does not imply dynamic monitor attachment if it is not implemented.
  2. If implemented, attach/detach behavior has lifecycle tests.
  3. If removed, callers and docs no longer mention it as available behavior.
**Plans:** TBD

Plans:
- [ ] 18-01: Audit attach/detach references.
- [ ] 18-02: Implement or remove the stubs.

#### Phase 19: AST Helper Consolidation
**Goal:** Reduce duplicated manual Erlang AST construction across weavers.
**Depends on:** v0.1.
**Success Criteria** (what must be TRUE):
  1. Common AST helpers are centralized or clearly separated by purpose.
  2. Existing weaver behavior remains unchanged.
  3. Compile/weave tests cover migrated helpers.
**Plans:** TBD

Plans:
- [ ] 19-01: Identify shared AST helper patterns.
- [ ] 19-02: Migrate low-risk helpers behind tests.

#### Phase 20: Build Reproducibility
**Goal:** Make build and analysis commands easier to reproduce across environments.
**Depends on:** v0.1.
**Success Criteria** (what must be TRUE):
  1. Required Erlang/OTP version expectations are documented.
  2. `make compile`, `make test`, and `make analyze` behavior is documented or mapped to a standard tool.
  3. Local-only generated files and virtualenvs are excluded from source workflows.
**Plans:** TBD

Plans:
- [ ] 20-01: Document build prerequisites.
- [ ] 20-02: Consider minimal `rebar3` support or strengthen Makefile docs.

#### Phase 21: Generated Fixture Layout Cleanup
**Goal:** Separate hand-written tests from generated regeneration artifacts.
**Depends on:** Phase 1.
**Success Criteria** (what must be TRUE):
  1. Generated Erlang fixtures no longer live in a confusing source-like `ebin` path.
  2. `.gitignore` and test compilation rules prevent accidental fixture compilation.
  3. Regeneration fixture purpose is documented.
**Plans:** TBD

Plans:
- [ ] 21-01: Move or rename generated fixture directories.
- [ ] 21-02: Update ignore/build rules and documentation.

#### Phase 22: Generator Variable Name Normalization
**Goal:** Normalize variable-name representation inside synthesis helpers.
**Depends on:** v0.1.
**Success Criteria** (what must be TRUE):
  1. Synthesis helper APIs use one documented representation for variable names.
  2. State-update generation no longer needs mixed atom/string exclusions such as `'From'` and `"From"`.
  3. Regression tests cover init, action, verdict, and recursive paths after normalization.
**Plans:** TBD

Plans:
- [ ] 22-01: Audit generator variable extraction and function-argument helpers.
- [ ] 22-02: Normalize variable names and update generator tests.

## Progress

| Phase | Milestone | Plans Complete | Status | Completed |
|-------|-----------|----------------|--------|-----------|
| 1. Test Harness Baseline | v0.1 | 3/3 | Complete | 2026-06-24 |
| 2. Parser Contract Alignment | v0.1 | 2/2 | Complete | 2026-06-24 |
| 3. Generated Monitor Compile Correctness | v0.1 | 2/2 | Complete | 2026-06-26 |
| 4. Sound AGM State Regeneration | v0.1 | 3/3 | Complete | 2026-06-29 |
| 4.1 AGM Engine Extraction and Thesis Reconciliation | v0.1 | 3/3 | Complete | 2026-07-01 |
| 5. Irrevocability and Verdict Semantics | v0.1 | 3/3 | Complete | 2026-07-06 |
| 5.2 Synthesis Soundness Gate | v0.1 | 0/2 | In progress | - |
| 6. Specification Grammar Definition | v0.2 | 0/2 | Planned | - |
| 7. Parser Error Model | v0.2 | 0/3 | Planned | - |
| 8. Condition Language Completion | v0.2 | 0/2 | Planned | - |
| 9. Atom Safety | v0.2 | 0/2 | Planned | - |
| 10. Unknown Process Deletion Safety | v0.3 | 0/2 | Planned | - |
| 11. ETS State Isolation | v0.3 | 0/2 | Planned | - |
| 12. Deprecated Stacktrace API Replacement | v0.3 | 0/2 | Planned | - |
| 13. Timing-Sensitive Tracer Test Stabilization | v0.3 | 0/2 | Planned | - |
| 14. Claim-to-Code Traceability | v0.4 | 0/2 | Planned | - |
| 15. AGM Experiment Suite | v0.4 | 0/2 | Planned | - |
| 16. Thesis Documentation Cross-Check | v0.4 | 0/2 | Planned | - |
| 17. Developer Notes for Future Readers | v0.4 | 0/2 | Planned | - |
| 18. Dynamic Attach/Detach Decision | v0.5 | 0/2 | Backlog | - |
| 19. AST Helper Consolidation | v0.5 | 0/2 | Backlog | - |
| 20. Build Reproducibility | v0.5 | 0/2 | Backlog | - |
| 21. Generated Fixture Layout Cleanup | v0.5 | 0/2 | Backlog | - |
| 22. Generator Variable Name Normalization | v0.5 | 0/2 | Backlog | - |

---
*Created: 2026-06-24 after codebase and thesis-claim audit.*
