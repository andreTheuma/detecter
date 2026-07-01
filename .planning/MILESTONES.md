# Project Milestones: detectEr Thesis Stabilization

No milestones have shipped yet.

## Planned Milestones

### v0.1 Thesis Claim Stabilization

**Goal:** Make the implementation support the documentation claims about modular synthesis, automaton-guided regeneration, soundness, and irrevocability.

**Phases:** 1-5, including inserted Phase 4.1

**Status:** In progress

**Current progress:**
- Phase 1 plan 01-01 is complete.
- Phase 1 plan 01-02 is complete.
- Phase 1 plan 01-03 is complete.
- Phase 1 is complete.
- Phase 2 plan 02-01 is complete.
- Phase 2 plan 02-02 is complete.
- Phase 2 is complete.
- Phase 3 plan 03-01 is complete.
- Phase 3 plan 03-02 is complete.
- Phase 3 is complete.
- `make test` reaches EUnit and runs `log_tracer_test`, `sys_info_parser_test`, `generated_monitor_smoke_test`, `generated_agm_recovery_test`, and `agm_engine_test`.
- `generated_monitor_smoke_test` now compiles generated Erlang for `prop_no_leak`, `prop_no_failure`, and `prop_correct_start`.
- `tracer_test` remains manual because it is timing-dependent; the Makefile documents the manual command.
- Phase 4 context, research, and plans are captured.
- Phase 4 plan 04-01 is complete: generated transition rows preserve multiplicity and retain literal/symbolic event descriptors alongside condition predicates.
- Generated AGM helpers now consume transition lists directly, and smoke tests reject map-era helper calls.
- Phase 4 plan 04-02 is complete: generated monitors infer a singleton state, compare complete monitoring consequences across every compatible event descriptor, and treat exact event recovery as optional metadata.
- Phase 4 plan 04-03 is complete: eight generated-module semantic tests cover unique, ambiguous, impossible, equal/conflicting multi-event, and uniform/conflicting/unproven symbolic recovery.
- Thesis implementation, methodology, and discussion text reflects transition rows, singleton state recovery, deterministic monitoring consequences, and explicit withholding.
- Thesis state indexing is fixed: `X0` is immediately before the missing event and `X1` is immediately after it; Phase 4 recovery code must use the same convention.
- Phase 4 is complete and establishes the characterization gate for inserted Phase 4.1.
- Phase 4.1 plan 04.1-01 is complete: `agm_engine` provides a pure runtime interface with 13 focused checks.
- Phase 4.1 plan 04.1-02 is complete: `maxhml_agm_codegen` owns model/property-specific AGM AST generation, and a ninth generated-module check verifies the explicit engine dependency.
- Phase 4.1 plan 04.1-03 is complete: all eight supplied thesis chapters were audited, four amended and four verified unchanged, and the 129-page thesis builds.
- Phase 4.1 is complete. Next task: Phase 5 verdict equivalence and irrevocability.

### v0.2 System Information Robustness

**Goal:** Make `.spec` parsing safe, explicit, and maintainable beyond the thesis-critical examples.

**Phases:** 6-9

**Status:** Planned

### v0.3 Runtime Hardening

**Goal:** Reduce runtime risks that could undermine experiments but are not central to AGM correctness.

**Phases:** 10-13

**Status:** Planned

### v0.4 Research Evidence and Thesis Traceability

**Goal:** Make the final implementation auditable against the thesis narrative.

**Phases:** 14-17

**Status:** Planned

### v0.5 Deferred Cleanup

**Goal:** Track useful cleanup that should not distract from thesis stabilization.

**Phases:** 18-22

**Status:** Backlog

---
*Last updated: 2026-07-01 after Phase 4.1 completion.*
