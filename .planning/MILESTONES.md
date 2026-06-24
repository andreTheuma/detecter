# Project Milestones: detectEr Thesis Stabilization

No milestones have shipped yet.

## Planned Milestones

### v0.1 Thesis Claim Stabilization

**Goal:** Make the implementation support the documentation claims about modular synthesis, automaton-guided regeneration, soundness, and irrevocability.

**Phases:** 1-5

**Status:** In progress

**Current progress:**
- Phase 1 plan 01-01 is complete.
- Phase 1 plan 01-02 is complete.
- Phase 1 plan 01-03 is complete.
- Phase 1 is complete.
- Phase 2 plan 02-01 is complete.
- Phase 2 plan 02-02 is complete.
- Phase 2 is complete.
- `make test` reaches EUnit and runs `log_tracer_test`, `sys_info_parser_test`, and `generated_monitor_smoke_test`.
- `tracer_test` remains manual because it is timing-dependent; the Makefile documents the manual command.
- Next task: begin Phase 3 generated monitor compile correctness.

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

**Phases:** 18-21

**Status:** Backlog

---
*Created: 2026-06-24 after roadmap creation and Phase 1 plan 01-01 verification.*
