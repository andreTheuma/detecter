# Requirements: detectEr Thesis Stabilization

**Defined:** 2026-06-24
**Core Value:** Generated monitors must only emit verdicts that are sound and irrevocable for the traced system model, especially when traces contain missing events.

## v0.1 Requirements

### Test Harness

- [x] **TEST-01**: `make test` excludes generated `detecter/test/**/ebin/**` Erlang artifacts from hand-written test-source compilation.
- [x] **TEST-02**: Regeneration parser tests run through the default test target.
- [x] **TEST-03**: A generated monitor compile smoke test runs for a representative recursive regeneration property.
- [x] **TEST-04**: Timing-sensitive tracer tests are documented as manual and excluded from the default target until stabilized.

### Parser Contract

- [x] **PARS-01**: `sys_info_parser_test` matches the symbolic parser contract consumed by synthesis.
- [x] **PARS-02**: START/NULL handling is consistent with `detecter/priv/sys_info.spec` and thesis examples.
- [x] **PARS-03**: Parser tests clean up temporary files reliably.

### Generated Monitor Correctness

- [x] **GEN-01**: Modular synthesis no longer emits invalid `update_current_state/0` calls for the reproduced init-path failure.
- [x] **GEN-02**: Representative regeneration properties compile from generated source.
- [x] **GEN-03**: Regression tests catch future generated state-update arity mistakes.

### AGM Semantics

- [x] **AGM-01**: Missing-event handling never treats a list of candidate states as a single state.
- [x] **AGM-02**: Generated system-information data preserves duplicate state-pair transitions so every compatible event consequence can be considered.
- [x] **AGM-03**: Missing-event recovery proceeds only after singleton state inference and a singleton monitoring consequence; exact event recovery is optional.
- [x] **AGM-04**: Impossible or ambiguous state inference, conflicting consequences, and unproven symbolic consequences return `withhold` rather than acceptance or rejection.
- [x] **AGM-05**: A symbolic event descriptor may proceed only when the generated monitor proves one consequence over its complete represented domain.

### AGM Architecture and Academic Alignment

- [x] **ENG-01**: Reusable AGM runtime inference and consequence aggregation are extracted from `maxhml_eval.erl` behind an explicit interface.
- [x] **ENG-02**: Property-specific AGM code generation is separated from maxHML semantic traversal.
- [x] **ENG-03**: The extracted AGM engine has focused tests independent of generated monitor orchestration.
- [x] **ACAD-01**: Every supplied thesis chapter is reviewed after extraction and recorded as amended or verified unchanged with code/test evidence.
- [x] **ACAD-02**: Thesis implementation listings and architectural claims describe the post-refactor module boundaries and runtime dependency accurately.

### Verdict Semantics

- [ ] **VERD-01**: Deterministically recovered traces produce the same verdict as corresponding complete traces.
- [ ] **VERD-02**: Ambiguous missing traces do not produce eager verdicts.
- [ ] **VERD-03**: Once emitted, a verdict is not retracted by later trace extension or event recovery.

## v0.2 Requirements

### System Information Robustness

- **SPEC-01**: The accepted `.spec` grammar is documented.
- **SPEC-02**: Malformed `.spec` input returns structured parser errors instead of pattern-match crashes.
- **SPEC-03**: Union, disjunction, strings, and multiple guards are either implemented or explicitly rejected with tests.
- **SPEC-04**: Parser input cannot create unbounded new atoms from untrusted text.

## v0.3 Requirements

### Runtime Hardening

- **RUN-01**: Unknown process deletion events do not crash tracer state cleanup.
- **RUN-02**: Generated monitor state is isolated enough to avoid collisions across independent runs.
- **RUN-03**: Deprecated `erlang:get_stacktrace/0` usage is replaced.
- **RUN-04**: Tracer routing tests are stable enough for their chosen execution tier.

## v0.4 Requirements

### Research Evidence

- **DOC-01**: Thesis claims map to implementation modules and regression tests.
- **DOC-02**: AGM examples cover complete, uniquely recoverable, ambiguous, and invalid traces.
- **DOC-03**: Thesis wording is reconciled with final implementation behavior and limitations.
- **DOC-04**: Developer notes explain modular synthesis and AGM recovery workflows.

## v0.5 Requirements

### Deferred Cleanup

- **CLN-01**: Stubbed dynamic attach/detach behavior is either implemented or removed from implied API surface.
- **CLN-02**: Duplicated AST helper construction is consolidated or documented.
- **CLN-03**: Build reproducibility expectations are documented or improved.
- **CLN-04**: Generated regeneration fixture layout is separated from hand-written test source.
- **CLN-05**: Synthesis variable names use one documented internal representation instead of mixed atoms and strings.

## Out of Scope

| Feature | Reason |
|---------|--------|
| Probabilistic missing-event verdicts | The thesis requires deterministic soundness. |
| General uncertainty beyond missing events | The research scope is data-restricted traces with missing events. |
| Full runtime API redesign before v0.1 | Thesis-critical synthesis and AGM behavior comes first. |
| Complete `.spec` language redesign before v0.1 | Parser robustness is tracked in v0.2 after claim-critical semantics. |

## Traceability

| Requirement | Phase | Status |
|-------------|-------|--------|
| TEST-01 | Phase 1 | Complete |
| TEST-02 | Phase 1 | Complete |
| TEST-03 | Phase 1 | Complete |
| TEST-04 | Phase 1 | Complete |
| PARS-01 | Phase 2 | Complete |
| PARS-02 | Phase 2 | Complete |
| PARS-03 | Phase 2 | Complete |
| GEN-01 | Phase 3 | Complete |
| GEN-02 | Phase 3 | Complete |
| GEN-03 | Phase 3 | Complete |
| AGM-01 | Phase 4 | Complete |
| AGM-02 | Phase 4 | Complete |
| AGM-03 | Phase 4 | Complete |
| AGM-04 | Phase 4 | Complete |
| AGM-05 | Phase 4 | Complete |
| ENG-01..ENG-03 | Phase 4.1 | Complete |
| ACAD-01..ACAD-02 | Phase 4.1 | Complete |
| VERD-01 | Phase 5 | Pending |
| VERD-02 | Phase 5 | Pending |
| VERD-03 | Phase 5 | Pending |
| SPEC-01..SPEC-04 | Phases 6-9 | Planned |
| RUN-01..RUN-04 | Phases 10-13 | Planned |
| DOC-01..DOC-04 | Phases 14-17 | Planned |
| CLN-01..CLN-05 | Phases 18-22 | Backlog |

**Coverage:**
- v0.1 requirements: 23 total
- Mapped to phases: 23
- Unmapped: 0

---
*Requirements defined: 2026-06-24*
*Last updated: 2026-07-01 after Phase 4.1 verification.*
