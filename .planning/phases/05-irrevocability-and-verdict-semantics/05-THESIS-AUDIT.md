# Phase 5 Thesis Audit

**Audit date:** 2026-07-02
**Code baseline:** uncommitted Phase 5 working tree, pending manual review
**Scope:** abstract plus the eight active chapter sources audited in Phase 4.1

| Thesis source | Disposition | Final code and test evidence |
|---------------|-------------|------------------------------|
| `frontmatter/abstract.tex` | amended | Replaced the obsolete lost-lookahead and unverified-verdict boundary with the ordered original-envelope replay implemented by `generate_missing_event_recovery_case/1` and `generate_receive_state_replay/3`, the complete/recovered equivalence cases, and the eight-row terminal irrevocability matrix. The abstract retains the one-marker, send-lookahead, bounded-fragment, named-ETS and no-tracer-detection limits. |
| `chap1/introduction_main.tex` | amended | Added the evaluated implementation boundary for post-agreement commit, direct replay, complete/recovered comparison and queued/post-verdict extension evidence. The research aims remain soundness and irrevocability rather than a new question or an excluded Phase 6+ capability. |
| `chap2/background_detecter.tex` | verified unchanged | Its detectEr synthesiser/monitor/tracer account and the internal `maxhml_eval` / `maxhml_agm_codegen` / `agm_engine` split remain accurate. Its binary terminal-verdict description agrees with generated `acceptance/1` and `rejection/1`; Phase 5 changes evidence, not this background architecture. |
| `chap2/background_runtime_verification.tex` | verified unchanged | The definitions of soundness, impartial withholding, irrevocability, acceptance and rejection remain the semantic criteria exercised by the four withholding lifecycles and exact terminal-verdict collector. No implementation-specific stale replay claim occurs in this chapter. |
| `chap2/background_system_modelling.tex` | verified unchanged | The deterministic-LTS assumption still matches `agm_engine:reachable_state/3` and the supplied-model precondition. Phase 5 neither validates model determinism nor changes the LTS semantics. |
| `chap3/methodology.tex` | verified unchanged | Singleton post-missing state inference, one complete monitoring consequence, optional exact event recovery, preserved observed order and conservative withholding remain the obligations implemented by `recover_missing_event/3`, `resolve_monitoring_consequence/2`, post-agreement commit and direct replay. |
| `chap3/implementation.tex` | amended | Updated the generated recovery listings and prose to show read-only envelope capture, one list-valued ETS commit after agreement, unary continuation callbacks, replay/pending dispatch and recursive/maximal-fixpoint bridges. Test evidence now records 15 engine checks, 25 generated checks and the eight terminal rows. |
| `chap4/results_and_discussion_main.tex` | amended | Replaced outstanding replay/equivalence/irrevocability work with measured evidence: ordered continuing replay, complete/recovered `yes` and `no`, four withholding lifecycles, tagged pre-start queueing, post-verdict concrete detail and 72 default checks. Future work and conclusions retain the model, event-type, generated-fragment, ETS, repeated-recovery and tracer limits. |
| `appA/appendix_a_main.tex` | verified unchanged | Appendix A documents baseline detectEr monitor synthesis rather than the experimental AGM runtime. It contains no claim about lookahead replay, deterministic recovery, withholding or Phase 5 verdict evidence that requires amendment. |

## Claim Boundary

Phase 5 validates existing soundness and irrevocability claims for the tested generated fragment. It does not establish an exhaustive maxHML theorem or implement repeated missing-event recovery, non-`send` lookahead, tracer-side loss detection, per-monitor ETS isolation, model validation, or any Phase 6+ feature.

## Evidence Distinctions

- **Ordered replay:** `handle_missing_event/1` returns the untouched trace envelope; generated replay and pending dispatchers preserve it until ordinary matching semantics consume it.
- **Atomic agreement boundary:** `commit_recovery_state/1` performs one list-valued `ets:insert/2` only after `resolve_monitoring_consequence/2` succeeds.
- **Equivalence:** complete and recovered encodings of the same bounded logical sequences have equal exact verdicts or equal reference-tagged continuing observations.
- **Irrevocability:** complete/recovered × `yes`/`no` × queued/post-verdict rows each observe one exact terminal atom, the expected worker result and normal termination.
- **Withholding:** ambiguous or unproven recovery returns an internal reason, emits no external verdict and leaves both recovery-state entries unchanged.
