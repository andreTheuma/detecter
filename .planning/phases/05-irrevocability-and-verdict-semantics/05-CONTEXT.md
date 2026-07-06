# Phase 5: Irrevocability and Verdict Semantics - Context

**Gathered:** 2026-07-02
**Status:** Ready for planning

<domain>
## Phase Boundary

Phase 5 closes the thesis-critical implementation gap between local AGM recovery and end-to-end verdict behavior. It must preserve the consumed lookahead event, compare complete and deterministically recovered traces over the same logical sequence, prove that ambiguous recovery emits no verdict, and demonstrate that an emitted verdict cannot be retracted.

This is the final phase of `v0.1 Thesis Claim Stabilization`. It is limited to one explicitly marked missing event, the currently supported generated-monitor fragment, and the existing monitor message protocol. It does not add multi-event hypothesis tracking, a third verdict, native tracer-side loss detection, general lookahead event support, exhaustive maxHML verification, or a new buffering architecture.

</domain>

<decisions>
## Implementation Decisions

### Lookahead Replay
- **D-01:** The generated monitor owns preservation and replay of the lookahead event consumed during recovery.
- **D-02:** Replay uses direct ordered dispatch with the original trace envelope. It must not send the event back through the mailbox or reconstruct only its payload.
- **D-03:** Recovery first identifies the state after the missing event. The replayed event then follows the ordinary generated clause and advances system state exactly once.
- **D-04:** Replay applies ordinary monitor semantics. A matching event reduces the recovered continuation; an irrelevant event leaves the monitor in that continuation awaiting the next relevant event.

### Equivalence Boundary
- **D-05:** Complete and recovered traces are compared by observable monitoring behavior, not internal structural identity.
- **D-06:** Corresponding logical prefixes must not diverge through a premature verdict. The common suffix must then produce the same terminal verdict, or leave both executions in equivalent non-verdict continuations.
- **D-07:** No-verdict checks require deterministic synchronization or a test-only monitor-state checkpoint. Fixed sleeps are not acceptable evidence.
- **D-08:** Use a bounded representative matrix covering `yes`, `no`, and continued no-verdict behavior. Include exact recovery and multiple candidate events with one monitoring consequence.

### Withholding Lifecycle
- **D-09:** A production monitor that withholds terminates normally without sending `yes`, `no`, or a third verdict.
- **D-10:** White-box tests distinguish withholding from failure by checking normal termination, an empty verdict mailbox, and the internal `{withhold, Reason}` result where exposed.
- **D-11:** Recovery state mutation is atomic with consequence agreement. ETS is updated to the state after the missing event only after one complete monitoring consequence has been established.
- **D-12:** Preserve stable internal reason atoms: `ambiguous_state`, `impossible_recovery`, `ambiguous_consequence`, and `unproven_consequence`.
- **D-13:** Withholding does not continue consuming events to seek later disambiguation. That would introduce a new uncertainty-handling method beyond the thesis.

### Terminal Semantics
- **D-14:** A generated monitor emits exactly one terminal `yes` or `no` verdict and then terminates normally.
- **D-15:** Irrevocability tests cover extensions already queued when the decisive event is processed and extensions sent after the verdict.
- **D-16:** Later recovery of the missing event is represented using the existing concrete trace-event envelope, not a new correction protocol.
- **D-17:** Exercise both verdicts over complete and deterministically recovered paths, asserting one verdict, no contradictory or duplicate verdict, and normal termination.

### Academic and Milestone Boundary
- **D-18:** Phase 5 remains within the thesis because it validates soundness and irrevocability claims already made; it does not extend the research question.
- **D-19:** After implementation, update the thesis automatically and audit every active chapter against the final behavior.
- **D-20:** Stop after Phase 5 and completion of milestone `v0.1`. Phases 6 through 22 remain future milestones and must not be entered automatically.

### Codex's Discretion
- Exact names and placement of generated replay helpers.
- Test fixture organization and reusable synchronization helpers.
- The smallest generated AST refactor needed to delay ETS mutation until consequence agreement.

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### GSD Scope and Requirements
- `.planning/PROJECT.md` - Core value, architectural boundaries, and thesis-critical constraints.
- `.planning/REQUIREMENTS.md` - `VERD-01` through `VERD-03`.
- `.planning/ROADMAP.md` - Phase 5 success criteria and milestone boundary.
- `.planning/phases/04-sound-agm-state-regeneration/04-CONTEXT.md` - Locked singleton-state and deterministic-consequence contract.
- `.planning/phases/04.1-agm-engine-extraction-and-thesis-reconciliation/04.1-CONTEXT.md` - Pure engine and generated-code ownership boundaries.
- `.planning/phases/04.1-agm-engine-extraction-and-thesis-reconciliation/04.1-VERIFICATION.md` - Verified baseline and test totals before verdict work.

### Thesis Claims
- `../Master-Thesis/Documentation/Thesis/frontmatter/abstract.tex` - Current local-versus-end-to-end claim boundary.
- `../Master-Thesis/Documentation/Thesis/chap1/introduction_main.tex` - Soundness and irrevocability objectives.
- `../Master-Thesis/Documentation/Thesis/chap2/background_runtime_verification.tex` - Verdict, soundness, and irrevocability definitions.
- `../Master-Thesis/Documentation/Thesis/chap3/methodology.tex` - Deterministic monitoring consequence and lookahead-preservation obligation.
- `../Master-Thesis/Documentation/Thesis/chap3/implementation.tex` - Current generated monitor architecture and known replay limitation.
- `../Master-Thesis/Documentation/Thesis/chap4/results_and_discussion_main.tex` - Evaluation boundary and outstanding equivalence tests.

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets
- `detecter/src/regeneration/agm_engine.erl`: pure state recovery and consequence aggregation; it must remain free of ETS, receive, replay, and verdict effects.
- `detecter/src/synthesis/maxhml_agm_codegen.erl`: owns generated recovery cases, `handle_missing_event/1`, ETS adapters, and the effectful boundary where atomic commit and replay belong.
- `detecter/src/synthesis/maxhml_eval.erl`: supplies property-specific reduction functions and generated monitor clauses.
- `detecter/test/regeneration/generated_agm_recovery_test.erl`: already compiles temporary generated modules and exercises workers, verdict mailboxes, withholding reasons, and generated state functions.

### Established Patterns
- Generated modules are compiled with test-only `export_all` for white-box EUnit checks.
- Verdicts are messages (`yes` or `no`) sent to `From`; verdict functions then return.
- Consequence identity includes terminal verdicts or continuation function names plus complete bound argument lists.
- The default `make test` target is the required regression gate.

### Integration Points
- `generate_missing_event_recovery_case/1` currently invokes the selected continuation immediately and must be extended to preserve and dispatch the lookahead first.
- `generate_handle_missing_event_function/0` currently consumes a `send` trace and commits ETS before consequence agreement; Phase 5 must return sufficient recovery data and defer mutation.
- Existing `start_monitor_state/3`, process monitoring, and mailbox assertions can seed the new equivalence and irrevocability harness.
- `await_current_function/4` currently polls with `timer:sleep/1`; new claim-critical tests require deterministic synchronization instead.

</code_context>

<specifics>
## Specific Ideas

- Treat a complete trace and its recovered counterpart as two encodings of the same logical event sequence.
- Exercise extensions both before and after a verdict so termination cannot hide a queued contradictory output.
- Preserve withholding reasons for research diagnostics without promoting them into the verdict domain.

</specifics>

<deferred>
## Deferred Ideas

- General buffering proxy or event broker - future architecture work.
- Continued monitoring after ambiguous recovery to seek later disambiguation - new research beyond the thesis.
- A `{recovered_event, ...}` protocol - unnecessary extension to the current monitor interface.
- Absorbing terminal receive loops - observationally unnecessary for the thesis implementation.
- Native tracer-side missing-event detection, repeated missing events, non-`send` lookahead generalization, parser hardening, ETS isolation, and exhaustive formula verification - future milestones.

</deferred>

---

*Phase: 05-irrevocability-and-verdict-semantics*
*Context gathered: 2026-07-02*
