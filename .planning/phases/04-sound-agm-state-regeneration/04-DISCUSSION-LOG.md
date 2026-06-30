# Phase 4: Sound AGM State Regeneration - Discussion Log

**Date:** 2026-06-26
**Mode:** Co-op planning, manual GSD path

## Area: Transition Representation

**Question:** How should generated system information preserve ambiguity?

**Decision:** Replace the generated transition map keyed by `{Src, Dst}` with a list of transition rows. A map collapses duplicate state-pair transitions, which makes ambiguous event recovery undetectable.

**Locked shape:** `[{Src, Dst, EventSpec, ConditionFun}]`

## Area: Recovery Contract

**Question:** What should missing-event handling return when recovery is not deterministic?

**Decision:** Return `{withhold, Reason}`, not `false` and not rejection. `{ok, Recovery}` is reserved for singleton state and singleton concrete event recovery.

**Rationale:** Thesis text states that monitors preserve soundness by withholding judgement when information is insufficient.

## Area: Symbolic Events

**Question:** Can symbolic ranges such as `N` or `Z \ 0` count as recovered missing events?

**Decision:** No. Symbolic ranges may help filter possible states, but the missing event itself is only recovered when it is a single concrete literal.

**Rationale:** Thesis text states event regeneration requires complete certainty and that range-derived events do not produce verdicts.

## Area: Phase Boundary

**Decision:** Phase 4 fixes deterministic AGM recovery and withholding. Phase 5 handles complete-trace versus recovered-trace equivalence and irrevocability regression tests.

## GSD Runtime Note

The local GSD helper runtime failed with:

```text
Cannot find module '../../../package.json'
```

Artifacts were therefore written manually following the GSD discuss and plan-phase contracts.
