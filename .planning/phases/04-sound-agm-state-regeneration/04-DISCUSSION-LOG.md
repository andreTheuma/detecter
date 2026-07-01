# Phase 4: Sound AGM State Regeneration - Discussion Log

**Date:** 2026-06-26
**Mode:** Co-op planning, manual GSD path

## Area: Transition Representation

**Question:** How should generated system information preserve ambiguity?

**Decision:** Replace the generated transition map keyed by `{Src, Dst}` with a list of transition rows. A map collapses duplicate state-pair transitions, which makes ambiguous event recovery undetectable.

**Locked shape:** `[{Src, Dst, EventSpec, ConditionFun}]`

## Area: Recovery Contract

**Question:** What should missing-event handling return when recovery is not deterministic?

**Superseded decision:** Return `{withhold, Reason}`, not `false` and not rejection. The original discussion reserved `{ok, Recovery}` for singleton state and singleton concrete event recovery.

**Revision (2026-07-01):** `{ok, Recovery}` requires singleton state inference, but may retain several literal or symbolic event descriptors. The monitor proceeds only after every descriptor has one complete monitoring consequence. Exact event recovery is optional metadata.

**Rationale:** Soundness depends on deterministic monitor behavior over every model-compatible event, not on identifying which monitor-equivalent event occurred.

## Area: Symbolic Events

**Question:** Can symbolic ranges such as `N` or `Z \ 0` count as recovered missing events?

**Superseded decision:** The original discussion allowed symbolic ranges only for state filtering and required withholding because they do not identify one literal event.

**Revision (2026-07-01):** A symbolic range may proceed when the generated monitor proves one consequence over the entire represented domain. The exact event remains unknown. Conflicting or unproven symbolic consequences withhold.

**Rationale:** Exact regeneration is a useful refinement, while deterministic monitoring consequence is the soundness gate.

## Area: Phase Boundary

**Decision:** Phase 4 fixes deterministic AGM recovery and withholding. Phase 5 handles complete-trace versus recovered-trace equivalence and irrevocability regression tests.

## GSD Runtime Note

The local GSD helper runtime failed with:

```text
Cannot find module '../../../package.json'
```

Artifacts were therefore written manually following the GSD discuss and plan-phase contracts.
