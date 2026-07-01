# Phase 4: Sound AGM State Regeneration - Context

**Gathered:** 2026-06-26
**Status:** Implemented and verified
**Source:** GSD discuss-phase manual path. The GSD helper runtime failed with `Cannot find module '../../../package.json'`, so context was captured directly from repository and thesis material.

<domain>
## Phase Boundary

Phase 4 makes automaton-guided missing-event recovery sound by construction. The generated monitor must preserve enough system-transition information to infer a singleton post-missing state and compare the complete monitoring consequence of every compatible missing event. Exact event identity is optional when all candidates produce the same consequence.

This phase does not prove complete-trace versus recovered-trace verdict equivalence. That belongs to Phase 5. This phase also does not redesign the `.spec` grammar, normalize generator variable names, or fix ETS/session isolation; those are tracked in later phases.
</domain>

<decisions>
## Implementation Decisions

### Transition Representation
- Generated `init_transitions/0` must stop using a map keyed only by `{Src, Dst}` for AGM recovery work.
- The generated transition table should preserve multiplicity using a list shape equivalent to:

```erlang
[{Src, Dst, EventSpec, ConditionFun}]
```

- `EventSpec` must retain enough information to distinguish concrete literal events from symbolic/ranged events. Examples:
  - `{literal, 0}`
  - `{literal, -1}`
  - `{symbolic, natural_integer}`
  - `{symbolic, any_integer_except, 0}`
  - `{literal, null}` for the START/NULL row if retained in the generated table
- `ConditionFun` remains the runtime predicate used by reachable/predecessor helpers.

### Recovery Contract
- `handle_missing_event/1` must return `{ok, Recovery}` only when state recovery is deterministic.
- Deterministic recovery means:
  1. The inferred state immediately after the missing event is a singleton.
  2. Every event descriptor between the last known and inferred states is preserved.
  3. Reducing every represented event through the current monitor produces one complete consequence signature.
- Recovery failure must return `{withhold, Reason}` rather than `false` or rejection.
- Expected reasons include `ambiguous_state`, `impossible_recovery`, `ambiguous_consequence`, and `unproven_consequence`.

### Withholding Semantics
- Withholding is not a verdict. It must not call `acceptance/1` or `rejection/1`.
- Generated missing-event branches should treat `{withhold, Reason}` as "no sound verdict can be issued from this restricted trace".
- This aligns with the thesis claim that monitors preserve soundness by withholding rather than eagerly accepting or rejecting when inference is not deterministic.

### Symbolic Event Handling
- Symbolic or ranged descriptors, such as `N` or `Z \ 0`, do not identify one concrete missing event.
- They may nevertheless support monitoring when the generated reducer proves one consequence over the complete represented domain.
- Unsupported domains, guards, or unresolved event-dependent continuations must withhold conservatively.

### Phase 5 Boundary
- Phase 4 may update generated monitor state to the recovered state after the missing event, preserving the existing "retrace last event" style if needed.
- Phase 5 will test and tighten verdict equivalence, event replay, and irrevocability semantics.
</decisions>

<canonical_refs>
## Canonical References

Downstream planning and implementation must read these before changing code.

### Thesis Claims
- `/Users/andretheuma/university/Master-Thesis/Documentation/Thesis/chap1/introduction_main.tex` — soundness and irrevocability aims; deterministic inferred states.
- `/Users/andretheuma/university/Master-Thesis/Documentation/Thesis/chap3/methodology.tex` — methodology for singleton state inference and deterministic monitoring consequences.
- `/Users/andretheuma/university/Master-Thesis/Documentation/Thesis/chap3/implementation.tex` — implementation narrative for transition descriptors, state regeneration, and consequence aggregation.
- `/Users/andretheuma/university/Master-Thesis/Documentation/Thesis/chap4/results_and_discussion_main.tex` — bounded evidence for concrete and symbolic continuation/withholding behavior.

### Code
- `detecter/src/synthesis/maxhml_eval.erl` — generated transition table, AGM helpers, missing-event branches, state updates.
- `detecter/src/regeneration/sys_info_parser.erl` — parser output consumed by transition generation.
- `detecter/test/regeneration/sys_info_parser_test.erl` — current parser contract coverage.
- `detecter/test/regeneration/generated_monitor_smoke_test.erl` — generated monitor compile matrix.
- `detecter/priv/sys_info.spec` — canonical token-system state transition table used by generated monitor tests.
</canonical_refs>

<code_context>
## Reusable Assets and Patterns

- `generated_monitor_smoke_test` already compiles generated monitors into isolated temporary directories and compiles all emitted Erlang files.
- Generated modules can be compiled with Erlang compiler options such as `export_all` in tests when unexported generated helper functions must be called.
- Current generated AGM helper names are misspelled as `preceeding_*`; preserve spelling during Phase 4 unless a separate rename is deliberately planned.
- Current helper functions fold over `maps:to_list(init_transitions())`; switching `init_transitions/0` to a list requires updating every generated helper fold together.

## Defect Addressed

Current generated code computes `S_X0` as a list of candidates and passes that list into `validate_state_transition/2` as though it were a single state:

```erlang
S_X0 = sets:to_list(...),
case validate_state_transition(LastKnownState, S_X0) of
    true -> ...
    false -> false
end
```

This lost the distinction between "no recovery", "ambiguous recovery", and "unique recovery", and later generated branches could treat `false` as rejection. Plans 04-02 and 04-03 replaced this path with singleton state resolution, monitoring-consequence aggregation, and explicit withholding.
</code_context>

<deferred>
## Deferred Ideas

- Full `.spec` grammar redesign and structured parser errors remain v0.2 work.
- Generated variable-name normalization remains Phase 22.
- Generated monitor session/ETS isolation remains runtime hardening work.
- Complete replay of the next observed event and verdict equivalence belongs to Phase 5.
</deferred>
