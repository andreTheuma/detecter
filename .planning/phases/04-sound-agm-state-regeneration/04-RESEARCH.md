# Phase 4 Research: Sound AGM State Regeneration

**Date:** 2026-06-26
**Status:** Complete enough for planning

## Thesis Alignment

The thesis supports withholding rather than rejection when recovery is not deterministic:

- `chap3/methodology.tex` states that incomplete traces preserve soundness by withholding a verdict.
- `chap3/methodology.tex` states that event regeneration is certain only when there is one action between the inferred states.
- `chap3/implementation.tex` says `find_event/2` returns non-deterministic errors when multiple distinct events map to the same state transition.
- `chap3/implementation.tex` and `chap4/results_and_discussion_main.tex` state that symbolic ranges such as `n in N` are not concrete recovered events and do not produce verdicts.
- `chap4/results_and_discussion_main.tex` explicitly says the monitor would rather withhold than eagerly accept or reject.

## Code Findings

### Current Transition Table

`maxhml_eval:generate_sys_info_transition/1` emits:

```erlang
#{{Src, Dst} => ConditionFun}
```

This loses multiplicity for duplicate `{Src, Dst}` rows. It also stores only the condition function, not a descriptor for the event. That makes it impossible to distinguish a concrete singleton event from a symbolic predicate once generated.

### Current Recovery Defect

Generated `handle_missing_event/1` computes:

```erlang
S_X0 = sets:to_list(...),
case validate_state_transition(LastKnownState, S_X0) of
    true -> ...
    false -> false
end
```

`S_X0` is a candidate list, while `validate_state_transition/2` expects one destination state. The generated branch then maps `false` to rejection in some monitor functions. This is the critical soundness bug for Phase 4.

### Test Surface

Generated AGM helper functions are not exported by default. Tests can compile generated modules with the Erlang `export_all` compiler option to call `init_transitions/0`, `reachable_state/2`, `handle_missing_event/1`, and new recovery helpers without changing generated production exports.

## Implementation Direction

1. Preserve transition rows as a list with event descriptors.
2. Update generated helper folds to consume transition lists.
3. Add singleton-selection helpers and concrete-event deduction.
4. Return `{ok, Recovery}` or `{withhold, Reason}` from `handle_missing_event/1`.
5. Change generated missing-event case handling so withholding never calls `rejection/1`.
6. Add generated-module tests for unique, ambiguous, impossible, and symbolic recovery.
