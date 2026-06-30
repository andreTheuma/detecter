---
phase: 4
plan: 04-01
status: complete
completed: 2026-06-29
requirements:
  - AGM-02
---

# Plan 04-01 Summary: Preserve Transition Multiplicity

## Delivered

- Generated `init_transitions/0` now returns ordered `{Src, Dst, EventSpec, Condition}` rows instead of a map keyed by `{Src, Dst}`.
- Literal and symbolic event descriptors are retained separately from executable event predicates.
- `get_system_states/0`, `reachable_state/2`, `reachable_states_from_state/1`, `preceeding_states_from_state/1`, and `validate_state_transition/2` now consume transition lists directly.
- `validate_state_transition/2` uses `lists:any/2`, preserving its boolean contract across duplicate source/destination rows.
- The obsolete `parse_sys_info_event/1` name and commented map-based deduction prototype were removed.
- The generated `NULL` predicate now evaluates `Event =:= null` and therefore always returns a boolean.
- Generated-monitor smoke coverage asserts that emitted source contains neither `maps:to_list(StateTransitionTable)` nor `maps:is_key`.

## Verification

- `make compile-test` passed.
- `make test` passed:
  - `log_tracer_test`: 24 tests
  - `sys_info_parser_test`: 5 tests
  - `generated_monitor_smoke_test`: 3 tests
- A generated `prop_no_failure` monitor compiled successfully and showed list-based four-field transition rows.
- `git diff --check` passed.

## Next

Plan 04-02 will use the retained event descriptors to enforce singleton concrete-event recovery and explicit withholding.
