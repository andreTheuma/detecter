---
phase: 04-sound-agm-state-regeneration
status: passed
verified: 2026-07-01
score: 5/5
requirements:
  - AGM-01
  - AGM-02
  - AGM-03
  - AGM-04
  - AGM-05
---

# Phase 4 Verification

## Goal

Verify that generated missing-event recovery requires singleton system-state inference and one monitoring consequence across every model-compatible event, while treating exact event identity as optional metadata.

## Requirement Evidence

| Requirement | Result | Evidence |
|-------------|--------|----------|
| AGM-01 | Passed | `ambiguous_state_recovery` and `impossible_recovery` verify that empty or multi-state intersections return `withhold` and do not replace the scalar ETS state. |
| AGM-02 | Passed | `equal_concrete_consequences` and `conflicting_concrete_consequences` use duplicate `s1 -> s2` rows with literals 0 and 1; both descriptors remain available to consequence analysis. |
| AGM-03 | Passed | Unique, multi-literal, and symbolic tests proceed only with one complete consequence signature. Multi-literal and symbolic exact-event metadata remains `unknown`. |
| AGM-04 | Passed | Ambiguous state, impossible state, conflicting literal/symbolic consequences, and unsupported symbolic proof all return explicit withholding outcomes without a verdict. |
| AGM-05 | Passed | `uniform_symbolic_consequence` proceeds when the natural-integer domain has one consequence for the bound value; crossing and unsupported cases withhold. |

## Automated Verification

Focused command:

```text
make compile-test
erl -noshell -pa ebin -eval \
  'case eunit:test(generated_agm_recovery_test, [verbose]) of
       error -> init:stop(1);
       Result -> Result
   end.' -s init stop
```

Result: 8/8 AGM tests passed.

Full command:

```text
make test
```

Result:

- `log_tracer_test`: 24/24 passed
- `sys_info_parser_test`: 5/5 passed
- `generated_monitor_smoke_test`: 3/3 passed
- `generated_agm_recovery_test`: 8/8 passed

## Scope Boundary

Phase 4 verifies missing-event state inference and monitoring-consequence resolution for the generated scenarios. Complete-trace versus recovered-trace verdict equivalence, replay of the already-observed event, and verdict irrevocability remain Phase 5. Refactoring the AGM implementation out of `maxhml_eval.erl` remains inserted Phase 4.1 and must preserve this suite unchanged.

## Verdict

Passed. Phase 4's implementation goal and all five AGM requirements are supported by permanent generated-module regression evidence.
