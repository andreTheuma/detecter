# detectEr Full Audit — 2026-07-20 Follow-up (Code + Best Practices)

**Date:** 2026-07-20
**Auditor:** Claude (full codebase re-audit, fix verification, live reproductions, dialyzer, best-practices sweep)
**Baseline:** branch `feature/sound-agm-state-regeneration` at `8d310e3`, **plus** the uncommitted H2-revision working tree (`maxhml_eval.erl`, `maxhml_synthesis_test.erl`, three planning docs)
**Verification runs:** `make test` → all 6 suites green (exit 0); `make compile` → 0 warnings; focused dialyzer (OTP 29.0.2 PLT); 4 live reproductions (see Appendix)
**Prior audit:** `AUDIT-v0.1-manual-review.md` (2026-07-06). This document verifies its fixes, re-checks its open items, and reports new findings.

---

## 1. Executive Summary

**The Phase 5.2 fixes are real and hold.** C1 (fragment validation), C2 (compilation-scoped generation state), H1 (stacktrace API), M3 (`=:=` dedup), M5 (multi-property rejection), and M6 (warning-free production compile) were each verified in code *and* exercised through their regression tests. The in-flight H2 revision (init payload = model's first event) is coherent across code, tests, and planning docs, and is now proven by a true end-to-end `flu_spec/0` test — the biggest test-coverage hole from the last audit is closed.

**However, this audit found two new High-severity defects in the same "accepted input → broken artifact" family as C1**, both reproduced live:

- **N1 — a property *inside* the supported fragment whose conjunction branches use different binder names is accepted (`ok`) but generates a monitor that does not compile** (`update_current_state(Other, Tok)` — arity-2 call to an arity-1 function, `Tok` unbound). On the `erl` path the broken FLU source is written with **zero diagnostics** because the FLU module is never linted.
- **N2 — the default (beam) output path is broken for *every* AGM property, including well-formed ones.** `maxhml_agm_codegen:match/2` builds `=` as an infix *operator*; the reverted `{op,'=',...}` form pretty-prints fine (so the `erl`+`erlc` path works) but is rejected by `compile:forms` (`illegal pattern`, unbound-variable cascade). Result: a **zero-byte `.beam`** on disk, errors misattributed to `<name>_flu.hml:0`, and `compile/2` still returns `ok`. The tutorial's documented workflow (`[{outdir, "ebin"}]`, no `erl`) hits exactly this path.

Both are cheap to fix (N2 is essentially one line) and both are invisible to the current suite because every test passes the `erl` flag and every fixture uses identical binder names across branches. They compound with the still-open **H3** (write/compile results swallowed), which is sharpened by this audit: the FLU writer returns `file:close/1`'s result unconditionally, so no failure in generation, linting, or compilation can ever reach the caller.

Also new: **the dialyzer contract rot in `maxhml_eval` has crossed from "debt" to "tool-blinding"** (N3) — wrong `-spec`s (`string()` instead of `[atom()]`, `af_sym_act()` for raw patterns) plus two unexported `gen_eval` types cascade into absurd conclusions ("`initial_state_from_opts/1` will never be called"), which would drown any real signal the planned CI dialyzer gate could produce.

Severity totals this audit: **0 Critical, 2 High (new), 1 Medium (new), 4 Low (new), 1 Info**, plus verification of 7 fixes, re-confirmation of 10 open prior findings (one sharpened, one downgraded), and a best-practices sweep (CI, README, Makefile, tooling).

**Bottom line:** the soundness core (engine, recovery, withholding, irrevocability) remains in good shape and better-tested than at v0.1 close; the synthesis *output integrity* boundary (N1, N2, H3) is the weak edge and is worth a micro-phase before any live demo or artifact submission.

---

## 2. What Was Verified Green

| Check | Result |
|---|---|
| `make test` (6 suites: log_tracer, sys_info_parser, smoke, maxhml_synthesis, generated_agm_recovery, agm_engine) | All pass, exit 0, with the in-flight working tree applied |
| `make compile` (production, no `-DTEST`) | **0 warnings** (M6 holds) |
| `grep get_stacktrace src/` | No remnants (H1 holds) |
| C1 fix | `validate_supported_fragment/1` present ([maxhml_eval.erl:188-256](../detecter/src/synthesis/maxhml_eval.erl)), called from `compile/2`; rejection tests assert structured error **and** no output file |
| C2 fix | Process-dictionary namespace `{maxhml_fun_args, _}` + `reset_generation_state/0` at compile start ([maxhml_eval.erl:269-289](../detecter/src/synthesis/maxhml_eval.erl)); isolation test compiles two properties with different binders in one VM and `erlc`s both |
| M3 fix | `merge_monitoring_consequences/2` compares with `=:=`, with explanatory comment ([agm_engine.erl:201-219](../detecter/src/regeneration/agm_engine.erl)) |
| M5 (via C1) | `validate_supported_fragment([_, _ \| _]) → multiple_properties` + regression test |
| L5 fix | Streamer relocated to `test/manual/automated_event_streamer.erl` |
| H2 revision (in-flight) | Init clause advances state with the init payload per the supplied-model convention; START-row derivation kept (`initial_state_from_opts/1`); **end-to-end test** drives init → event → verdict → `DOWN` and asserts ETS `{current,previous}` at a checkpoint. Semantics, tests, and REQUIREMENTS/ROADMAP wording all agree |
| Smoke test (improved since last audit) | Now compiles all generated sources per fixture and asserts no `update_current_state()` zero-arity calls and no map-based transition helpers |
| Checkpoint clauses | Generated only under `-ifdef(TEST)` ([maxhml_agm_codegen.erl:351-387](../detecter/src/synthesis/maxhml_agm_codegen.erl)) — production surface unchanged |
| Grammar sources | All `.yrl`/`.xrl` present under `detecter/priv/`; regeneration helpers exist (`build:leex/0`, `build:yecc/0`) though not Makefile-wired |

---

## 3. New Findings

### N1 (High) — Accepted-fragment property with differing branch binders generates a non-compiling monitor, silently

**Reproduced.** The property below is squarely inside the supported fragment (two-branch conjunction, `nec` branches, verdict/recursion continuations) and differs from the canonical fixture only in naming the complement branch's binder `Other` instead of `Tok` — a completely natural way to write it:

```
[{_ <- _, token_server:loop(OwnTok, _)}]
max X.(
  [{_:_ ! Tok when OwnTok =:= Tok}]ff
  and
  [{_:_ ! Other when OwnTok =/= Other}]X
).
```

`maxhml_eval:compile/2` returns **`ok`**. The generated `mixed_flu.erl` contains (twice — ordinary receive and pending-dispatch replay):

```erlang
update_current_state(Other, Tok),   %% update_current_state/2 undefined; 'Tok' unbound
```

On the `erl` path this broken source is written **with zero diagnostics**; the user discovers it only if/when they `erlc` the monitor themselves.

**Root cause (three cooperating defects):**
1. `extract_bound_vars_from_guard/1` on the conjunction node returns the vars of **both** branch patterns ([maxhml_eval.erl:1273-1283](../detecter/src/synthesis/maxhml_eval.erl)), and the per-branch state updates are built from that merged set ([maxhml_eval.erl:400-463](../detecter/src/synthesis/maxhml_eval.erl)) — so each branch's `update_current_state` call references the *other* branch's variable. With identical binder names, `usort` collapses the set to one var and everything works — which is why all nine fixtures pass.
2. The conjunction/nec paths do not go through `generate_state_update_calls/1` (which correctly takes exactly one var or none) — this is exactly prior finding **M9**; N1 is its concrete, reproduced consequence. The zero-var variant (`[{_:_ ! _}]X` branch) produces `update_current_state()` — same class; the smoke test now guards that variant for the three fixtures but nothing guards the mixed-binder variant.
3. **H3**: the FLU writer never lints on the `erl` path (see §4), so the broken artifact is written silently.

**Fix:** build each branch's state update from *its own* pattern only (route through `generate_state_update_calls(PatPhiLeft/Right)`), and lint the FLU source before writing (H3 fix). Add a mixed-binder property to `generated_monitor_smoke_test` — its existing compile-and-assert-no-errors step will then guard this permanently.

**Note:** because nothing runs, no unsound verdict is emitted — this is fail-broken, not fail-unsound. But it violates the post-C1 contract that `ok` ⇒ usable monitor, for inputs the validator explicitly accepts.

---

### N2 (High) — Default beam output path is broken for every AGM monitor; zero-byte `.beam` written, `ok` returned

**Reproduced with a well-formed, fully-supported property** (the canonical send-loop shape). `maxhml_eval:compile("goodprop.hml", [{outdir, "."}, {mtab, ...}])` — no `erl` flag — yields:

```
goodprop_flu.hml:0: illegal pattern
goodprop_flu.hml:0: variable 'CurrentState' is unbound
goodprop_flu.hml:0: variable 'InferredState' is unbound
goodprop_flu.hml:0: variable 'LookaheadEnvelope' is unbound
... (every variable bound via the codegen's match helper)
```

and leaves **`goodprop_flu.beam` at 0 bytes** on disk. The mfa_spec monitor (`goodprop.beam`) compiles fine.

**Root cause:** `maxhml_agm_codegen:match/2` ([maxhml_agm_codegen.erl:1007-1012](../detecter/src/synthesis/maxhml_agm_codegen.erl)) constructs `Pattern = Expr` as `erl_syntax:infix_expr(P, operator('='), E)`. That reverts to `{op, _, '=', L, R}` — not a `{match, ...}` form. `erl_pp` happens to print it as `L = R`, and re-parsing the printed text repairs it into a real match — which is why the `erl`-then-`erlc` path (used by every test) works. `compile:forms` on the reverted AST does not get that second parse, so every `match/2`-bound variable is "unbound" and the aliased receive pattern in `handle_missing_event` is an "illegal pattern".

**Aggravators:**
- The zero-byte `.beam` exists because `write_lookup_monitor` opens the output file *before* compiling and closes it empty on error ([gen_eval.erl:698-728](../detecter/src/synthesis/gen_eval.erl)). A later `code:load` on it fails confusingly.
- Errors are attributed to `<name>_flu.hml:0` (the `{source, File}` passed to `compile:forms` is the source `.hml`, and generated forms carry line 0) — actively misleading.
- H3 swallows the failure: `compile/2` returns `ok`.
- **The tutorial documents this exact invocation**: `maxhml_eval:compile("props/prop_no_leak.hml", [{outdir, "ebin"}])` in `tutorial/docs/detecter-linear-time/synthesising-analysers.md` and `inline-instrumentation.md`.

**Fix:** one line — use `erl_syntax:match_expr(Pattern, Expr)` in `match/2`. Then add one beam-path compilation to the smoke test (currently all three call sites in the test suite pass `erl`). Fixing H3 makes the failure loud in the interim; fixing the open-file-before-compile order stops the corrupt-artifact side effect.

---

### N3 (Medium) — Dialyzer is blinded on `maxhml_eval` by contract rot; blocks the planned CI dialyzer gate

The focused dialyzer run produced a cascade of impossible conclusions, all traceable to wrong contracts rather than wrong code:

- `extract_vars/2`, `extract_vars_guard/3`, `extract_free_vars_from_guard/2`, `extract_bound_vars_from_guard/1` all declare `-> string()` but return **lists of atoms**. Dialyzer therefore types bound-vars as `[char()]`, concludes `erl_syntax:variable(char())` "will never return", and from there that `generate_state_update_calls/1` always returns `[]`/aborts.
- Consequence: dialyzer reports **"`initial_state_from_opts/1` will never be called"** and **"`invert_operator/1` will never be called"** — both are called from the live init-block clause; the whole clause is marked dead downstream of the bogus spec.
- `generate_state_update_calls/1` / `generate_state_update_args/1` declare `Pat :: gen_eval:af_sym_act()` but receive raw action patterns (`{init,...}`, `{send,...}`).
- `generate_verdict_function/2`'s spec doesn't overlap its actual `({tt|ff, 0}, 0)` usage.
- `gen_eval` references `af_guard/0` and `af_pattern/0` in `maxhml_eval` specs but **does not `-export_type` them** — dialyzer flags "Unknown types", weakening analysis of every signature that mentions them.
- `maxhml_agm_codegen` still has no `-spec`s at all (prior §8.2).

**Why it matters:** the roadmap's CI dialyzer gate (v0.3) is worthless while the flagship module produces ~10 false "never called/never match" warnings per run. **Fix:** mechanical — correct the five specs to `[atom()]`/pattern types, export the two types from `gen_eval`, delete or re-scope the dead `with()`/`spec()` types. Half an hour; do it in the same commit that wires `make analyze` into CI so the gate starts clean.

---

### N4 (Low) — Empty `.hml` file crashes synthesis instead of returning a structured error

**Reproduced:** `maxhml_eval:compile("empty.hml", ...)` → `function_clause` in `gen_eval:visit_entry_form/3`. The lexer's zero-token result becomes `{ok, skip}`; `maxhml_eval:compile/2` explicitly forwards this to `gen_eval:compile/5` ([maxhml_eval.erl:156-157](../detecter/src/synthesis/maxhml_eval.erl)), whose `{ok, Ast}` clause happily binds `Ast = skip` and passes the atom into `create_module` → `visit_entry_form`. The `skip` convention has no consumer.
**Fix:** in `maxhml_eval:compile/2`, turn `{ok, skip}` into `{error, empty_property_file}` (or handle `skip` in `gen_eval:compile/5`). Natural v0.2 (parser robustness) item; listed here because the path is currently a plain crash with a misleading site.

### N5 (Low) — Missing/invalid `mtab` option fails as a deep `badmatch`, and its default is nonsensical

`opts:monitor_table_opt/1` defaults to `"."` ([opts.erl:113-115](../detecter/src/monitoring/opts.erl)). Compiling an AGM property without `{mtab, File}` reaches `sys_info_parser:parse_file(".")` → `{badmatch, {error, eisdir}}` deep inside `generate_sys_info_function/1`. (`initial_state_from_opts/1` catches its own copy of this failure and silently falls back to `s0` first, so the crash arrives at the *second* parse, after generation has begun.) **Fix:** no default — validate the option up front and return `{error, {missing_option, mtab}}`. One clause.

### N6 (Low) — Init-payload selection for multi-variable init patterns silently picks the *rightmost* named variable

`generate_state_update_args/1` takes the head of `extract_vars(Pat, [])` filtered — and `extract_vars` builds its accumulator by prepending, so the head is the **last** variable in pattern order. For `token_server:loop(OwnTok, _)` that is `OwnTok` and all is well; for `loop(OwnTok, Count)` the "model's first event" silently becomes `Count`. With the in-flight H2 convention making the init payload semantically load-bearing, this choice should be deliberate: either restrict the fragment (validator: exactly one named MFArgs variable in the init pattern) or document the rule. Currently it is an accident of list construction.

### N7 (Trivial, in-flight diff) — Stale module docstring contradicts the new H2 convention

`maxhml_synthesis_test.erl` lines 9-11 still say the init block "no longer feeds spawn arguments into `update_current_state/1` (H2)" — the exact opposite of what the uncommitted diff implements and tests (lines 152-155 and the fixtures were updated; the header was not). Fix the three lines before committing, since the audit trail (`AUDIT-v0.1-manual-review.md` revision note) says "do not remove the init update again" and a future reader of this header could conclude the opposite.

### N8 (Info) — Synthesis TRACE chatter is unconditional in every build

Confirmed live during reproductions: `?TRACE` visitor logs print for every compile. `log.hrl` defaults `log_level` to 1 (TRACE) and nothing in the Makefile overrides it (`-Dlog_level=...` is supported but unused). Prior §8.5 stands; a `-Dlog_level=4` in the production erlc line is the one-line interim.

---

## 4. Prior Open Findings Re-checked

| ID | Status | Notes |
|---|---|---|
| **H3** | **Open — sharpened** | `write_monitors/3` dead clauses confirmed ([gen_eval.erl:684-691](../detecter/src/synthesis/gen_eval.erl)): both writers return `file:close/1`'s result, so the `{ok,_,_,_}`/`{error,_,_}` clauses never match and `compile/2` returns `ok` unconditionally. **New detail:** on the `erl` path the FLU module is written via bare `list_erl` — the `write_erl` (lint) call is commented out ([gen_eval.erl:719-722](../detecter/src/synthesis/gen_eval.erl)) — so FLU output gets *no diagnostics at all*; and both writers open the output file before compiling, so beam-path failures leave zero-byte `.beam` files (see N2). This is now the enabling defect for both new Highs. Promote from v0.2-backlog to the N1/N2 fix batch. |
| **H4** | Open — unchanged | `invert_operator/1` ([maxhml_eval.erl:1329-1337](../detecter/src/synthesis/maxhml_eval.erl)) still inverts only a leading `=:=` and returns any other guard unchanged, so the `pos`-init rejection clause duplicates the positive guard (unreachable clause; guard-failing init events leave the monitor stuck, no verdict). Note the fragment validator *accepts* `pos` init roots, so this sits inside the supported surface. The prior audit's fix (unguarded catch-all second clause, detectEr's own idiom) remains right and small. |
| **M1** | Open | `update_current_state` still two separate `ets:insert/2` calls ([maxhml_agm_codegen.erl:727-754](../detecter/src/synthesis/maxhml_agm_codegen.erl)); recovery commit correctly uses the single list insert. |
| **M2** | Open — now load-bearing | `reachable_state/3` still last-match-wins ([agm_engine.erl:49-60](../detecter/src/regeneration/agm_engine.erl)); no determinism validation at synthesis. With the H2 convention deliberately storing `[]` for out-of-alphabet init payloads, the "refuse/warn on `[]` store" guard from the prior audit is the single cheapest robustness win left in the engine area. |
| **M4** | Open | Improper list `[setminus \| {is_integer, N}]` still produced ([sys_info_parser.erl:88](../detecter/src/regeneration/sys_info_parser.erl)) and pattern-matched in codegen ([maxhml_agm_codegen.erl:620,698](../detecter/src/synthesis/maxhml_agm_codegen.erl)); descriptor/guard asymmetry confirmed (`N \ 1` or atom payloads crash `generate_sys_info_event_spec/1` with `function_clause`); `parse_operator` has no catch-all (any operator except `\` is a `case_clause` crash, making `parse_guard`'s `_ -> error` clause dead). Scheduled v0.2 — still correct placement. |
| **M7** | **Downgraded** | Mixed `pos` conjunctions can no longer reach generation through `maxhml_eval:compile/2` (validator rejects `pos` branches). The `function_clause` crash remains latent for direct `gen_eval:compile/5`/`modularise_hml` callers; fold the guard-tightening into Phase 22 normalization. |
| **M8** | Open | `LookaheadEnvelope` shadowing in every generated reduction fun ([maxhml_agm_codegen.erl:231-256](../detecter/src/synthesis/maxhml_agm_codegen.erl)); `event_spec_membership` adapter still names `(EventSpec, Value)` as `(Event, State)`. Cosmetic. |
| **M9** | Open — escalated into **N1** | The audit's warning ("this class has already produced one shipped bug") has now produced a second, reproduced one. The normalization (all state-update generation through one helper) should ride with the N1 fix, not wait for Phase 22. |
| **L1/L2/L3/L4/L6** | Open | `preceeding_*` adapters; `[]` state sentinel; regex-based architecture tests; commented-out blocks & TODO/DOUBLE-CHECK markers in `maxhml_eval` (lines 26-30, 107, 358, 766-767) plus 12 TODOs in `tracer.erl`, 7 in `log_tracer.erl`; test nits. As scheduled (v0.5 / Phases 19-22). |
| **L7** | Partially moot locally | A PLT existed on this machine and `make analyze`-style runs work; the missing `plt:` target and rebar3 recommendation stand for fresh machines and CI. |
| **L8** | Open | `agm_engine.erl`, `maxhml_agm_codegen.erl`, `sys_info_parser.erl` still carry no GPL header (upstream files do). Also: `maxhml_eval.erl` retains `@author Duncan Paul Attard` + placeholder `@doc Module description` despite heavy thesis-era modification — worth an authorship/attribution tidy for the thesis artifact. |
| **SPEC-04 / atom sites** | Open, scheduled | 3 `list_to_atom` sites in `sys_info_parser`, module names from file paths in `gen_eval`/`hml_eval`, generated-name atoms in `maxhml_eval`/codegen. Phase 9 as planned. |

Inherited runtime modules (`tracer`, `log_tracer`, `weaver`, `gen_file_poller`, `evm_tracer`, …): targeted hazard sweep only (catch-all swallows: none; `timer:sleep`: one, in the *intentional* `{delay, Ms, Event}` replay feature of `log_tracer`; ETS: two known public named tables in `log_tracer`/`tracer`, Phase 11 scope; no `os:cmd`/`open_port`). No new criticals surfaced; the deep audit of these remains correctly parked in Phases 10-13.

---

## 5. In-flight Working Tree — Review Verdict

The uncommitted diff (H2 revision) is **safe to commit** with one fix:

- ✅ Code change matches the documented convention (init payload = model's first event; START row = pre-init state; out-of-alphabet payload degrades to conservative withholding).
- ✅ `init_start_state_end_to_end_test` upgraded meaningfully: START row deliberately ≠ `s0` (`sA`), init payload is a real model event (`sA →9→ s1`), full init → recurse → verdict → `DOWN` cycle with checkpoint-anchored ETS assertions.
- ✅ REQUIREMENTS SND-03 and ROADMAP SC-3 rewordings match the code and cite the 2026-07-07 revision.
- ✅ Audit-doc revision note (+10 lines) records the reversal rationale and the "do not remove again" guard.
- ⚠️ **N7**: fix the stale module docstring (lines 9-11) before committing.

---

## 6. Best Practices / Modernization Sweep

1. **CI is effectively dead for this fork** (`.github/workflows/build.yml`): triggers only on `push` to `master` (feature branches never build), pins `erlang:22.1.5` (seven majors behind the OTP 29 the project targets — the H1 stacktrace fix *can't* even be validated on 22), compiles with one flat `erlc` glob (doesn't mirror the Makefile's behaviour-first two-pass; `$SRC/**/*.erl` only works by shell-glob accident), and runs only `log_tracer_test` of the six suites. The README badge advertises upstream `duncanatt/detecter`'s workflow, not this fork's. **Recommendation (≈1 hour, highest leverage):** new workflow on `push`/`pull_request` for all branches, `erlef/setup-beam` with OTP 27 + 29, `make test`, plus `dialyzer` with a cached PLT once N3 is fixed. This was §8.1 last time; N1/N2 are exactly the class of defect ("nobody exercises the other path") a matrix CI catches.
2. **README.md is the upstream stub** (badges → upstream repo, version 0.9, tutorial link to upstream pages; last touched Nov 2023). For a thesis artifact: state the fork's purpose (AGM), supervisor/attribution, OTP requirement, `cd detecter && make test`, the `.spec`+`.hml`+`mtab`+`erl` workflow, and the known-limitations pointer. Small but examiner/artifact-reviewer-facing.
3. **Makefile hygiene:** no `.PHONY` declarations (a file named `test`/`clean` would silently break targets); the six near-identical eunit lines could be one loop over a `SUITES` variable; `test-loop` uses `[[ ]]`/`((...))` bashisms that break on dash-`/bin/sh` platforms; add a `plt:` target (prior L7) and a `grammar:` target wiring the existing `build:leex/0`+`build:yecc/0` so `priv/*.yrl|xrl` regeneration is discoverable (currently nothing rebuilds the committed generated parsers, inviting drift).
4. **rebar3 migration** (prior L7, Phase 20): still recommended — `rebar3 eunit`, `dialyzer`, `fmt`, `lint`, and an actual OTP app structure (there is no `.app.src` today) for free. Keep the Makefile as a thin wrapper during transition.
5. **Logging:** compile-time-only level with TRACE default (N8). Either `-Dlog_level=4` for production/compile targets, or migrate `log.hrl` macros to OTP `logger` (v0.3-adjacent).
6. **No git tags exist.** Tag the v0.1-closing commit (`v0.1-thesis-claims`) — prior §8.6, still undone, and cheap insurance for thesis-listing traceability before the in-flight commit moves the branch again.
7. **Formatting/linting:** `erlfmt` + `elvis` once, then enforced in CI (prior §8.3). The mixed 2/4-space indentation and comment styles between inherited and new code persist.
8. **Property-based testing** (prior §8.4): unchanged recommendation — `proper`-generated LTS+trace+deletion with the two machine-checkable soundness invariants would upgrade Chapter 7's evidence from 8 matrix rows to thousands of cases; natural Phase 15 content.
9. **Docs/tutorial:** `tutorial/docs/detecter-linear-time/synthesising-analysers.md` documents the beam-path invocation that N2 breaks — after fixing N2, add a line steering AGM users to `erl` output + `erlc` as the verified path, or keep beam as default once the smoke test covers it.

---

## 7. Security Posture Delta

No new exposure classes. N1/N2 belong to the *availability/integrity-of-artifact* family (fail-broken, fail-silent), not fail-unsound: no path was found by which an accepted input yields a *wrong verdict* — the C1 validator plus conservative reduction gating (`unsupported_monitor_pattern`, `unmatched_literal`, `unsupported_symbolic_guard` all withhold) held up under adversarial reading. Atom-exhaustion (Phase 9) and shared-ETS (Phase 11) items unchanged. The zero-byte `.beam` artifact (N2) is worth fixing promptly because a stale-or-empty beam silently loaded in place of a monitor is an *absence-of-monitoring* failure mode, which for an RV tool is a soundness-adjacent operational risk.

---

## 8. Suggested Sequencing

| When | What | Items |
|---|---|---|
| **This commit** | Fix N7 docstring; commit the in-flight H2 revision | N7 |
| **Phase 5.3 (new, ~½ day, before any demo/artifact use)** | Synthesis output integrity: `match_expr` one-liner; per-branch state-update vars (M9 normalization); FLU lint + real return values from writers + no file-open-before-compile; mixed-binder + beam-path smoke fixtures | **N1, N2, H3, M9** |
| **With 5.3 or v0.3 CI plan** | Spec-rot burn-down + export types; then CI workflow (matrix OTP, full suite, dialyzer); tag baseline | **N3**, §6.1, §6.6 |
| **v0.2 (existing scope)** | Grammar/error model absorbs N4, N5, M4, M2 (+`[]`-store guard), N6 (restrict or document), H4 | Phases 6-9 |
| **v0.3–v0.5** | ETS isolation, logger, rebar3, proper suite, cleanup (M1, M8, L1/L2/L4/L8) | as roadmapped |

---

## Appendix A — Reproduction Commands (all verified 2026-07-20)

**N1 (mixed binder names → broken monitor, `ok` returned, no diagnostics):**
```bash
cat > mixed.hml <<'EOF'
with
  token_server:loop(_, _)
check
  [{_ <- _, token_server:loop(OwnTok, _)}]
  max X.(
    [{_:_ ! Tok when OwnTok =:= Tok}]ff
    and
    [{_:_ ! Other when OwnTok =/= Other}]X
  ).
EOF
printf '{s1, 0, s2};\n{s2, 9, s3};\n' > model.spec
erl -noshell -pa detecter/ebin -eval \
  'io:format("~p~n",[maxhml_eval:compile("mixed.hml",[{outdir,"."},{mtab,"model.spec"},erl])]),init:stop().'
# => ok            (and TRACE chatter; no error output)
erlc mixed_flu.erl
# => update_current_state/2 undefined; variable 'Tok' is unbound (twice)
```

**N2 (default beam path broken for well-formed properties):** same property with identical binders (`Tok` both branches), compile **without** `erl`:
```bash
erl -noshell -pa detecter/ebin -eval \
  'io:format("~p~n",[maxhml_eval:compile("goodprop.hml",[{outdir,"."},{mtab,"model.spec"}])]),init:stop().'
# => goodprop_flu.hml:0: illegal pattern / variable 'CurrentState' is unbound / ...
# => returns ok; ls -la goodprop_flu.beam → 0 bytes
```

**N4 (empty file):**
```bash
: > empty.hml
erl -noshell -pa detecter/ebin -eval \
  'io:format("~p~n",[catch maxhml_eval:compile("empty.hml",[{outdir,"."},{mtab,"model.spec"},erl])]),init:stop().'
# => {'EXIT',{function_clause,[{gen_eval,visit_entry_form,[maxhml_eval,skip,...]}|_]}}
```

**N3 (dialyzer cascade):**
```bash
cd detecter && make compile && \
dialyzer --plt ~/Library/Caches/erlang/.dialyzer_plt -pa ebin \
  ebin/agm_engine.beam ebin/maxhml_agm_codegen.beam ebin/maxhml_eval.beam \
  ebin/sys_info_parser.beam ebin/gen_eval.beam ebin/opts.beam
# => "initial_state_from_opts/1 will never be called", "invert_operator/1 will never be called",
#    [V|_] can never match [], Unknown types gen_eval:af_guard/0, af_pattern/0, ...
```
