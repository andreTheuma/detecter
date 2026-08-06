# detectEr Full Audit — v0.1 Manual Review Gate

**Date:** 2026-07-06
**Auditor:** Claude (full codebase + thesis cross-check + literature review)
**Baseline:** branch `feature/sound-agm-state-regeneration`, unstaged Phase 5 working tree
**Verification run:** `make test` → 72/72 green (24 tracing, 5 parser, 3 smoke, 25 generated AGM, 15 engine)
**Scope:** AGM feature (engine, codegen, synthesis), tests, parser, build, thesis alignment, literature positioning, security posture

This document serves the pending GSD todo *"Manually review the complete unstaged Phase 5 code, test, thesis, and planning changes before committing."*

---

## 1. Executive Summary

**Phase 5 itself is in good shape and safe to commit.** The lookahead-replay, atomic-commit, withholding, and irrevocability work is correct against its locked decisions (D-01…D-20), the 40 new/updated AGM tests are deterministic and causally synchronized, and the thesis chapters were reconciled honestly with measured evidence.

**However, the audit found two confirmed critical defects outside the Phase 5 diff** that undermine the thesis's core value statement ("generated monitors must only emit verdicts that are sound"):

- **C1 — the synthesizer silently emits *unsound* monitors** for property shapes it does not support (chained necessities ending in a verdict). Verified by generating a monitor that rejects **without ever observing the decisive event**.
- **C2 — compiling two properties in one Erlang VM corrupts the second monitor** via stale `persistent_term` memoization. Verified: the second generated module references the first property's variable names and fails to compile.

Neither is a Phase 5 regression — both predate it — but both must be scheduled explicitly (recommendation: a small Phase 5.2 "fragment validation and compiler-state isolation" before v0.1 is declared closed, since both directly threaten the soundness claim that v0.1 exists to stabilize).

Severity totals: **2 Critical, 4 High, 9 Medium, 8 Low**, plus 6 thesis/documentation items and 3 planning-hygiene items.

---

## 2. What Was Verified Green

| Check | Result |
|---|---|
| `make test` (default target) | 72/72 pass, matches STATE.md and thesis listing `lst:agm_regression_results` |
| Test counts vs thesis claims | 15 `agm_engine_test`, 25 `generated_agm_recovery_test` — exact match |
| `agm_engine` purity (ENG-01) | Confirmed: no ETS/receive/send/verdict effects in the module |
| Read-only recovery boundary (D-11) | Confirmed in generated source: `handle_missing_event/1` performs no ETS write; single list-valued `ets:insert/2` in `commit_recovery_state/1` after consequence agreement |
| Envelope preservation + replay (D-01…D-04) | Confirmed: pending-dispatcher design correctly reproduces selective-receive semantics — the retained envelope is re-tried *first* at each subsequent state, exactly like an older mailbox message |
| Withholding lifecycle (VERD-02, D-09/D-12) | All four reasons (`ambiguous_state`, `impossible_recovery`, `ambiguous_consequence`, `unproven_consequence`) terminal, non-verdicting, ETS-snapshot-stable |
| Irrevocability matrix (VERD-03) | 8 rows (complete/recovered × yes/no × queued/post-verdict), exact verdict multiset asserted after normal `DOWN` |
| Test determinism (D-07) | No `timer:sleep` in claim-critical tests; reference-tagged checkpoints + same-sender ordering + `spawn_monitor` |
| Symbolic uniformity proofs | Conservative and sound: unmatched literals → `unproven_consequence`; guard-boundary crossings → `ambiguous_consequence`; unsupported guards → withhold |
| Thesis test/architecture claims | Chapter 3/4 listings match generated code near-verbatim; limitations (hard-coded `s0`, public ETS, send-only lookahead, one missing event, no model validation) are stated |

---

## 3. Critical Findings

### C1 — Synthesizer emits unsound monitors for unsupported-but-accepted property shapes

**Evidence (reproduced during audit).** Synthesizing

```
with token_server:loop(_, _)
check
  [{_ <- _, token_server:loop(OwnTok, _)}]
  [{_:_ ! Tok when OwnTok =:= Tok}]ff.
```

produces in `chained_flu.erl`:

```erlang
flu_spec() ->
    ...
    receive
        {{trace, _, spawned, _, {token_server, loop, [OwnTok, _]}}, From} ->
            update_current_state(OwnTok),
            send5(OwnTok, From)
    end.
send5(OwnTok, From) ->
    rejection(From).          %% <-- emits `no` WITHOUT observing the send event
```

The monitor rejects immediately after the init event. The guard `OwnTok =:= Tok` and the send event itself are never checked. **This is an unsound verdict**, violating the core v0.1 value and VERD-01/AGM-04 in spirit.

**Root cause.** In `maxhml_eval:generate_function/2` ([maxhml_eval.erl:510-521](detecter/src/synthesis/maxhml_eval.erl)), when a necessity's continuation `Phi` is terminating (`tt`/`ff`), the generated function is a *bare verdict wrapper* with **no `receive`** — `lists:nth(2, clause_body(Clause))` extracts only the continuation call. This wrapper convention is correct **only** when the parent state (the two-branch conjunction receive) has already consumed the decisive event. For a standalone/chained necessity there is no such parent, so the verdict fires one event early. The same architectural assumption makes deeper non-recursive conjunction branches double-consume events.

**Why tests never caught it.** All test properties place terminal verdicts under the supported conjunction shape; the smoke tests only check *compilation*, not verdict behavior, of other shapes.

**Recommended fix (detailed).** Do **not** try to make chained necessities work in v0.1 (that is new scope). Instead, make the generator *reject* what it does not support — fail closed, which is the standard expected of a soundness-first tool:

1. Add a `validate_supported_fragment/1` pass in `maxhml_eval` (called from `modularise_hml/2` or `gen_eval:create_module/5` before generation) that walks the AST and accepts exactly the fragment the thesis claims:
   - root: optional init modality, then `max X.` or the two-branch conjunction or a terminal verdict;
   - conjunction branches: `nec` with continuation ∈ {`tt`, `ff`, recursion variable};
   - nothing else (`pos` outside init, `or`, chained `nec`, nested conjunctions → reject).
2. On violation, return `{error, {unsupported_property_fragment, Node}}` through `gen_eval:compile/5` (which already has an error path) so `maxhml_eval:compile/2` callers get a structured failure instead of a corrupt monitor.
3. Add regression tests: each rejected shape asserts a synthesis error and asserts **no** `.erl` output file is produced.
4. Thesis: one sentence in Chapter 6 limitations — "property shapes outside this fragment are rejected at synthesis time" — which turns an unsoundness hole into a documented, enforced boundary (strengthens ACAD-02/DOC-03).

**GSD placement:** new Phase 5.2 (recommended before closing v0.1) or first plan of v0.2. Effort: small (a recursive-descent validator over `af_maxhml()` + tests).

---

### C2 — `persistent_term` memoization corrupts every subsequent compilation in the same VM

**Evidence (reproduced during audit).** Compiling `spec_a.hml` (binds `OwnTok`) then `spec_b.hml` (binds `BaseTok`, same shape/line layout) in one VM yields a `spec_b_flu.erl` that mixes both variable sets and **does not compile**:

```erlang
x(BaseTok, OwnTok, From) ->            %% phantom 3-arity merging both specs
    send6send8(OwnTok, From).
...
%% erlc: variable 'OwnTok' is unbound / variable 'BaseTok' is unbound
```

**Root cause.** `maxhml_eval` memoizes generated-function argument lists in `persistent_term` keyed by *generated function name atoms* (`x`, `send6send8`, …) — see [maxhml_eval.erl:211-227, 276-303, 437-454, 653](detecter/src/synthesis/maxhml_eval.erl). The keys:
- are never erased, so they leak across compilations for the life of the VM;
- collide across different properties (recursion variables like `X` have **no line-number component**; action/line names collide whenever two specs share a layout — which is the common case for small specs);
- `persistent_term` is the wrong tool by design: the OTP docs reserve it for effectively-permanent global data because updates/deletions trigger global GC scans.

**Aggravating subtlety (document even after fixing).** The memo is not merely a cache — it *defines semantics*: the conjunction path stores the **continuation's** args under the **branch function's** name so that `generate_function(InnerBranch)` later adopts them, which is what makes composite-state wrapper arities line up. This implicit cross-function coupling is order-dependent and invisible to a reader; it belongs in an explicit data structure.

**Why tests never caught it.** All nine fixture properties in `generated_agm_recovery_test` use identical variable names and line layouts, so the stale entries happen to coincide.

**Recommended fix (detailed).** Replace the global memo with compilation-scoped state:

1. Introduce an explicit environment: `-record(gen_env, {fun_args = #{} :: #{atom() => [var()]}})`, threaded through `generate_function/generate_init_block/generate_function_args` (mechanical: those functions already pass `_Opts`; extend to `{Opts, Env}` or use a dedicated ETS/process-dictionary namespace like the existing `?KEY_PH_*`).
   - Lower-effort interim option: keep the lookup pattern but move it to the process dictionary and add `erase_generation_state/0` invoked at the top of `maxhml_eval:compile/2` (mirrors the existing `init_ph/0` convention). This alone fixes the corruption; the record refactor can follow in Phase 19/22 cleanup.
2. While there, split "arity contract for composite branches" into an explicitly computed table (first pass computes `#{function_name => args}` for the whole AST; second pass generates) — this removes the order dependence and makes the composite/wrapper contract reviewable and testable in isolation.
3. Regression test: compile two properties with *different* binder names and *identical* line layout in one VM (exactly the audit reproduction) and assert both generated modules compile; assert `x` arity is identical across a re-compilation of the same file.
4. Fix the related asymmetry: `generate_function_args({var,...})` uses default `[]` while the memo writers use default `empty` ([maxhml_eval.erl:862](detecter/src/synthesis/maxhml_eval.erl)); after the refactor there should be one lookup helper.

**GSD placement:** same Phase 5.2 as C1 (they gate the same claim: "synthesis produces sound, correct monitors"). Effort: small-medium.

---

## 4. High Findings

### H1 — `erlang:get_stacktrace/0` is *removed* (not merely deprecated): error paths crash with `undef` on OTP 24+

7 call sites: [gen_eval.erl:331](detecter/src/synthesis/gen_eval.erl), [hml_eval.erl:287](detecter/src/monitoring/hml_eval.erl), [lin_weaver.erl:158,202](detecter/src/synthesis/lin_weaver.erl), [weaver.erl:163,179,223](detecter/src/monitoring/weaver.erl). The project runs OTP 29; the compiler already warns "is removed". Any failure in those branches (e.g. output-dir creation error during synthesis) now dies with `undef` instead of the intended re-raise, hiding the real reason.

**Fix:** mechanical — `catch Class:Reason:Stacktrace -> erlang:raise(Class, Reason, Stacktrace)` / `try ... catch error:Reason:ST -> ...`. **Roadmap adjustment:** Phase 12 currently frames this as "remove compilation warnings"; retitle it "replace removed stacktrace API (error paths currently crash)" and consider pulling it forward — it is a 30-minute fix with real diagnostic value.

> **Revision (2026-07-07, thesis-text audit):** the original H2 fix removed the init-clause
> `update_current_state` call outright. The thesis's supplied-model convention, however,
> deliberately treats the payload bound at initialisation as the model's **first event**
> (the token-system walkthroughs depend on `s0 →(1)→ s1` at init), so the call was
> reinstated with the START-row derivation kept. The remaining exposure — an init payload
> outside the model's alphabet stores `[]` — fails conservatively (later recovery
> withholds) and is documented as a runtime-hardening limitation. The π₅-restricted
> walkthrough was replicated end-to-end against the reinstated code (checkpoint at
> `recv33`, ETS `current=s1, previous=s2`, no verdict) confirming thesis–code agreement.

### H2 — Generated init block poisons the SUS state table

Two compounding defects in [maxhml_eval.erl:645-682](detecter/src/synthesis/maxhml_eval.erl):

1. `generate_init_block` (single-modality path) emits `update_current_state(FirstBoundVar)` where `FirstBoundVar` is a **spawn argument** from the init pattern (e.g. `OwnTok`). That value is fed to `reachable_state(s0, OwnTok)` as if it were a model *event*. Unless the spawn argument coincidentally matches an `s0` transition, the result is `[]`, and `update_current_state` then **stores `current_state = []`** — every later recovery starts from a nonsense state and can only withhold. Verified in the generated `chained_flu.erl` above.
2. The `START/NULL` row parsed into `init_transitions()` is never used: `current_state` is hard-coded to `s0` (the thesis documents the hard-coding but not the poisoning).

No test executes `flu_spec/0` end-to-end (recovery tests seed ETS and call state functions directly), so this is invisible to the current suite.

**Fix (detailed):**
- Derive the initial state from the parsed `START` row (`{'START', S0Dest, {literal,null}, _}` → insert `S0Dest`), falling back to `s0` with a synthesis warning when absent.
- Drop the `update_current_state` call from the init clause **unless** the model actually defines the init payload as an event (if you want to keep the option, guard the generated call: only update when `reachable_state(Current, Event) =/= []`, else leave state untouched — but the simple removal matches the thesis's model semantics where the trace's *monitored events*, not spawn arguments, drive the automaton).
- Make `update_current_state/1` refuse to store `[]` (see M2) so this whole failure class becomes loud.
- Add one integration test that runs `flu_spec()` end-to-end: init event → send events → verdict, asserting ETS state after init. (This is also the natural seed for Phase 15's experiment suite.)

**GSD placement:** Phase 5.2 or fold into Phase 6/7 (spec grammar/parse errors) — but before any live demo of the tool, since `flu_spec/0` is the real entry point.

### H3 — `write_monitors/3` never sees its expected shapes; synthesis errors are silently swallowed

Dialyzer (run during audit): *"The pattern `[{'ok',_,_,_} | Monitors]` can never match"* — `write_lookup_monitor/3` and `write_monitor/3` actually return `ok | {error, atom()}` (the result of `file:close/1`!), so [gen_eval.erl:690-697](detecter/src/synthesis/gen_eval.erl) always falls through to the catch-all `write_monitors(_,_,_) -> ok`. Consequences: write/compile errors in the second monitor are never shown; `file:open` failures crash with `badmatch` instead of a structured error.

**Fix:** make both writers return `{ok, File}` or `{error, Reason}` explicitly (not `file:close/1`'s result), have `write_monitors` pattern-match those exact shapes, delete the catch-all, and surface errors through `compile/5`'s error return. Add `-spec`s that dialyzer accepts. **Run dialyzer in CI** (see §8) so contract rot is caught — this one had clearly been broken for a while.

### H4 — Necessity states never emit the dual `yes` verdict (documented-adjacent, but diverges from the thesis's own worked example)

Reference detectEr synthesizes `[p,c]ff` as `(p,c).no + (p,¬c).yes` — the thesis's monitor `m₁` (methodology eq. `φ₁`) shows exactly this. The modular generator emits only the positive clause for `nec` states ([maxhml_eval.erl:467-476, 645-668](detecter/src/synthesis/maxhml_eval.erl)); a guard-failing event is simply left in the mailbox and the monitor stays put. Sound but incomplete — and observably different from reference detectEr for the same property. Within the tested fragment the complement is encoded manually via the two-branch conjunction, which is why tests pass. Related: `invert_operator/1` ([maxhml_eval.erl:1181-1189](detecter/src/synthesis/maxhml_eval.erl)), used by the init-`pos` path, only inverts a leading `=:=` — for any other guard (or multi-conjunct guards) the "rejection" clause gets the **same or wrong** guard, so guard-failing init events block instead of rejecting; note ¬(G1∧G2) ≠ (¬G1)∧G2 in any case.

**Fix:** for the init-`pos` path, replace `invert_operator` with detectEr's own mutual-exclusion idiom — an unguarded catch-all second clause (`clause([pat], Guard, continue)` then `clause([pat/underscore], none, verdict)`), which is correct for every guard by construction. For non-init `nec` states, adding the dual clause interacts with AGM (the reduction fun, replay and pending dispatchers must all mirror it), so schedule it deliberately in v0.2 and, for now, state explicitly in the thesis that the modular fragment omits the complement branch and therefore under-approximates reference detectEr's verdicts on guard-failing events (one sentence in Chapter 6's robustness paragraph).

---

## 5. Medium Findings

### M1 — `update_current_state/1` is not atomic (two `ets:insert/2` calls)
[maxhml_agm_codegen.erl:727-754](detecter/src/synthesis/maxhml_agm_codegen.erl). Phase 5 made the *recovery* commit a single list insert precisely to avoid torn `{previous_state, current_state}` views, but the ordinary update still does two inserts on a **public named** table. Single-writer today, but externally observable and inconsistent with the recovery path. **Fix:** reuse `ets_insert_many/1` (already in the module) — a two-line codegen change + regenerate expectations.

### M2 — Nondeterministic models are silently mis-resolved instead of rejected
`agm_engine:reachable_state/3` folds over all transitions and **keeps the last match** ([agm_engine.erl:49-60](detecter/src/regeneration/agm_engine.erl)). The determinism precondition is documented as unvalidated, but the failure mode today is *silent wrong state*, not withholding. **Fix (cheap, high leverage for a soundness thesis):** validate at synthesis time when `init_transitions` is built — for each source state, check pairwise literal/literal duplicates and literal-within-symbolic overlaps (`agm_engine:event_spec_membership/2` already decides literal∈symbolic); reject the model or emit a loud warning. Optionally also make `reachable_state` return `error(nondeterministic_transition)` when two matches disagree (belt-and-braces at runtime). Extends Phase 6 (grammar definition) naturally; strengthens the thesis's "precondition, not enforced" into "precondition, checked where decidable".

### M3 — Consequence-signature dedup uses `==` semantics, thesis requires "identical bound values"
`merge_monitoring_consequences/2` uses `lists:keymember/3` ([agm_engine.erl:201-212](detecter/src/regeneration/agm_engine.erl)), which compares with arithmetic equality: signatures `{continue, f, [1]}` and `{continue, f, [1.0]}` merge. Decision log and thesis say continuations must carry *identical* bound values. **Fix:** `lists:any(fun({S, _}) -> S =:= Signature end, Acc)` + one engine test with `1` vs `1.0`.

### M4 — `sys_info_parser` internal representation and coverage defects (pre-v0.2 subset worth doing early)
Dialyzer-confirmed improper list: `[ParsedOp | parse_payload(...)]` builds `[setminus | {is_integer, 0}]` ([sys_info_parser.erl:88](detecter/src/regeneration/sys_info_parser.erl)); downstream code pattern-matches this improper list ([maxhml_agm_codegen.erl:620, 698](detecter/src/synthesis/maxhml_agm_codegen.erl)) — it works, but it is exactly the kind of representation Phase 22 exists to kill; make it a proper tuple `{setminus, Payload}`. Also: dead clause at [sys_info_parser.erl:89](detecter/src/regeneration/sys_info_parser.erl); `parse_atom`'s `try list_to_atom` catch is dead code (atom creation doesn't throw; the atom-table risk is SPEC-04's concern); descriptor/guard asymmetry — `generate_sys_info_event_spec` supports set-minus only on `is_any_integer` with integer payloads while `generate_sys_info_guard` also accepts atoms, so `{s1, N \ x, s2}` crashes descriptor generation with `function_clause`. Keep the full grammar/error model in Phases 6-8, but the improper list + dead clauses are 1-hour fixes now.

### M5 — Multi-property `.hml` files silently generate a monitor for only the first property
`visit_entry_form/3` and every `visit_function_forms/3` clause consume only the head form ([gen_eval.erl:560-645](detecter/src/synthesis/gen_eval.erl)). The legacy `mfa_spec` path handles all forms; the FLU path drops the rest without a message. **Fix:** raise `{error, multiple_properties_unsupported}` (fail loudly) until multi-property FLU synthesis is designed.

### M6 — Production compile hides all diagnostics: `-W0`
[Makefile:22](detecter/Makefile). `make compile` suppresses every warning — including "get_stacktrace is removed". **Fix:** drop `-W0`; burn down the ~16 warnings (mostly unused vars/functions — see L4); then add `-Werror` (or `+warnings_as_errors`) to keep the signal. Note `compile-test` builds *src* with `-DTEST`, so production artifacts should always come from `make compile` — document this in the README (Phase 17/20).

### M7 — Conjunction guard in `generate_function` admits shapes its helpers reject
The `?HML_AND` clause guard `when ModLeft =:= nec; ModRight =:= pos; ModRight =:= nec; ModLeft =:= pos` ([maxhml_eval.erl:259](detecter/src/synthesis/maxhml_eval.erl)) is satisfied by *any* one disjunct, but `extract_bound_vars_from_guard`'s conjunction clause requires **both** sides `nec` ([maxhml_eval.erl:1125-1135](detecter/src/synthesis/maxhml_eval.erl)) — mixed `pos` conjunctions crash with `function_clause` mid-generation. Fold this into C1's fragment validator (reject `pos` branches explicitly), and tighten the guard to the actually-supported `andalso` form.

### M8 — Generated code shadows `LookaheadEnvelope` and mis-names adapter parameters
The reduction-fun continuations bind `fun(LookaheadEnvelope) -> ...` inside a scope where `LookaheadEnvelope` is already bound ([maxhml_agm_codegen.erl:231-256](detecter/src/synthesis/maxhml_agm_codegen.erl)) — legal, correct, but every generated monitor compiles with shadow warnings that will mask real ones. Use a distinct generated name (`ReplayEnvelope`). Cosmetic-but-adjacent: the `event_spec_membership` adapter names its args `Event, State` for what are `EventSpec, Value` ([maxhml_agm_codegen.erl:818-823](detecter/src/synthesis/maxhml_agm_codegen.erl)).

### M9 — Two different "state update args" filters can generate `update_current_state/2` calls
The `nec` receive path filters only `'_'` ([maxhml_eval.erl:457](detecter/src/synthesis/maxhml_eval.erl)) while `generate_state_update_args/1` uses `?STATE_UPDATE_EXCLUDED_VARS` (`'From'` etc.) and takes exactly one var. A pattern binding two named vars (e.g. `{Sender:_ ! Tok}`) generates `update_current_state(Sender, Tok)` → `undef` at runtime. This is the same class as the Phase 3 arity bug (GEN-03's regression tests don't cover this variant). **Fix:** route all clause generation through `generate_state_update_calls/1`; this is Phase 22's normalization — raise its priority from "backlog" given it has already produced one shipped bug.

---

## 6. Low Findings / Cleanup

- **L1** — `preceeding_*` misspelling in generated adapters ([maxhml_agm_codegen.erl:788-799](detecter/src/synthesis/maxhml_agm_codegen.erl)) *and* thesis listing `lst:prec_states_event`. Rename to `preceding_*` in one coordinated commit (code + tests + thesis) or accept permanently — currently it is at least consistent.
- **L2** — `reachable_state` "no state" sentinel is `[]` typed `state() | []` — un-idiomatic and enables H2's poisoning. Prefer `{ok, State} | error`. Touches generated code; schedule with M2.
- **L3** — Source-regex "architecture tests" (`generated_agm_recovery_test` lines 226-241/288-300/342-364/570-581; `agm_engine_test:reduction_type_is_envelope_aware_test`) are brittle against pretty-printer changes. Acceptable as thesis-claim anchors; consider asserting on `Module:module_info(exports)`/AST instead of raw text where possible.
- **L4** — Warning burn-down list (from this audit's compile): unused types `with()`/`spec()` ([maxhml_eval.erl:124,129](detecter/src/synthesis/maxhml_eval.erl)); always-false guard at [maxhml_eval.erl:1067](detecter/src/synthesis/maxhml_eval.erl); unused vars in `visit_entry_form`/`visit_function_forms`; unused `lin_*` helpers; exported-from-call vars in `lin_weaver`. Plus large commented-out blocks and `TODO/DOUBLE CHECK` markers in `maxhml_eval` — convert to tracked issues (GSD capture) and delete.
- **L5** — `automated_event_streamer.erl` (untracked; STATE.md blocker): it is a manual driver that exercises `flu_spec/0` with an init event. Decision recommendation: keep it, move under `test/manual/` with a comment header, and wire it into Phase 15's experiment suite — it is currently the *only* artifact that runs the generated init path (see H2).
- **L6** — Test nits: `?assertEqual([Expected], [Expected | collect_verdicts([])])` ([generated_agm_recovery_test.erl:117](detecter/test/regeneration/generated_agm_recovery_test.erl)) is an obfuscated "no more verdicts" assertion — write `?assertEqual([], collect_verdicts([]))`; `cleanup_suite` purges before delete (works, but `code:delete` then `code:purge` is the canonical order).
- **L7** — Build/tooling: `make analyze` fails on a fresh machine (no PLT) — add a `plt:` target (`dialyzer --build_plt --apps erts kernel stdlib compiler syntax_tools`) and document required OTP (Phase 20). Consider migrating to `rebar3` (gets you `rebar3 dialyzer`, `eunit`, `fmt`, `lint` and dependency hygiene for free) — Phase 20 already contemplates this.
- **L8** — License headers: `agm_engine.erl`, `maxhml_agm_codegen.erl`, `sys_info_parser.erl` carry no license header while the repo is GPL-3 and upstream files carry the full notice. Add headers to new modules (and your authorship), keeping upstream attribution intact in modified files.

---

## 7. Thesis Cross-Check Findings (beyond the Phase 5 audit's dispositions)

The Phase 5 `05-THESIS-AUDIT.md` dispositions were spot-checked and hold. Additional items found by this audit:

- **T1 — Formal notation bug in methodology eq. `S_{X_0}^{α_{-1}}`** ([methodology.tex:276-279](../Master-Thesis/Documentation/Thesis/chap3/methodology.tex)): the set is defined with `∃X₋₁ ∈ PROC`, i.e. destinations of α₋₁ from *any* state — but X₋₁ is the *known* last state (the worked example computes `{s₁}` from known `s₂`; under the written definition the answer would be `{s₁, s₃}` since `s₃ --3--> s₃`). Fix the definition to use the known X₋₁ (no existential), mirroring eq. `S_{X_2}^{α_1}` where the existential *is* correct because X₂ is genuinely unknown. An examiner in formal methods will notice this.
- **T2 — Listing `lst:parseablestt` ends `{s3, Z \ 0, s3}.`** — the parser does not strip a trailing period (it strips only `;`), so the thesis's own example file is unparseable as shown (it would produce the atom `'s3}.'`). Either fix the listing to end with `;` (matching `test/regeneration/sys_info.spec`) or make the parser accept the terminal `.` (one-line trim change) — then the thesis and parser agree.
- **T3 — Related-work gaps (recommend adding):**
  - *Kauffman, Havelund, Fischmeister — "Monitorability over Unreliable Channels" (RV'19) and "What can we monitor over unreliable channels?" (STTT 2021)*: the definitive classification of which properties admit **trustworthy verdicts under loss mutations** — the same question your methodology answers operationally with a model. Not currently cited (verified against all `.bib` files); it belongs in the language-based strand of §"Existing Strategies" and sharpens your contribution ("they characterize immune properties; we recover non-immune cases by exploiting an operational model").
  - *Predictive runtime verification* (Pinisetty et al., "Predictive runtime verification of timed properties"; also Zhang/Leucker "predictive semantics"): uses a priori system knowledge to anticipate verdicts — the closest published family to AGM's use of an operational model. Positioning against it preempts the obvious examiner question "how is this not predictive RV?" (answer: predictive RV *accelerates* verdicts on complete traces; AGM *recovers soundness* on incomplete ones — same knowledge, different problem).
- **T4 — "AGM" acronym collision**: in the logic/KR literature AGM universally means Alchourrón–Gärdenfors–Makinson belief revision — an adjacent formal-methods audience *will* misread it. Add a first-use footnote ("not to be confused with AGM belief revision") in the abstract/intro.
- **T5 — Silent acceptance of unsupported shapes is currently undocumented** (pairs with C1): until the validator exists, the thesis text "General nested possibility, disjunction, arbitrary conjunction, and arbitrary chained modalities are not implemented" understates reality — they are not implemented *but are accepted and mis-synthesized*. Fix the code (preferred) rather than weakening the thesis.
- **T6 — φ₁ worked example vs implementation** (pairs with H4): Chapter "RV's Operational Domain" derives `m₁ = (x,x=-1).no + (x,x≠-1).yes` and narrates the `yes` reduction on event `1`; the modular generator cannot produce that `yes` for a bare guarded init-necessity. Chapter 6's robustness caveat should state the omission of the complement branch explicitly.

---

## 8. Modernization / Industry-Standard Recommendations (cross-cutting)

1. **CI gate (highest leverage, ~1 hour):** a GitHub Actions workflow running `make compile` (no `-W0`), `make test`, and `dialyzer` (cached PLT) on OTP 27/29(/master). Every finding class in §3-§5 (removed BIFs, contract rot, warning creep, VM-order-dependent test luck) becomes machine-caught. Suggested as a new early plan in v0.3 or alongside Phase 20.
2. **Dialyzer debt:** the audit's focused run (engine + codegen + eval + parser + gen_eval) produced a short, high-signal list (§H3, §M4). Add `-spec`s to the public API of `maxhml_agm_codegen` (currently none) — the engine already has good specs.
3. **Formatting/linting:** adopt `erlfmt` + `elvis` (or `rebar3 lint`) to normalize the mixed indentation and comment styles between inherited detectEr code and new modules; run once, then enforce in CI.
4. **Property-based testing (thesis-strengthening):** Phase 15's experiment suite is a natural fit for `proper`: generate random deterministic LTSs + traces, delete one event, and assert the two machine-checkable invariants — (i) recovered verdict ∈ {complete-trace verdict, withhold} (soundness), (ii) verdict emitted ⇒ identical to complete-trace verdict (VERD-01 generalization). This converts the bounded 8-row matrix into thousands of randomized checks and would materially strengthen Chapter 7's evidence claims.
5. **Structured logging:** the synthesis TRACE output is unconditional to stdout during tests (visible in `make test`); route `log.hrl` macros through `logger` levels so test output stays readable.
6. **Naming/versioning hygiene:** tag the commit that closes v0.1 (`git tag v0.1-thesis-claims`) so thesis listings reference an immutable baseline (DOC-01 traceability).

---

## 9. Security Posture (ASVS-lite, consistent with 05-RESEARCH)

| Area | Status | Notes |
|---|---|---|
| Input validation (V5) | **Deferred, correctly scoped** | `.spec`/`.hml` parsing is crashy but fail-stop; scheduled Phases 6-9. C1 is the exception — an *accepted* input yields unsound output (fail-open) — hence its criticality. |
| Atom exhaustion (SPEC-04) | Open, scheduled Phase 9 | `list_to_atom` in `sys_info_parser` (3 sites) + `gen_eval:compile` module names from file paths. |
| Shared mutable state | Open, scheduled Phase 11 | Public named `sus_state` + named `monitorTable`; single-VM research tool, acceptable interim. |
| Test-only surface | ✅ | Checkpoint clauses generated only under `-DTEST` (`-ifdef` verified); ensure production builds use `make compile`. |
| Dependency risk | ✅ minimal | OTP-only, no third-party deps. |

---

## 10. Planning-Docs Hygiene (fix before commit)

1. **ROADMAP.md Progress table is stale:** Phase 4 row says `1/3 In progress` though all three plans are `[x]` and STATE.md records 3/3 complete; Phase 4.1 has no row at all; Phase 4/5 completion dates are missing.
2. **REQUIREMENTS.md traceability:** VERD-01..03 rows say "Verified - pending manual review" — after this review, flip to Complete alongside the commit.
3. **STATE.md blockers:** the `automated_event_streamer.erl` ownership decision can be closed with L5's recommendation.

---

## 11. Suggested Sequencing

| When | What | Items |
|---|---|---|
| **Now (this commit)** | Commit Phase 5 as-is (it is correct); fix planning docs | §10 |
| **Phase 5.2 (new, small — before declaring v0.1 done)** | Fail-closed fragment validation; compiler-state isolation; init-state fix; M3; M6 (-W0 removal + warning burn-down); H1 (mechanical) | C1, C2, H1, H2, M3, M6 |
| **v0.2 (existing scope, augmented)** | Parser/grammar work absorbs M4, M2 (determinism validation), M5; H4 dual-branch design; H3 | Phases 6-9 |
| **v0.3** | CI + dialyzer gate (new plan), ETS isolation, tracer hardening | §8.1, Phases 10-13 |
| **v0.4** | Property-based AGM experiment suite; thesis edits T1-T6 | §8.4, Phases 14-17 |
| **v0.5** | M9/L1/L2/L4/L8 cleanup under Phases 19/22 | — |

---

## Appendix A — Reproduction Commands

**C1 (unsound chained necessity):**
```bash
cat > /tmp/chained.hml <<'EOF'
with
  token_server:loop(_, _)
check
  [{_ <- _, token_server:loop(OwnTok, _)}]
  [{_:_ ! Tok when OwnTok =:= Tok}]ff.
EOF
printf '{s1, 0, s2};\n{s2, 9, s3};\n' > /tmp/chained.spec
erl -noshell -pa detecter/ebin -eval \
  'maxhml_eval:compile("/tmp/chained.hml",[{outdir,"/tmp"},{mtab,"/tmp/chained.spec"},erl]),init:stop().'
grep -A1 "^send5" /tmp/chained_flu.erl   # => rejection(From) with no receive
```

**C2 (persistent_term corruption):** compile two properties whose only difference is the binder name (`OwnTok` vs `BaseTok`) in one `erl` invocation, then `erlc` the second `_flu.erl` — it fails with unbound-variable errors and a phantom `x/3`.

**Dialyzer (focused):**
```bash
dialyzer --build_plt --apps erts kernel stdlib compiler syntax_tools   # once
cd detecter && make compile-test && \
dialyzer -pa ebin ebin/agm_engine.beam ebin/maxhml_agm_codegen.beam \
  ebin/maxhml_eval.beam ebin/sys_info_parser.beam ebin/gen_eval.beam
```
