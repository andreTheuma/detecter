# Phase 5: Irrevocability and Verdict Semantics - Research

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

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

### the agent's Discretion
- Exact names and placement of generated replay helpers.
- Test fixture organization and reusable synchronization helpers.
- The smallest generated AST refactor needed to delay ETS mutation until consequence agreement.

### Deferred Ideas (OUT OF SCOPE)
- General buffering proxy or event broker - future architecture work.
- Continued monitoring after ambiguous recovery to seek later disambiguation - new research beyond the thesis.
- A `{recovered_event, ...}` protocol - unnecessary extension to the current monitor interface.
- Absorbing terminal receive loops - observationally unnecessary for the thesis implementation.
- Native tracer-side missing-event detection, repeated missing events, non-`send` lookahead generalization, parser hardening, ETS isolation, and exhaustive formula verification - future milestones.
</user_constraints>

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| VERD-01 | Deterministically recovered traces produce the same verdict as corresponding complete traces. | Preserve the complete lookahead envelope, resolve recovery before mutation, atomically commit the post-missing state, and call a generated direct dispatcher for the recovered continuation. Compare observable verdict/continuation behavior over a bounded complete-versus-recovered matrix. [VERIFIED: `.planning/REQUIREMENTS.md`; `.planning/phases/05-irrevocability-and-verdict-semantics/05-CONTEXT.md`] |
| VERD-02 | Ambiguous missing traces do not produce eager verdicts. | Keep all four stable `{withhold, Reason}` paths terminal and non-verdicting; prove unchanged ETS state, normal process exit, internal result, and an empty verdict mailbox. [VERIFIED: `.planning/REQUIREMENTS.md`; `detecter/src/regeneration/agm_engine.erl`; `detecter/test/regeneration/generated_agm_recovery_test.erl`] |
| VERD-03 | Once emitted, a verdict is not retracted by later trace extension or event recovery. | Test `yes` and `no`, complete and recovered paths, with extensions queued before the decisive event is processed and sent after the verdict; assert one verdict and normal termination. [VERIFIED: `.planning/REQUIREMENTS.md`; `.planning/phases/05-irrevocability-and-verdict-semantics/05-CONTEXT.md`] |
</phase_requirements>

**Researched:** 2026-07-02  
**Domain:** Erlang generated-monitor orchestration, missing-event lookahead replay, ETS commit ordering, and verdict lifecycle  
**Confidence:** HIGH

## Summary

The implementation gap is localized to generated orchestration. `agm_engine:recover_missing_event/3` and `resolve_monitoring_consequence/2` are already pure and return enough recovery/consequence data; they must remain free of ETS, receive, replay, and verdict effects. The current generated `handle_missing_event/1` consumes only a `send` trace envelope, extracts its payload, and writes `previous_state/current_state` before consequence agreement. `generate_missing_event_recovery_case/1` subsequently invokes a zero-arity continuation, so a continuing recovery omits the consumed lookahead. [VERIFIED: `detecter/src/regeneration/agm_engine.erl`; `detecter/src/synthesis/maxhml_agm_codegen.erl`]

Implement replay by returning the untouched lookahead envelope with the recovery map, resolving the complete monitoring consequence, performing one list-valued `ets:insert/2` for the recovery commit, and invoking an envelope-aware generated callback. For a non-terminal consequence, that callback directly calls a per-state dispatcher containing the same patterns, guards, state updates, and continuations as the ordinary receive clauses. Its catch-all tail-calls the recovered state, which reproduces selective-receive behavior for an irrelevant already-consumed lookahead without requeueing it. For a terminal consequence at the missing position, the callback ignores the later envelope and invokes the existing verdict function. [VERIFIED: Phase 5 D-01 through D-04; Erlang ETS documentation]

Tests should extend `generated_agm_recovery_test.erl`, compile temporary generated modules with `export_all`, and use a generated test-only checkpoint clause under `-DTEST`. A reference-tagged checkpoint sent after prior trace messages gives deterministic evidence that the monitor reached a specific continuation; fixed sleeps and `process_info/2` polling are unnecessary. Process monitors and the worker's explicit result message establish normal termination. [VERIFIED: existing test harness and Phase 5 D-07/D-10]

**Primary recommendation:** Refactor generated receive states into ordinary receive clauses plus shared direct dispatchers, make resolved continuation callbacks accept the original lookahead envelope, and place one atomic ETS list insert strictly between consequence agreement and callback invocation.

## Architectural Responsibility Map

| Capability | Primary Tier | Secondary Tier | Rationale |
|------------|--------------|----------------|-----------|
| State inference and consequence aggregation | Pure runtime engine (`agm_engine`) | — | The engine already computes recovery and deduplicates complete consequence signatures without effects; Phase 5 must preserve that boundary. [VERIFIED: `agm_engine.erl`; Phase 4.1 D-03] |
| Lookahead capture, commit, and replay | Generated monitor runtime | Code generator | These operations depend on the live mailbox, ETS table, generated clauses, and verdict recipient. [VERIFIED: Phase 5 D-01; `maxhml_agm_codegen.erl`] |
| Replay-dispatch AST construction | Code generator (`maxhml_agm_codegen`) | maxHML traversal (`maxhml_eval`) | The generator owns AGM-specific AST; traversal supplies branch patterns, guards, targets, and bound arguments. [VERIFIED: Phase 4.1 D-04/D-05] |
| Observable equivalence and lifecycle tests | EUnit generated-module harness | Erlang VM process/ETS primitives | The existing harness compiles temporary modules, executes generated states, observes verdict messages, and inspects ETS. [VERIFIED: `generated_agm_recovery_test.erl`] |
| Thesis reconciliation | Thesis sources | Phase verification evidence | D-19 requires the active chapters to match final replay and verdict behavior after tests pass. [VERIFIED: Phase 5 CONTEXT.md] |

## Standard Stack

### Core

| Library/runtime | Version | Purpose | Why Standard |
|-----------------|---------|---------|--------------|
| Erlang/OTP | OTP 29 / ERTS 17.0.2 | Generated monitor execution, processes, selective receive, ETS, compiler APIs | Installed project runtime; the repository is an Erlang codebase compiled directly by `erlc`. [VERIFIED: local environment; `detecter/Makefile`] |
| EUnit | OTP-bundled | Generated integration and pure-engine tests | Existing default regression framework and assertion style. [VERIFIED: `detecter/Makefile`; test modules] |
| `syntax_tools` / `erl_syntax` | OTP-bundled | Build generated Erlang AST | Existing synthesis implementation uses `erl_syntax` throughout. [VERIFIED: `maxhml_eval.erl`; `maxhml_agm_codegen.erl`] |
| ETS | OTP-bundled | Generated system-state storage | Existing generated monitors use named table `sus_state`; isolation redesign is explicitly deferred. [VERIFIED: generated code and Phase 5 deferred scope] |

### Supporting

| Tool | Version | Purpose | When to Use |
|------|---------|---------|-------------|
| GNU Make | 3.81 | Compile and run the complete regression gate | Run `make test` from `detecter/` after each implementation wave. [VERIFIED: local environment; `detecter/Makefile`] |
| `spawn_monitor/1` / process monitors | OTP 29 | Observe normal worker termination | Use in all verdict and withholding lifecycle tests. A monitor delivers `{'DOWN', Ref, process, Pid, Reason}` when the process terminates. [CITED: https://www.erlang.org/doc/system/ref_man_processes.html] |

**Installation:** None. This phase introduces no external package. [VERIFIED: phase scope and existing build]

## Architecture Patterns

### System Architecture Diagram

```text
test/tracer
   |
   | {missing_event, From}
   v
generated receive state
   |
   | consumes LookaheadEnvelope =
   | {{trace, Pid, send, Payload, To}, From}
   v
handle_missing_event/1
   |
   | agm_engine:recover_missing_event(Transitions, Source, Payload)
   | returns Recovery + original LookaheadEnvelope
   | (NO ETS mutation)
   v
resolve_monitoring_consequence(Recovery, ReductionFun)
   |
   +-- {withhold, Reason} ----------------> normal return, no verdict, no commit
   |
   +-- {ok, Consequence}
          |
          | ets:insert(sus_state,
          |   [{previous_state, Source},
          |    {current_state, Inferred}])
          v
      envelope-aware continuation callback
          |
          +-- terminal consequence -------> acceptance/rejection -> one verdict -> normal exit
          |
          +-- continuing consequence -----> direct per-state dispatch
                                                |
                                                +-- matches ordinary clause -> ordinary update/reduction
                                                |
                                                +-- irrelevant envelope -> recovered state receive loop
```

This flow keeps the pure engine effect-free and makes the commit conditional on both singleton recovery and one complete consequence. [VERIFIED: Phase 4.1 boundary and Phase 5 D-11]

### Recommended Project Structure

```text
detecter/
├── src/regeneration/agm_engine.erl              # Pure types/aggregation only
├── src/synthesis/maxhml_agm_codegen.erl          # Recovery, commit, replay-dispatch AST
├── src/synthesis/maxhml_eval.erl                 # Supplies ordinary branch clauses/targets
└── test/regeneration/
    ├── agm_engine_test.erl                       # Callback-shape contract if changed
    └── generated_agm_recovery_test.erl           # VERD-01/02/03 integration matrix
```

No new production module is warranted. [VERIFIED: existing ownership boundaries; Phase 5 scope]

### Pattern 1: Preserve the Envelope at the Effect Boundary

`handle_missing_event/1` should return recovery metadata and the exact received envelope, with no ETS write:

```erlang
%% Representative generated shape
handle_missing_event(From) ->
    SourceState = ets:lookup_element(sus_state, current_state, 2),
    receive
        Lookahead =
            {{trace, _, send, Payload, _}, From} ->
            case agm_engine:recover_missing_event(
                init_transitions(), SourceState, Payload
            ) of
                {ok, Recovery} ->
                    {ok, Recovery, Lookahead};
                {withhold, Reason} ->
                    {withhold, Reason}
            end
    end.
```

The original envelope carries source PID, event kind, payload, destination, and verdict recipient; reconstructing only `Payload` would not reproduce the generated clause match. [VERIFIED: `gen_eval:pat_tuple/1`; Phase 5 D-02]

### Pattern 2: Commit Only After Consequence Agreement

```erlang
case handle_missing_event(From) of
    {ok, Recovery, Lookahead} ->
        case resolve_monitoring_consequence(Recovery, ReductionFun) of
            {ok, #{continuation := Continue}} ->
                commit_recovery_state(Recovery),
                Continue(Lookahead);
            {withhold, Reason} ->
                {withhold, Reason}
        end;
    {withhold, Reason} ->
        {withhold, Reason}
end.

commit_recovery_state(Recovery) ->
    Source = maps:get(source_state, Recovery),
    Inferred = maps:get(inferred_state, Recovery),
    ets:insert(
        sus_state,
        [{previous_state, Source}, {current_state, Inferred}]
    ).
```

One `ets:insert/2` call with a list is atomic and isolated for the entire operation. [CITED: https://www.erlang.org/docs/29/apps/stdlib/ets.html#insert-2]

### Pattern 3: Shared Ordinary/Replay Dispatch

Generate one direct dispatcher per non-terminal receive state. Ordinary mailbox clauses and replay must use the same branch bodies:

```erlang
state(Args, From) ->
    receive
        Envelope = {{trace, _, send, Event, _}, From}
                when relevant(Event) ->
            dispatch_state(Envelope, Args, From);
        {missing_event, From} ->
            recover_state(Args, From);
        {agm_checkpoint, Ref, TestPid} ->
            TestPid ! {agm_checkpoint, Ref, self(), state},
            state(Args, From)
    end.

dispatch_state(
    {{trace, _, send, Event, _}, From},
    Args,
    From
) when relevant(Event) ->
    update_current_state(Event),
    next_state(Args, From);
dispatch_state(_IrrelevantLookahead, Args, From) ->
    state(Args, From).
```

The wildcard belongs only in the direct dispatcher. Adding it to the ordinary `receive` would consume irrelevant future mailbox events and change selective-receive semantics. Erlang leaves all non-selected mailbox messages unchanged. [CITED: https://www.erlang.org/doc/system/expressions.html#receive]

### Pattern 4: Envelope-Aware Consequence Callback

Keep consequence identity unchanged, but make the stored callback accept one lookahead envelope:

```erlang
%% Continuing consequence
{{continue, NextFunction, BoundArgs},
 fun(Lookahead) ->
     dispatch_next_function(Lookahead, BoundArgs)
 end}

%% Terminal consequence at the missing position
{{verdict, no},
 fun(_Lookahead) ->
     rejection(From)
 end}
```

`agm_engine` still compares only `{verdict, yes|no}` or `{continue, Function, CompleteArgs}` signatures and never invokes the callback. [VERIFIED: `agm_engine:resolve_monitoring_consequence/2`]

### Pattern 5: Deterministic Test Checkpoint

Compile `maxhml_agm_codegen` with the existing `-DTEST` build and generate a reference-tagged checkpoint clause only in test builds:

```erlang
-ifdef(TEST).
checkpoint_clause(StateName, StateArgs) ->
    %% AST equivalent of:
    %% {agm_checkpoint, Ref, TestPid} ->
    %%     TestPid ! {agm_checkpoint, Ref, self(), StateName},
    %%     StateName(StateArgs)
    ...
-else.
checkpoint_clause(_StateName, _StateArgs) ->
    [].
-endif.
```

The test sends lookahead then checkpoint from the same process. Erlang preserves signal order from one sender to one destination, so receipt of the checkpoint acknowledgement proves all earlier test messages were delivered in order and the monitor reached the target receive state. [CITED: https://www.erlang.org/doc/system/ref_man_processes.html#signals]

### Anti-Patterns to Avoid

- **Requeueing the lookahead with `self() ! Lookahead`:** introduces mailbox scheduling and violates D-02. [VERIFIED: Phase 5 CONTEXT.md]
- **Reconstructing `{{trace, _, send, Payload, _}, From}`:** loses original envelope fields and can change pattern matching. [VERIFIED: `gen_eval:pat_tuple/1`]
- **Committing inside `handle_missing_event/1`:** state can advance on `ambiguous_consequence` or `unproven_consequence`, which violates D-11. [VERIFIED: current defect in `maxhml_agm_codegen.erl`]
- **Adding a catch-all ordinary receive clause:** consumes events that current selective receive leaves queued. [CITED: https://www.erlang.org/doc/system/expressions.html#receive]
- **Using `timer:sleep/1` or polling `current_function`:** does not establish a deterministic logical boundary. [VERIFIED: Phase 5 D-07]
- **Keeping the process alive in an absorbing verdict loop:** explicitly deferred and unnecessary because terminal functions already return after sending. [VERIFIED: Phase 5 D-14 and deferred ideas]
- **Turning `{withhold, Reason}` into a third external verdict:** violates the fixed `yes|no` verdict domain. [VERIFIED: Phase 5 D-09/D-12]

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Atomic two-key recovery commit | Rollback flags or a custom lock protocol | One list-valued `ets:insert/2` | OTP guarantees whole-operation atomicity and isolation. [CITED: https://www.erlang.org/docs/29/apps/stdlib/ets.html#insert-2] |
| Event ordering in tests | Sleeps or retry loops | Same-sender ordering plus a reference-tagged checkpoint | Establishes a causal boundary rather than elapsed-time evidence. [CITED: https://www.erlang.org/doc/system/ref_man_processes.html#signals] |
| Normal termination observation | `is_process_alive/1` polling | `spawn_monitor/1` and `DOWN` reason `normal` | Process monitors are the standard lifecycle primitive. [CITED: https://www.erlang.org/docs/29/apps/erts/erlang.html#spawn_monitor-1] |
| Replay buffering | Proxy process, event broker, or mailbox resend | Generated direct dispatcher | Buffering is out of scope and direct replay is locked by D-02. [VERIFIED: Phase 5 CONTEXT.md] |
| Alternative verdict semantics | Pending/unknown verdict protocol | Internal `{withhold, Reason}` plus no verdict message | The thesis trades completeness for soundness without extending the verdict domain. [VERIFIED: thesis methodology and Phase 5 D-09] |

## Common Pitfalls

### Pitfall 1: “Atomic” Commit Still Uses Two Calls

**What goes wrong:** `previous_state` is visible with the new value while `current_state` is still old, or vice versa.  
**Why it happens:** Two individually atomic calls are not one atomic multi-object operation.  
**How to avoid:** Insert both tuples in one `ets:insert/2` list after consequence agreement. [CITED: https://www.erlang.org/docs/29/apps/stdlib/ets.html#insert-2]  
**Warning sign:** Two consecutive `ets_insert(...)` AST nodes remain in the successful recovery branch.

### Pitfall 2: Replay Duplicates or Skips the Lookahead State Update

**What goes wrong:** Calling `update_current_state/1` in recovery orchestration and again in the ordinary branch advances twice; invoking only the recovered continuation skips the lookahead entirely.  
**How to avoid:** Commit only the post-missing inferred state, then let the shared ordinary dispatcher own the lookahead's ordinary state update. [VERIFIED: Phase 5 D-03/D-04]  
**Warning sign:** Replay orchestration calls `update_current_state(Payload)` outside the ordinary branch body.

### Pitfall 3: Irrelevant Direct Replay Is Treated as Consumption

**What goes wrong:** The recovered monitor discards the lookahead and returns a value instead of remaining in the recovered continuation.  
**How to avoid:** The direct dispatch catch-all tail-calls the recovered receive state. [VERIFIED: Phase 5 D-04]  
**Warning sign:** The dispatcher has no fallback or its fallback returns `ok`.

### Pitfall 4: Withholding Mutates ETS

**What goes wrong:** An ambiguous or unproven trace emits no verdict but leaves the global state advanced, contaminating later checks.  
**How to avoid:** Keep `handle_missing_event/1` read-only and place the only recovery commit in the `{ok, Consequence}` branch. [VERIFIED: current generated flow and D-11]  
**Warning sign:** A withholding test finds `current_state = InferredState`.

### Pitfall 5: Empty Mailbox Is Checked Before the Worker Finishes

**What goes wrong:** `receive after 0` passes before a delayed verdict arrives.  
**How to avoid:** Await the worker result and `DOWN` with reason `normal`, then inspect the verdict mailbox. The generated verdict and wrapper result are sent by the same worker in program order. [CITED: https://www.erlang.org/doc/system/ref_man_processes.html#signals]  
**Warning sign:** `assert_no_verdict/0` runs while the worker is alive.

### Pitfall 6: A Verdict Count Assertion Consumes Only the Expected Atom

**What goes wrong:** A contradictory or duplicate verdict remains unnoticed in the mailbox.  
**How to avoid:** After normal termination, collect both `yes` and `no` messages and assert the exact list is `[Expected]`. [VERIFIED: D-14/D-17]  
**Warning sign:** Helpers such as the current `await_plain_verdict/0` match only `no`.

### Pitfall 7: Tests Broaden the Research Claim

**What goes wrong:** The plan drifts into repeated missing events, non-`send` lookahead, arbitrary maxHML, tracer loss detection, or ETS isolation.  
**How to avoid:** Use one explicit missing marker, the current generated necessity/conjunction fragment, existing envelopes, and bounded fixtures only. [VERIFIED: Phase boundary and deferred ideas]

## Deterministic Validation Matrix

| Case | Recovery form | Lookahead behavior | Expected observation | Requirements |
|------|---------------|--------------------|----------------------|--------------|
| Complete vs recovered `yes` | Exact literal | Matching lookahead reaches acceptance | Same single `yes`, normal exit | VERD-01, VERD-03 |
| Complete vs recovered `no` | Exact literal | Matching lookahead reaches rejection | Same single `no`, normal exit | VERD-01, VERD-03 |
| Complete vs recovered `no` | Multiple literals, one complete consequence | Matching lookahead reaches rejection | Same single `no`; recovery metadata event remains `unknown` | VERD-01 |
| Continued no-verdict prefix | Exact literal | Irrelevant lookahead | Both acknowledge the same test checkpoint with no verdict; common suffix then has equal outcome | VERD-01 |
| Ambiguous state | Ambiguous inferred state | Recovery stops | `{withhold, ambiguous_state}`, unchanged ETS, normal exit, no verdict | VERD-02 |
| Impossible recovery | No inferred state | Recovery stops | `{withhold, impossible_recovery}`, unchanged ETS, normal exit, no verdict | VERD-02 |
| Ambiguous consequence | One inferred state, conflicting signatures | Recovery stops before commit | `{withhold, ambiguous_consequence}`, unchanged ETS, normal exit, no verdict | VERD-02 |
| Unproven consequence | Unsupported symbolic proof | Recovery stops before commit | `{withhold, unproven_consequence}`, unchanged ETS, normal exit, no verdict | VERD-02 |
| Queued extension | Complete/recovered × `yes`/`no` | Decisive envelope followed immediately by contradictory/duplicate-provoking envelopes | Exactly one expected verdict; normal exit | VERD-03 |
| Post-verdict extension/recovery detail | Complete/recovered × `yes`/`no` | Send extension and concrete trace envelope after receiving verdict | Exactly one expected verdict; normal exit | VERD-03 |

The matrix is bounded and representative; it is not an exhaustive maxHML proof. [VERIFIED: Phase 5 D-08 and deferred scope]

## Code Examples

### Lifecycle Helper

```erlang
spawn_monitor_state(Module, Function, Args, Parent) ->
    spawn_monitor(fun() ->
        Result = apply(Module, Function, Args),
        Parent ! {worker_result, self(), Result}
    end).

await_normal_result(Pid, Ref) ->
    Result =
        receive
            {worker_result, Pid, Value} -> Value
        after 1000 ->
            erlang:error({worker_result_timeout, Pid})
        end,
    receive
        {'DOWN', Ref, process, Pid, normal} -> Result;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error({worker_failed, Reason})
    after 1000 ->
        erlang:error({worker_down_timeout, Pid})
    end.
```

Timeouts are failure bounds, not synchronization evidence; the result and `DOWN` messages are the synchronization. [CITED: https://www.erlang.org/doc/system/ref_man_processes.html#monitors]

### Exact Verdict Collection

```erlang
collect_verdicts(Acc) ->
    receive
        Verdict when Verdict =:= yes; Verdict =:= no ->
            collect_verdicts([Verdict | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

assert_single_verdict(Expected) ->
    ?assertEqual([Expected], collect_verdicts([])).
```

Call this only after normal worker termination. [VERIFIED: lifecycle analysis above]

### Reference-Tagged Checkpoint

```erlang
checkpoint(Worker) ->
    Ref = make_ref(),
    Worker ! {agm_checkpoint, Ref, self()},
    receive
        {agm_checkpoint, Ref, Worker, StateName} ->
            StateName
    after 1000 ->
        erlang:error({checkpoint_timeout, Worker})
    end.
```

Using a fresh reference isolates the acknowledgement from unrelated test messages. [CITED: https://www.erlang.org/doc/system/expressions.html#receive]

## Plan Decomposition

### Plan 05-01 — Atomic Recovery and Ordered Replay

- Refactor generated non-terminal receive states to expose per-state direct dispatchers while preserving ordinary selective receive.
- Change generated reduction callbacks to accept the original lookahead envelope.
- Make `handle_missing_event/1` return `{ok, Recovery, LookaheadEnvelope}` without mutation.
- Generate `commit_recovery_state/1` using one list-valued `ets:insert/2`.
- Update focused engine/generated tests for callback arity, no pre-agreement mutation, exact-envelope preservation, matching replay, and irrelevant replay.
- Gate: targeted `agm_engine_test` and `generated_agm_recovery_test`, then `make test`.

### Plan 05-02 — VERD-01 Equivalence and VERD-02 Withholding

- Add the `-DTEST` checkpoint clause and reusable worker/result/`DOWN` helpers.
- Add complete-versus-recovered `yes`, `no`, continued no-verdict, exact recovery, and multi-candidate/single-consequence cases.
- Strengthen all four withholding cases to assert internal reason, unchanged ETS, normal termination, and zero verdicts after termination.
- Gate: repeated targeted suite execution without sleeps, then `make test`.

### Plan 05-03 — VERD-03 Irrevocability

- Add the complete/recovered × `yes`/`no` terminal matrix.
- For each path, cover an extension already queued behind the decisive event and an extension sent after the verdict.
- Represent later missing-event detail with the existing concrete trace envelope.
- Assert exact verdict multiset `[Expected]`, no duplicate/contradictory verdict, worker result, and `DOWN ... normal`.
- Gate: targeted suite and full `make test`.

### Plan 05-04 — Thesis Reconciliation and Milestone Gate

- Update statements that currently say lookahead is consumed without replay and equivalence/irrevocability remain unverified.
- Audit the same eight thesis sources recorded in `04.1-THESIS-AUDIT.md`, recording amended or verified-unchanged dispositions with final code/test evidence.
- Preserve all limitations still true: one explicit missing event, `send` lookahead only, bounded generated fragment, no tracer-side loss detection, public named ETS, and no exhaustive proof.
- Build the complete thesis and record final test totals.
- Stop after Phase 5 and milestone `v0.1`; do not enter Phase 6.

The thesis update follows implementation because its claims must cite final behavior and test evidence. [VERIFIED: Phase 5 D-19/D-20]

## State of the Art

| Old/current approach | Phase 5 approach | Impact |
|----------------------|------------------|--------|
| `handle_missing_event/1` commits inferred state before consequence agreement | Read-only recovery followed by post-agreement atomic commit | Withholding cannot leave speculative state behind. [VERIFIED: current code; D-11] |
| Consumed lookahead payload is used only for state inference | Preserve and directly dispatch the complete envelope | Recovered and complete executions process the same logical observed sequence. [VERIFIED: D-02/D-05] |
| Zero-arity continuation enters the post-missing monitor state | Envelope-aware callback dispatches lookahead into that state | Matching lookahead reduces once; irrelevant lookahead leaves the continuation waiting. [VERIFIED: D-03/D-04] |
| Continuation tests poll `process_info/2` with `timer:sleep/1` | Reference-tagged test-only checkpoint | No-verdict evidence is causal and deterministic. [VERIFIED: current test helper; D-07] |
| Tests await only `no` and check mailbox opportunistically | Symmetric verdict collector after normal termination | Detects duplicate and contradictory verdicts for both terminal values. [VERIFIED: current test helper; D-17] |

## Environment Availability

| Dependency | Required By | Available | Version | Fallback |
|------------|-------------|-----------|---------|----------|
| Erlang runtime/compiler | Generation and EUnit | ✓ | OTP 29 / ERTS 17.0.2 | — |
| GNU Make | Default regression gate | ✓ | 3.81 | Direct `erlc`/EUnit commands only for targeted diagnosis |
| `latexmk` | D-19 thesis build | ✓ | Installed at `/opt/local/bin/latexmk` | Existing thesis build procedure |
| External package manager | None | Not required | — | — |

No missing dependency blocks planning. [VERIFIED: local environment audit]

## Validation Architecture

### Test Framework

| Property | Value |
|----------|-------|
| Framework | EUnit bundled with Erlang/OTP 29 |
| Config file | `detecter/Makefile` |
| Quick run command | `cd detecter && make compile-test && erl -noshell -pa ebin -eval 'case eunit:test(generated_agm_recovery_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop` |
| Engine run command | `cd detecter && make compile-test && erl -noshell -pa ebin -eval 'case eunit:test(agm_engine_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop` |
| Full suite command | `cd detecter && make test` |

The default target currently runs 24 tracing, 5 parser, 3 smoke, 9 generated AGM, and 13 engine checks at the verified Phase 4.1 baseline. [VERIFIED: `04.1-VERIFICATION.md`]

### Phase Requirements → Test Map

| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|--------------|
| VERD-01 | Complete/recovered observable equivalence for `yes`, `no`, and continuing prefixes | Generated integration | targeted `generated_agm_recovery_test` command | ✅ extend existing file |
| VERD-02 | All ambiguity classes terminate normally without verdict or mutation | Generated integration + pure unit | targeted generated and engine commands | ✅ extend existing files |
| VERD-03 | One terminal verdict survives queued/post-verdict extensions on complete/recovered paths | Generated process integration | targeted `generated_agm_recovery_test` command | ✅ extend existing file |

### Sampling Rate

- **Per task commit:** targeted generated AGM suite; engine suite when callback types change.
- **Per wave merge:** `cd detecter && make test`.
- **Phase gate:** full suite green, deterministic claim-critical tests contain no `timer:sleep/1`, thesis build succeeds, and chapter audit is complete.

### Wave 0 Gaps

- [ ] Generated test-only checkpoint clause under `-DTEST`.
- [ ] Symmetric worker lifecycle helper using `spawn_monitor/1`.
- [ ] Exact verdict collector that checks both `yes` and `no`.
- [ ] Inline property/system-information fixtures for acceptance, rejection, irrelevant replay, and multi-candidate agreement.

## Security Domain

### Applicable ASVS Categories

| ASVS Category | Applies | Standard Control |
|---------------|---------|-----------------|
| V2 Authentication | no | No authenticated user boundary exists in this phase. [VERIFIED: phase scope] |
| V3 Session Management | no | Monitor session isolation is explicitly deferred. [VERIFIED: deferred ideas] |
| V4 Access Control | no | Tightening the public named ETS table is Phase 11, not Phase 5. [VERIFIED: ROADMAP.md] |
| V5 Input Validation | yes | Preserve exact tuple patterns and guards; unmatched lookahead must take the direct-dispatch fallback without dynamic evaluation. [VERIFIED: generated clause architecture] |
| V6 Cryptography | no | No cryptographic operation or secret is introduced. [VERIFIED: phase scope] |

### Known Threat Patterns for This Stack

| Pattern | STRIDE | Standard Mitigation |
|---------|--------|---------------------|
| Malformed or unrelated envelope changes monitor state | Tampering | Match the existing concrete trace envelope and update state only inside ordinary matching clauses. [VERIFIED: D-02/D-04] |
| Ambiguous recovery mutates globally visible ETS state | Tampering | Delay one atomic list insert until consequence agreement. [CITED: ETS documentation] |
| Test control message leaks into production protocol | Spoofing/Tampering | Generate checkpoint clauses only when the generator is compiled with `-DTEST`. [RECOMMENDED] |
| Public `sus_state` table collision | Tampering/DoS | Record as a retained limitation; do not redesign in Phase 5 because isolation is deferred to Phase 11. [VERIFIED: CONTEXT.md; ROADMAP.md] |

## Assumptions Log

| # | Claim | Section | Risk if Wrong |
|---|-------|---------|---------------|
| — | None. Recommendations are derived from locked decisions, inspected code/tests, thesis sources, and official Erlang/OTP documentation. | — | — |

## Open Questions (RESOLVED)

1. **Generated helper naming**
   - What is known: per-state direct dispatch is required; exact names are delegated to the planner/implementer. [VERIFIED: D-01/D-02 and Codex's discretion]
   - Recommendation: derive names deterministically from the existing generated state function atom and keep them private in production.
   - **Resolution:** Planner and implementer discretion; names must be deterministic and module-private.

2. **Terminal replay and ETS**
   - What is known: existing terminating generated branches skip `update_current_state/1` because the monitor immediately emits a verdict and returns. [VERIFIED: `maxhml_eval.erl`]
   - Recommendation: preserve ordinary terminal semantics. Commit the post-missing state, invoke the terminal callback, and do not add a Phase 5-only terminal ETS update; terminal process state is no longer observable behavior under D-05.
   - **Resolution:** Preserve ordinary terminal semantics exactly as recommended.

3. **Thesis test totals**
   - What is known: final totals will change when Phase 5 tests are added. [VERIFIED: planned test additions]
   - Recommendation: calculate and write totals only after the final full suite; do not predict them in a plan.
   - **Resolution:** Record measured totals only during Plan 05-03 verification and thesis reconciliation.

## Sources

### Primary (HIGH confidence)

- `.planning/phases/05-irrevocability-and-verdict-semantics/05-CONTEXT.md` — locked replay, equivalence, withholding, terminal, academic, and scope decisions.
- `.planning/REQUIREMENTS.md` — VERD-01 through VERD-03.
- `detecter/src/regeneration/agm_engine.erl` — pure recovery and consequence contracts.
- `detecter/src/synthesis/maxhml_agm_codegen.erl` — current premature ETS mutation and lost-lookahead orchestration.
- `detecter/src/synthesis/maxhml_eval.erl` and `detecter/src/synthesis/gen_eval.erl` — generated branch and envelope semantics.
- `detecter/test/regeneration/generated_agm_recovery_test.erl` — compilation fixture, worker harness, existing withholding tests, and sleep-based continuation polling.
- [Erlang/OTP 29 ETS documentation](https://www.erlang.org/docs/29/apps/stdlib/ets.html#insert-2) — list insertion atomicity and isolation.
- [Erlang process and signal documentation](https://www.erlang.org/doc/system/ref_man_processes.html) — same-sender ordering and process monitors.
- [Erlang receive documentation](https://www.erlang.org/doc/system/expressions.html#receive) — selective receive and unmatched-message behavior.

### Thesis Sources (HIGH confidence)

- `../Master-Thesis/Documentation/Thesis/frontmatter/abstract.tex`
- `../Master-Thesis/Documentation/Thesis/chap1/introduction_main.tex`
- `../Master-Thesis/Documentation/Thesis/chap2/background_runtime_verification.tex`
- `../Master-Thesis/Documentation/Thesis/chap3/methodology.tex`
- `../Master-Thesis/Documentation/Thesis/chap3/implementation.tex`
- `../Master-Thesis/Documentation/Thesis/chap4/results_and_discussion_main.tex`
- `.planning/phases/04.1-agm-engine-extraction-and-thesis-reconciliation/04.1-THESIS-AUDIT.md`

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH — directly observed in the local environment and repository.
- Architecture: HIGH — constrained by locked Phase 5 decisions and current generated code.
- Replay/ETS pattern: HIGH — derived from code flow and official OTP atomicity/receive semantics.
- Test design: HIGH — extends the existing EUnit harness using standard process-ordering and monitor primitives.
- Thesis scope: HIGH — explicit in CONTEXT.md and inspected thesis text.

**Research date:** 2026-07-02  
**Valid until:** 2026-08-01
