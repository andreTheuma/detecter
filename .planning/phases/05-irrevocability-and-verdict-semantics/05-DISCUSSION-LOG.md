# Phase 5: Irrevocability and Verdict Semantics - Discussion Log

> **Audit trail only.** Do not use as input to planning, research, or execution agents.
> Decisions are captured in CONTEXT.md; this log preserves the alternatives considered.

**Date:** 2026-07-02
**Phase:** 05-irrevocability-and-verdict-semantics
**Areas discussed:** Lookahead replay, equivalence boundary, withholding lifecycle, terminal semantics

---

## Lookahead Replay

| Decision | Alternatives considered | Selected |
|----------|-------------------------|----------|
| Replay owner | Generated monitor; outer driver; buffering proxy | Generated monitor |
| Dispatch method | Direct ordered dispatch; mailbox resend; payload reconstruction | Direct ordered dispatch |
| State progression | Normal two-step update; pre-advance and skip; engine mutation | Normal two-step update |
| Irrelevant lookahead | Ordinary monitor semantics; withhold; unconditional drop | Ordinary monitor semantics |

**User's choice:** The generated monitor synchronously replays the original lookahead envelope. Recovery records the state after the missing event, and normal event reduction updates through the lookahead once.

## Equivalence Boundary

| Decision | Alternatives considered | Selected |
|----------|-------------------------|----------|
| Equivalence meaning | Observable behavior; terminal verdict only; identical internals | Observable behavior |
| Observation points | Prefix-aware and final; final only; exhaustive proof | Prefix-aware and final |
| No-verdict evidence | Deterministic synchronization; timeout; omit check | Deterministic synchronization |
| Coverage | Bounded representative matrix; rejection only; exhaustive fragment | Bounded representative matrix |

**User's choice:** Compare complete and recovered executions at corresponding prefixes and over a common suffix, covering `yes`, `no`, continued monitoring, exact recovery, and several events with one consequence.

## Withholding Lifecycle

| Decision | Alternatives considered | Selected |
|----------|-------------------------|----------|
| Production behavior | Normal termination without verdict; third verdict; later disambiguation | Normal termination without verdict |
| Crash distinction | Normal exit plus no verdict; abnormal exit; any silence | Normal exit plus no verdict |
| ETS mutation | Commit after agreement; retain partial state; rollback | Commit after agreement |
| Diagnostic detail | Stable reason atoms; plain withhold; full internals | Stable reason atoms |

**User's choice:** Withholding is a normal non-verdict termination. It preserves stable internal reasons and leaves recovery state uncommitted when consequence agreement fails.

## Terminal Semantics

| Decision | Alternatives considered | Selected |
|----------|-------------------------|----------|
| Terminal implementation | Emit once and exit; absorbing loop; continue silently | Emit once and exit |
| Extensions | Queued and post-verdict; post-verdict only; exhaustive proof | Queued and post-verdict |
| Recovered detail | Ordinary trace event; new protocol; internal call | Ordinary trace event |
| Verdict coverage | Both verdicts and paths; rejection only; unit functions only | Both verdicts and paths |

**User's choice:** Complete and recovered generated monitors must emit exactly one `yes` or `no`, terminate normally, and never produce a second or contradictory verdict after queued or later trace events.

## Codex's Discretion

- Replay helper names and AST organization.
- Deterministic test-harness helper design.
- Fixture placement within the existing generated-monitor test structure.

## Deferred Ideas

- Buffering proxy and generalized event broker.
- Further-event disambiguation after withholding.
- New recovered-event message protocol.
- Absorbing terminal loops.
- All Phase 6 through 22 work, retained as future milestones.
