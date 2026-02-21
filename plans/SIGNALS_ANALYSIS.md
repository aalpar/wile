# Wile Codebase — Signals Engineering Analysis

**Date:** 2026-02-17
**Type:** Full codebase review from a signals engineering perspective

## System Dynamics Diagram

```
┌──────────────────────────────────────────────────────────────────────┐
│                          Engine (Public API)                         │
│  NewEngine() ───► Eval/Compile/Run ───► Close()                     │
│       │              │                      │                        │
│  [init: bootstrap    │  ⚠ no closed         │  closers[]             │
│   macros, registry,  │    guard on           │  p.closed=true        │
│   LibraryEnvFactory] │    Eval/Run/Call      │  (no mutex)           │
└──────────┬───────────┴──────────┬────────────┴───────────────────────┘
           │                      │
           ▼                      ▼
┌───────────────────┐  ┌──────────────────────────────────────────────┐
│  CompileTime      │  │              MachineContext.Run()             │
│  Continuation     │  │  ┌─────────────────────────────────────┐     │
│  (compiler)       │  │  │  for pc < len(ops) {                │     │
│                   │  │  │    if ops&1023==0 → check ctx.Done()│     │
│  ExpanderTime     │  │  │    ops[pc].Apply(mc) ───────────────┤     │
│  Continuation     │  │  │  }                                  │     │
│  (expander)       │  │  └──────────────┬──────────────────────┘     │
│                   │  │                 │                             │
│  ⚠ no expansion  │  │     ┌───────────┴──────────────┐              │
│    depth limit    │  │     ▼                          ▼              │
│                   │  │  errHalt ──► nil          err ──► propagate   │
└───────────────────┘  └──────────────────────────────────────────────┘
                                     │
          ┌──────────────────────────┤
          ▼                          ▼
┌──────────────────┐    ┌──────────────────────────────────────┐
│  ForeignFunction │    │  RunWithEscapeHandling()              │
│  Call            │    │  ┌────────────────────────────────┐   │
│                  │    │  │  for {                         │   │
│  defer recover() │    │  │    Run()                       │   │
│  err check:      │    │  │    if nil → unwind → done      │   │
│  1. escape       │    │  │    if escape → RestoreWinding   │──── ⚠ unbounded loop
│  2. abort        │    │  │    if abort → FindPrompt        │   │
│  3. exception    │    │  │    else → return err            │   │
│  4. → SchemeExc  │    │  │  }                             │   │
└──────────────────┘    │  └────────────────────────────────┘   │
                        └──────────────────────────────────────┘
          │
          ▼
┌──────────────────────────────────────────────────────────────┐
│                    Shared Global State                        │
│                                                              │
│  stringInterns (sync.Map)     ◄── ⚠ unbounded growth         │
│  LibraryEnvFactory (var func) ◄── ⚠ race: multi-engine       │
│  stackPool (sync.Pool)        ◄── shared across engines      │
│  subContextPool (sync.Pool)   ◄── shared across engines      │
│  continuationPool (sync.Pool) ◄── shared across engines      │
│  nextWindingID (atomic)       ◄── benign sharing             │
└──────────────────────────────────────────────────────────────┘
```

---

## Findings

### F1. LibraryEnvFactory: Package-Global Data Race — FIXED

**Lens**: Cross-talk / Temporal Coupling
**Where**: Previously `engine.go` (package-level global); now `environment/top_level_environment.go` (field on `TopLevelEnvironment`)
**Status**: **FIXED.** `LibraryEnvFactory` is now a field on `TopLevelEnvironment` (set via `SetLibraryEnvFactory`, read via `LibraryEnvFactory()`), not a package-level global. Each engine has its own `TopLevelEnvironment` with its own factory closure. The data race described here no longer exists.

The factory is set in `engine.go:182` during engine construction and read in `machine/library_loader.go:126` during library loading. Both access the per-engine `TopLevelEnvironment` instance.

---

### F2. String Intern Cache: Unbounded Monotonic Growth

**Lens**: Saturation
**Where**: `values/string.go:33`
**Theory**: **Little's Law** (L = λW). The intern cache is a queue with infinite service time (entries are never removed). L grows without bound: L(t) = λ × t, where λ is the rate of unique string creation. This is a monotonically growing system with no equilibrium — a system whose queue depth increases linearly with time has no steady state.

**Dynamics**: Every distinct string ≤64 bytes is interned forever in a process-global `sync.Map`. For a long-running REPL or server embedding Wile, user-generated strings accumulate permanently. Since `sync.Map` doesn't support eviction or weak references, this is a **slow memory leak** with rate proportional to unique string throughput. A process that evaluates 1M distinct small strings accumulates ~64MB of interned strings that can never be reclaimed.

Multiple Engine instances share this cache — one engine's workload grows another engine's memory footprint. This is cross-talk through a shared resource.

**Severity**:
- Steady-state impact: Low for short-lived processes; high for long-running embeds
- Transient impact: None
- Overload impact: Memory exhaustion curve is linear, not cliff — the system degrades gradually

**Proposed direction**: For Wile's stated use case (scripting, config, policy), this is likely acceptable. If long-running embeds become a product concern, consider: (1) per-engine intern maps instead of global, (2) a bounded LRU cache with configurable size, or (3) `sync.Map` with periodic sweep of unreferenced entries. Option (1) alone removes the cross-engine leak without adding complexity.

---

### F3. Engine Use-After-Close: No Guard on Public API

**Lens**: Mode Transition
**Where**: `engine.go:641` (Close), `engine.go:234` (Eval), `engine.go:295` (Compile), `engine.go:319` (Run)
**Theory**: **Metastable state** (Bronson et al., "Metastable Failures in Distributed Systems," HotOS 2021). After `Close()`, the engine is in a state that _looks_ valid (all fields still set, no nil pointers) but produces undefined behavior. This is a half-state — the system has transitioned to "closed" but external interfaces don't enforce the new state. In reliability terms, MTTR = ∞ because the system never recovers from the undefined state — it just produces wrong results.

**Dynamics**: An embedder calls `engine.Close()`, then later (perhaps from another goroutine, or due to a logic error) calls `engine.Eval()`. The `closed` field is `true`, but `Eval` doesn't check it. The call proceeds using the closed engine's environment. If `Close()` freed resources in extension closers, the eval may access freed state or produce incorrect results with no error signal.

**Severity**:
- Steady-state impact: None (nobody calls Eval after Close intentionally)
- Transient impact: High — no error signal when the invariant is violated
- Overload impact: N/A

**Proposed direction**: Add `if p.closed { return nil, ErrEngineClosed }` at the top of `Eval`, `Compile`, `Run`, `Call`, `Define`, `Get`. This is a ~6-line change that converts a silent corruption into a clear error. Cost: one branch per API call (negligible).

---

### F4. RunWithEscapeHandling: Unbounded Retry Loop

**Lens**: Feedback Loop
**Where**: `machine/machine_context.go:1228+` (`RunWithEscapeHandling`)
**Theory**: **Positive feedback loop without gain limiting**. The loop's transfer function: escape → restore → Run() → escape → restore → ... The loop gain is determined by whether the restored continuation produces another escape. If gain ≥ 1 (each run produces at least one escape), the loop never terminates. This violates the **Nyquist stability criterion** — the loop gain must be < 1 at the frequency where phase = -180° for the system to converge.

In practice, each restoration makes forward progress (the continuation advances), so the loop gain is < 1 for well-formed programs. But a pathological program with mutually-invoking continuations could create gain = 1.

**Dynamics**: The `for { }` loop has no iteration bound. Normal programs converge because each escape restores a distinct continuation that runs to completion. But if a program captures continuations in a pattern where invocation A escapes to B which escapes back to A, the loop oscillates without terminating. The only damping mechanism is `ctx.Done()` (checked every 1024 ops inside `Run()`), which requires the caller to set a context deadline.

**Severity**:
- Steady-state impact: None for typical Scheme programs
- Transient impact: Infinite loop for adversarial programs without context deadline
- Overload impact: CPU saturation on one goroutine

**Proposed direction**: The context cancellation mechanism provides adequate protection when used correctly. The real risk is embedding without context deadlines. Consider: (1) documenting that `RunWithEscapeHandling` requires a context with a deadline for untrusted code, or (2) adding an escape iteration counter with a configurable bound (defaulting to unlimited for backward compatibility).

---

### F5. Macro Expansion: No Depth Limit

**Lens**: Feedback Loop / Saturation
**Where**: `machine/expander_time_continuation.go` (recursive expansion)
**Theory**: **Unbounded recursion** is the simplest positive feedback loop — output feeds directly back as input with no attenuation. A macro that expands to another macro invocation creates a recursive loop. Unlike call depth (which has configurable `maxCallDepth`), expansion depth has no dedicated limit.

**Dynamics**: A macro like `(define-syntax loop (syntax-rules () ((loop) (loop))))` followed by `(loop)` will expand indefinitely. The expansion creates sub-VMs to run transformers, which do consume call depth — but the expansion loop itself (in the expander, not the VM) has no bound. Context cancellation applies if the transformer runs in the VM, but the expander's dispatch loop doesn't check `ctx.Done()` between expansions.

**Severity**:
- Steady-state impact: None for correct macros
- Transient impact: Stack overflow or OOM from infinite expansion
- Overload impact: Single evaluation hangs

**Proposed direction**: Add an expansion depth counter to `ExpanderTimeContinuation`, with a configurable limit (default generous, e.g., 1000). This mirrors the existing `maxCallDepth` pattern. The cost is one increment per expansion.

---

### F6. sync.Pool Drain Under GC Pressure

**Lens**: Mode Transition / Saturation
**Where**: `machine/pool.go`
**Theory**: **GC-induced transient** (Åström & Murray, *Feedback Systems*, Ch. 1). Go's `sync.Pool` drains all cached objects on every GC cycle. This creates a periodic transient: after GC, the next burst of `SaveContinuation` / `NewSubContext` calls allocate fresh objects instead of reusing pooled ones. The transient is **inrush current** — a sudden spike in allocation rate after a GC event.

The system's response is overdamped: allocation spikes, GC eventually reclaims the new objects, pools refill, allocation rate returns to normal. But under sustained high load, the GC cycle itself is triggered more frequently (because allocations are higher), creating a feedback loop: more GC → less pooling → more allocation → more GC. This is a negative feedback loop (higher allocation triggers GC which reclaims), but its settling time depends on the `GOGC` setting.

**Dynamics**: Under typical Wile workloads (scripting, config), this is irrelevant — GC pressure is low and pool drain has negligible impact. Under benchmark/stress conditions, the pattern creates measurement noise: benchmark results depend on when GC fires relative to the measurement window.

**Severity**:
- Steady-state impact: None for typical workloads
- Transient impact: Allocation spikes after GC (microseconds to milliseconds)
- Overload impact: Potential for GC-allocation oscillation under sustained high load

**Proposed direction**: If benchmarking stability matters, consider `debug.SetGCPercent(-1)` in benchmarks to disable GC-induced pool drain (already a Go benchmarking best practice).

---

### F7. Thread-Engine Shared State Without Isolation

**Lens**: Cross-talk
**Where**: `machine/machine_context.go:767` (NewSubContext), `environment/load_path_stack.go`
**Theory**: **Noisy neighbor effect** (Gunther, *Guerrilla Capacity Planning*, Ch. 6). Threads (SRFI-18) share the global `TopLevelEnvironment` and its `LoadPathStack`. The LoadPathStack is documented as non-thread-safe for LIFO ordering under concurrent loads. More broadly, `GlobalEnvironmentFrame` bindings are shared-mutable state between threads — a `(define x 5)` in one thread is visible to another.

The **Universal Scalability Law** predicts that shared mutable state creates a coherence cost β > 0. As thread count N increases, throughput grows as N / (1 + α(N-1) + β·N·(N-1)). With β > 0, there exists a peak N beyond which adding threads decreases throughput.

**Dynamics**: Two SRFI-18 threads both call `(load "file.scm")` concurrently. Thread A pushes its file path, Thread B pushes its file path. Thread A's include resolution now sees Thread B's directory at the top of the stack. The result: file loading resolves relative paths using the wrong directory.

For mutable globals: Thread A evaluates `(define x 1)`, Thread B evaluates `(define x 2)`. The final value of `x` is nondeterministic. This is documented behavior, not a bug — but it is a cross-talk surface.

**Severity**:
- Steady-state impact: None for single-threaded use
- Transient impact: Silent incorrect behavior under concurrent file loading
- Overload impact: Global binding contention scales with thread count

**Proposed direction**: The LoadPathStack issue is already documented. For global environment mutations, this is inherent to the Scheme model (shared heap). Per the codebase documentation: "SRFI-18 threading is not yet complete enough to justify the complexity" of per-thread isolation. When threading matures, consider: (1) per-thread LoadPathStack, (2) read-write locks on GlobalEnvironmentFrame for define/set! operations.

---

### F8. PopContinuation Dual callDepth Strategy

**Lens**: Signal Integrity
**Where**: `machine/machine_context.go:278` (PopContinuation), `machine/machine_context.go:240` (RestoreAndRelease)
**Theory**: **Signal distortion** from inconsistent encoding. Two code paths maintain `callDepth` using different strategies: `RestoreAndRelease` reads the cached value from the continuation (`p.callDepth = cont.callDepth`), while `PopContinuation` decrements directly (`p.callDepth--`). These should produce the same result in the normal case, but they encode different invariants:
- `RestoreAndRelease`: callDepth = the restored continuation's ancestor count (absolute)
- `PopContinuation`: callDepth = previous depth - 1 (relative)

**Dynamics**: Under normal operation, both strategies produce identical results because callDepth is always one more than the continuation's cached depth. But if a bug causes callDepth to drift from the continuation chain length (e.g., through an unreachable but hypothetical code path), the two strategies would diverge silently. `PopContinuation` would propagate the error (relative adjustment), while `RestoreAndRelease` would correct it (absolute reset).

**Severity**:
- Steady-state impact: None — the invariant holds in practice
- Transient impact: None observed
- Overload impact: None

**Proposed direction**: This is a minor signal integrity concern, not a bug. The dual strategy is intentional — `PopContinuation` exists for a different code path (see the code comments distinguishing it from `Restore`). No action needed, but worth noting in the architecture: absolute reset (from continuation) is more resilient than relative adjustment.

---

## Risk Summary

| # | Finding | P(trigger) × Blast Radius | Priority |
|---|---------|---------------------------|----------|
| F1 | LibraryEnvFactory data race | ~~Medium × High~~ **FIXED** (now per-engine) | ~~1~~ |
| F3 | Use-after-Close no guard | Low × Medium (undefined behavior) | **2** |
| F2 | String intern unbounded growth | High (always) × Low (slow leak) | **3** |
| F5 | Macro expansion no depth limit | Low × Medium (hang/OOM) | **4** |
| F4 | RunWithEscapeHandling unbounded | Very Low × Medium (CPU spin) | **5** |
| F7 | Thread shared state | Low × Medium (incorrect results) | **6** |
| F6 | sync.Pool GC drain | Low × Low (performance noise) | **7** |
| F8 | callDepth dual strategy | Near-zero × Low (drift resilience) | **8** |

---

## Closing

### 1. Stability Assessment

**Conditionally stable.** The system is stable under single-engine, single-threaded, context-deadline-equipped operation — which covers Wile's primary use case. The conditions that push toward instability are:

- **Multiple concurrent engines** (F1): data race on `LibraryEnvFactory`
- **Untrusted code without context deadlines** (F4, F5): unbounded loops
- **Long-running processes with diverse string input** (F2): memory growth

The stability margins are generous for the intended workload (config, scripting, policy). They narrow under multi-engine embedding or adversarial input.

### 2. Weakest Transition

**Engine lifecycle: Close → post-Close API calls (F3).** The transition from "open" to "closed" is one-directional and irreversible, but the closed state is not enforced at the API boundary. This creates a metastable half-state where the engine appears functional but may produce undefined behavior. Unlike most other findings, this one has zero observability — no error, no panic, no signal that something is wrong.

### 3. Top 3 Dynamic Risks

1. **LibraryEnvFactory race (F1)**: **FIXED** — factory is now a field on `TopLevelEnvironment`, per-engine.

2. **Use-after-Close (F3)**: Add `closed` guard to public API methods. _Why_: Six lines of code convert undefined behavior into a clear error. The cost-benefit ratio is extreme.

3. **String intern growth (F2)**: Move to per-engine intern maps. _Why_: Eliminates cross-engine memory leak and removes a process-global shared resource. Aligns with the embedding vision where each engine is isolated.
