# Repeated Stanzas Audit

**Date:** 2026-05-29
**Trigger:** Refactor commit `1c762d0e` hoisted three identical `srcCtx` stanzas in `internal/match/syntax_expand.go` into `(*ExpandOptions).resolveSourceContext`. This audit sweeps the rest of the codebase for similar candidates.

## Scope

Looking for **short (3-10 line) code stanzas repeated 2+ times** that are candidates for hoisting into helpers. Excludes:
- Single repeated lines (variable decls, error returns)
- Patterns already abstracted (generic helpers in `registry/core`)
- Hand-unrolled dispatch that is *intentionally* duplicated for performance
- Test setup code

Search method: Explore subagent with very thorough breadth, focusing on `machine/`, `internal/match/`, `internal/syntax/`, `registry/core/`, `values/`, `werr/`. Confirmed by direct reading.

## Findings

### 1. `MakeClosure` stack-pop validation — recommend extracting

**Sites:**
- `machine/operations_closure.go:38-45` (`OperationMakeClosure.Apply`)
- `machine/machine_context.go:569-578` (`Run()` `OpMakeClosure` case)

**Stanza:**

```go
compiletimeEnv, ok := mc.evals.Pop().(*environment.EnvironmentFrame)
if !ok {
    return mc, werr.WrapForeignErrorf(werr.ErrNotALocalEnvironmentFrame,
        "MakeClosure: expected environment frame on stack")
}
tpl, ok := mc.evals.Pop().(*NativeTemplate)
if !ok {
    return mc, werr.WrapForeignErrorf(werr.ErrNotAMachineTemplate,
        "MakeClosure: expected native template on stack")
}
```

**Why this exists:** `MakeClosure` is one of the 16 complex ops promoted to inline in `Run()` (per `machine/CLAUDE.md`). Both the inline path and the `Apply` method path do the same two-pop validation because the side-table dispatch falls back to `Apply` when `OpComplex` is hit.

**Assessment:** Real duplication. Same nouns, same verbs, identical error messages. Not a hot-arithmetic path — `MakeClosure` runs once per closure creation, dominated by environment allocation work below.

**Suggested helper** (in `operations_closure.go`):

```go
func popMakeClosureArgs(mc *MachineContext) (*environment.EnvironmentFrame, *NativeTemplate, error) {
    env, ok := mc.evals.Pop().(*environment.EnvironmentFrame)
    if !ok {
        return nil, nil, werr.WrapForeignErrorf(werr.ErrNotALocalEnvironmentFrame,
            "MakeClosure: expected environment frame on stack")
    }
    tpl, ok := mc.evals.Pop().(*NativeTemplate)
    if !ok {
        return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAMachineTemplate,
            "MakeClosure: expected native template on stack")
    }
    return env, tpl, nil
}
```

**Confidence:** High. Low perf risk. Recommend doing this one.

---

### 2. `inlineCar` / `inlineCdr` — DO NOT extract without benchmarks

**Sites:**
- `machine/call_promoted.go:136-149` (`inlineCar`)
- `machine/call_promoted.go:153-165` (`inlineCdr`)

**Stanza:**

```go
o := mc.evals.Pop()
mc.counters.StackDrains++
mc.counters.StackElementsDrained++
mc.counters.ForeignCalls++

p, ok := o.(values.Tuple)
if !ok || values.IsEmptyList(o) {
    return applyCallableError(mc, werr.WrapForeignErrorf(
        werr.ErrNotAPair, "<name>: expected pair, got %s", o.SchemeString()))
}
mc.SetValue(p.<Car|Cdr>())
return nil
```

**Why this exists:** Promoted opcodes for `car` and `cdr`, dispatched directly from the `Run()` switch. Hot path.

**Why NOT to extract:** `memory/promoted-ops-table-revert.md` documents that converting promoted-op dispatch to a table caused a 1.5% regression. Any extraction here that introduces an `accessor func(values.Tuple) values.Value` parameter has the same risk: Go does not reliably inline through function values, and `values.Tuple.Car` as a method value would allocate a small closure on the call path. Per `CLAUDE.md`: *"Go switch beats table dispatch for hot-path loops."*

**Assessment:** Cosmetically duplicated but semantically a deliberately-unrolled hot path. The CLAUDE.md memo explicitly accepts this kind of duplication.

**If you still want to do it:** Benchmark first with `make bench-gabriel` against master. The refactor is rejected unless the benchmark delta is within noise.

**Confidence:** Medium that this is real duplication, high that extracting it without measurement is wrong.

---

## Other patterns examined and rejected

| Pattern | Why not to refactor |
|---------|---------------------|
| `RequireArg` / `ExtractInteger` calls in `registry/core/prim_*.go` | Already abstracted via generic helpers (`helpers.RequireArg[T]`, `helpers.ExtractInteger`). The repetition that remains is *data*, not *structure*. |
| `MakeTypePredicate` invocations in `registry/core` | Already factored into `helpers.MakeTypePredicate`. |
| Counter-update sequences in `call_promoted.go` (9 sites) | Context-specific arities (1 vs 2 pops). A helper would obscure intent without saving lines. |
| 65-case `switch` on `Op` in `Run()` | Deliberate hand-unrolled jump table; Go's compiler generates better dispatch from a switch than from any table-driven alternative. |
| `WrapForeignErrorf` call patterns | `WrapForeignErrorf` *is* the helper. The variation in error messages is content, not boilerplate. |
| `srcCtx` variants in `applyHygieneToSymbol`, `combineEllipsisResults`, `capturedValueToSyntax` (the ones we *didn't* touch in the original refactor) | Differ semantically: nil-base handling, or starting from `opts.UseSiteCtx` rather than a template node. Forcing them through one helper would bloat its API with flags. |

## Methodology notes

The agent's first-pass scan went broad across `machine/`, `internal/match/`, `internal/syntax/`, `registry/core/`, `values/`, `werr/`, `extensions/`. The cap was 12 findings; it returned 2. That low count is informative:

- The codebase already uses generic helpers (`helpers.RequireArg[T]`, `helpers.MakeTypePredicate`) for the most common shapes.
- Many surface-level duplications are **intentional unrolling for performance** (promoted ops, switch dispatch) — these are documented in `memory/` and `CLAUDE.md`.
- The macro/hygiene layer (`internal/match/`, `machine/expander_*.go`) was where the original finding lived; the rest of that layer was already inspected during the original refactor and is clean apart from the noted variants.

## Recommendation

Take finding #1. Skip finding #2 unless benchmarks justify it. No other action warranted at this time.
