# Subsystem Simplification Plan

## Context

A codebase-wide architectural review identified 8 simplification opportunities across the core packages (`values/`, `machine/`, `environment/`, `internal/syntax/`). This plan organizes them into independent phases ordered by impact and risk.

The dependency graph is clean (no cycles) and the package layering is sound. The findings are about internal duplication, state tightness, and composability — not structural defects.

```
                              values/              internal/forms/
                                │                       │
                         internal/syntax/               │
                           │         │                  │
                  ┌────────┤    internal/tokenizer/     │
                  │        │         │                  │
             environment/  │   internal/parser/         │
                  │        │         │                  │
                  │   internal/match/│  internal/schemeutil/
                  │        │         │         │        │
                  │   internal/validate/───────┘        │
                  │        │         │                  │
                  └────────┴─────────┴──────────────────┘
                                     │
                                 machine/  (8 internal imports)
                                     │
                        ┌────────────┤
                        │            │
                    registry/    runtime/
                        │
                   registry/core
                   registry/helpers
```

## Phase 1: Unified Port Base (Low Risk, High Impact) ✅ DONE

### Problem

10 concrete port types across 3 structural families independently implement identical close/guard logic:

```
┌──────────────────────────────────────────────────────────────┐
│                 DUPLICATED ACROSS ALL 10 TYPES               │
├──────────────────────────────────────────────────────────────┤
│  closed bool field                                           │
│  IsClosed() bool  →  return p.closed                         │
│  Close() error    →  defer { p.closed = true }; maybe clsr   │
│  if p.closed { return ErrPortClosed } guard on every method  │
│  SchemeString() / IsVoid() / EqualTo() boilerplate           │
└──────────────────────────────────────────────────────────────┘
```

Three structural families:

| Family | Backing | Types | Fields |
|--------|---------|-------|--------|
| A (buffered reader) | `*bufio.Reader` + `io.Closer` | `BinaryInputPort`, `ByteVectorInputPort`, `CharacterInputPort` | `rdr`, `clsr`, `closed` |
| B (buffered writer) | `*bufio.Writer` + `io.Closer` | `BinaryOutputPort`, `ByteVectorOutputPort`, `CharacterOutputPort` | `wrt`, `clsr`, `closed` |
| C (bytes.Buffer) | `*bytes.Buffer` | `StringInputPort`, `StringOutputPort`, `ByteVectorBufferdOutputPort`, `ByteVectorInputOutputPort` | `buf`, `closed` |

### Design

Extract a shared embedded struct for close-state management:

```go
// portBase tracks the closed state of a port and optionally holds
// an io.Closer for the underlying stream.
type portBase struct {
    closed bool
    clsr   io.Closer
}

func (b *portBase) IsClosed() bool { return b.closed }

func (b *portBase) Close() error {
    if b.closed {
        return nil
    }
    b.closed = true
    if b.clsr != nil {
        return b.clsr.Close()
    }
    return nil
}

func (b *portBase) guardClosed() error {
    if b.closed {
        return WrapForeignErrorf(ErrPortClosed, "port is closed")
    }
    return nil
}
```

Each concrete type embeds `portBase` and uses `guardClosed()` at the top of every I/O method. Family C ports construct `portBase` with `clsr: nil`.

### Files Modified

| File | Changes |
|------|---------|
| `values/port_base.go` | New file: `portBase` struct + methods |
| `values/binary_input_port.go` | Embed `portBase`, remove `closed`/`clsr` fields, remove `Close`/`IsClosed`, replace guards |
| `values/binary_output_port.go` | Same pattern |
| `values/byte_vector_input_port.go` | Same pattern |
| `values/byte_vector_output_port.go` | Same pattern |
| `values/byte_vector_buffered_output_port.go` | Same pattern (clsr=nil) |
| `values/byte_vector_input_output_port.go` | Same pattern (clsr=nil) |
| `values/character_input_port.go` | Same pattern |
| `values/character_output_port.go` | Same pattern |
| `values/string_input_port.go` | Same pattern (clsr=nil) |
| `values/string_output_port.go` | Same pattern (clsr=nil) |

### Verification

```bash
go test ./values/...       # All port tests pass
make lint                  # No new issues
```

### Impact

- ~200 lines of duplicated close/guard boilerplate eliminated
- Close-state invariant centralized in one place
- Future port types get close semantics for free
- Zero behavioral change

---

## Phase 2: AsList() Deduplication (Low Risk, Moderate Impact) ✅ DONE

### Problem

Three implementations of `AsList()` share character-for-character identical core logic (switch on length 0/1, forward-build with placeholder cons cells):

| Implementation | Location | Input Type |
|---------------|----------|------------|
| `Vector.AsList()` | `values/vector.go:108` | `[]Value` |
| `ByteVector.AsList()` | `values/byte_vector.go:91` | `[]*Byte` |
| `values.List()` | `values/utils.go:25` | `...Value` |

`Operations.AsList()` (`machine/operations.go:25`) and `Stack.AsList()` (`machine/stack.go:56`) use a different algorithm (reverse iteration) but solve the same problem.

### Design

`Vector.AsList()` delegates to `values.List()` after the void guard:

```go
func (p *Vector) AsList() Tuple {
    if p.IsVoid() {
        return (*Pair)(nil)
    }
    return List((*p)...)
}
```

`ByteVector.AsList()` requires a conversion step since `[]*Byte` isn't `[]Value`:

```go
func (p *ByteVector) AsList() Tuple {
    if p.IsVoid() {
        return (*Pair)(nil)
    }
    vs := make([]Value, len(*p))
    for i, b := range *p {
        vs[i] = b
    }
    return List(vs...)
}
```

`Operations.AsList()` and `Stack.AsList()` are in a different package and use `Operation` (which satisfies `values.Value`), so they can also delegate after conversion.

### Files Modified

| File | Changes |
|------|---------|
| `values/vector.go` | Replace `AsList` body with delegation to `List()` |
| `values/byte_vector.go` | Replace `AsList` body with conversion + delegation |
| `machine/operations.go` | Consider delegation or leave as-is (different package, minor) |
| `machine/stack.go` | Consider delegation or leave as-is |

### Verification

```bash
go test ./values/... ./machine/...
make lint
```

### Impact

- Eliminates 2 copies of ~20-line list construction
- Single point of optimization/bug-fixing for list building

---

## Phase 3: ErrMachineHalt → nil at Run() Boundary (Medium Risk, High Impact) ✅ DONE

### Problem

`Run()` returns `ErrMachineHalt` when the VM exhausts its operations (normal completion). Every caller must write:

```go
if err != nil && !errors.Is(err, machine.ErrMachineHalt) {
    return err
}
```

This pattern repeats ~30 times across the codebase. `ErrMachineHalt` is a success value disguised as an error.

```
Run() return states: {nil, ErrMachineHalt, real-error}
Semantic states:     {success, error}
```

### Design

Translate `ErrMachineHalt` to `nil` at the `Run()` boundary. Keep the internal sentinel unexported for the VM loop break:

```go
// errHalt is the internal sentinel used to break the VM loop.
// It is never returned from Run() — Run() translates it to nil.
var errHalt = values.NewStaticError("machine halt")

func (p *MachineContext) Run() error {
    // ... existing loop ...
    for p.pc < len(p.template.operations) {
        // ...
    }
    // Normal completion: VM exhausted operations
    return nil
}
```

The existing `OperationRestoreContinuation` returns `errHalt` (unexported) when `mc.cont == nil`. `Run()` catches it and returns `nil`.

### Migration

1. Rename `ErrMachineHalt` to unexported `errHalt`
2. Update `Run()` to return `nil` on normal completion
3. Remove all `!errors.Is(err, ErrMachineHalt)` guards at call sites
4. Update `RunWithEscapeHandling` similarly
5. Update tests that check for `ErrMachineHalt`

### Call Sites to Update

| File | Pattern | Change |
|------|---------|--------|
| `engine.go:269,287,428` | `!errors.Is(err, ErrMachineHalt)` | Remove guard |
| `ffi.go:586,688` | `!errors.Is(err, ErrMachineHalt)` | Remove guard |
| `runtime/runtime.go:134` | `!errors.Is(err, ErrMachineHalt)` | Remove guard |
| `cmd/main.go:241` | `errors.Is(err2, ErrMachineHalt)` | Remove guard |
| `machine/compile_define_for_syntax.go:119` | `!errors.Is(err, ErrMachineHalt)` | Remove guard |
| `machine/compile_begin_for_syntax.go:83` | `!errors.Is(err, ErrMachineHalt)` | Remove guard |
| `machine/compile_time_continuation_test.go` (~20 sites) | `err != ErrMachineHalt` | Remove guard |
| `machine/machine_context.go` (internal) | Returns `ErrMachineHalt` | Returns `errHalt` |

### Verification

```bash
go test ./...              # All tests pass
make lint                  # No exported reference to ErrMachineHalt remains
grep -r 'ErrMachineHalt' . --include='*.go'  # Should find 0 matches
```

### Risk

Medium — requires careful audit of all `Run()` callers. Some callers may depend on the `ErrMachineHalt` vs `nil` distinction for control flow (unlikely but must verify). The `RunWithEscapeHandling` method has its own halt handling that must be coordinated.

### Impact

- ~30 guard clauses eliminated
- `Run()` contract becomes honest: nil = success, non-nil = error
- Test code simplified significantly

---

## Phase 4: syntax-case Globals → Per-Context State (Medium Risk, Medium Impact) ✅ DONE

### Problem

Three mutable package-level globals pass state between syntax-case operations:

```go
var currentSyntaxCaseBindings map[string]syntax.SyntaxValue  // line 42
var currentSyntaxCaseMatcher  *match.SyntaxMatcher           // line 46
var currentSyntaxCaseInput    syntax.SyntaxValue              // line 50
```

These are thread-unsafe. Concurrent macro expansion (SRFI-18 threads) would corrupt them.

### Design

Move the globals into a struct stored on `MachineContext`:

```go
type syntaxCaseState struct {
    bindings map[string]syntax.SyntaxValue
    matcher  *match.SyntaxMatcher
    input    syntax.SyntaxValue
}
```

Add to `MachineContext`:

```go
type MachineContext struct {
    // ... existing fields ...
    syntaxCase *syntaxCaseState  // nil when not in syntax-case expansion
}
```

Each `OperationStoreSyntaxCaseInput`, `OperationSyntaxCaseMatch`, `OperationBindPatternVars`, and `OperationSyntaxTemplateExpand` accesses `mc.syntaxCase` instead of the globals.

### Files Modified

| File | Changes |
|------|---------|
| `machine/machine_context.go` | Add `syntaxCase *syntaxCaseState` field |
| `machine/operation_syntax_case.go` | Replace 3 globals with `syntaxCaseState` struct + `ensureSyntaxCaseState()` helper; all operations use `mc.syntaxCase.*` |
| `machine/operation_syntax_case_test.go` | Update tests to use `mc.syntaxCase` instead of package globals |

### Implementation Notes

- `syntaxCaseState` struct defined in `operation_syntax_case.go` (co-located with the operations that use it)
- `ensureSyntaxCaseState(mc)` lazily allocates the state on first use (StoreSyntaxCaseInput)
- `ClearSyntaxCaseInput` sets `mc.syntaxCase = nil` (releases the entire state)
- `NewSubContext()` does NOT propagate `syntaxCase` — it's per-expansion, not per-thread
- Zero behavioral change: all tests pass unchanged

### Impact

- 3 mutable globals eliminated
- syntax-case becomes reentrant
- Enables future concurrent macro expansion
- State scope tightened from "process-global" to "per-VM-context"

---

## Phase 5: Environment Lookup Delegation (Low Risk, Moderate Impact) ✅ DONE

### Problem

`EnvironmentFrame` has 9 `Get*` methods that directly access `local.keys` and `global.keys` maps, reimplementing logic that `LocalEnvironmentFrame` and `GlobalEnvironmentFrame` already provide. Zero delegation.

There is also a semantic discrepancy: `GlobalEnvironmentFrame.GetGlobalIndex` interns the symbol before lookup; `EnvironmentFrame.GetGlobalIndex` does not.

### Investigation Results

1. **Depth tracking**: `EnvironmentFrame.GetLocalIndex` walks the parent chain with a depth counter `j`. `LocalEnvironmentFrame.GetLocalIndex` always returns depth 0. Delegation requires the walker to adjust depth: `NewLocalIndex(li[0], depth)`. Same applies to `MaybeCreateLocalBinding`.

2. **Interning discrepancy**: Confirmed latent bug. `EnvironmentFrame.GetGlobalIndex` returned `GlobalIndex.Index` pointing to an uninternmed symbol. Works today because all consumers use `*gi.Index` (value comparison), but violates the invariant that returned `GlobalIndex` values should hold interned symbols. Same gap existed in `CreateGlobalBinding` and `MaybeCreateOwnGlobalBinding`.

3. **GetIndex**: Zero production callers, documented bug (skips first frame). Deleted.

4. **GetBinding/GetBindingWithScopes/GetLocalIndexWithScopes**: Complex per-iteration scope matching logic (ScopesMatch, candidate accumulation, Flatt's maximization) makes delegation a net negative — it would push scope-awareness into the leaf types or require a predicate parameter, neither of which simplifies anything. Documented in comments.

### Changes Implemented

**Tier 1 — High value, safe:**
- Deleted `GetIndex` (dead code with documented bug, zero callers)
- Added `InternSymbol(key)` to `GetGlobalIndex`, `CreateGlobalBinding`, `MaybeCreateOwnGlobalBinding` — all three now intern the key before lookup/create, consistent with `GlobalEnvironmentFrame.CreateGlobalBinding` and `GlobalEnvironmentFrame.GetGlobalIndex`

**Tier 2 — Moderate value, delegation:**
- `GetLocalIndex`: delegates single-frame lookup to `LocalEnvironmentFrame.GetLocalIndex`, adjusts depth
- `MaybeCreateLocalBinding`: delegates single-frame lookup to `LocalEnvironmentFrame.GetLocalIndex`, creation to `LocalEnvironmentFrame.EnsureLocalBinding`

**Tier 3 — Low value, documented as intentionally non-delegated:**
- `GetBinding`: returns `*Binding` (no depth tracking needed), direct map access is equivalent
- `GetBindingWithScopes`: per-iteration scope matching prevents clean delegation
- `GetLocalIndexWithScopes`: cross-frame candidate maximization (Flatt model) prevents delegate-and-adjust pattern

### Files Modified

| File | Changes |
|------|---------|
| `environment/environment_frame.go` | Deleted `GetIndex`; added interning to `GetGlobalIndex`, `CreateGlobalBinding`, `MaybeCreateOwnGlobalBinding`; refactored `GetLocalIndex` and `MaybeCreateLocalBinding` to delegate; added doc comments to Tier 3 methods |

### Impact

- Dead code (`GetIndex`) eliminated
- Symbol interning gap closed — `GlobalIndex.Index` now always points to interned symbol
- `GetLocalIndex` and `MaybeCreateLocalBinding` have clear responsibility split: `EnvironmentFrame` owns the walk, `LocalEnvironmentFrame` owns the lookup
- Non-delegated methods documented with rationale

---

## Phase 6: VM State Struct Extraction ✅ DONE

### Problem

8 fields are declared in both `MachineContext` (19 fields total) and `MachineContinuation` (10 fields total):

```
┌──────────────────┬─────────────────┬──────────────────────┐
│ Field            │ MachineContext  │ MachineContinuation  │
├──────────────────┼─────────────────┼──────────────────────┤
│ env              │ yes             │ yes                  │
│ template         │ yes             │ yes                  │
│ value            │ yes             │ yes                  │
│ evals            │ yes             │ yes                  │
│ pc               │ yes             │ yes                  │
│ windingStack     │ yes             │ yes                  │
│ promptTag        │ yes             │ yes                  │
│ threadID         │ yes             │ yes                  │
└──────────────────┴─────────────────┴──────────────────────┘
```

### Investigation Results

The investigation reveals that **declaration overlap ≠ copy overlap**. The save/restore methods handle different subsets of these 8 fields:

```
┌──────────────┬────────────────┬─────────────┬──────────────────┐
│ Field        │ SaveCont saves │ Restore     │ PopContinuation  │
├──────────────┼────────────────┼─────────────┼──────────────────┤
│ env          │ ✓              │ ✓           │ ✓                │
│ template     │ ✓              │ ✓           │ ✓                │
│ value        │ ✓              │ ✗           │ ✓                │
│ evals        │ ✓              │ ✓ (Copy)    │ ✓ (no copy)      │
│ pc           │ ✓ (+offset)    │ ✓           │ ✓                │
│ threadID     │ ✓              │ ✗           │ ✗                │
│ windingStack │ ✗              │ ✗           │ ✗                │
│ promptTag    │ ✗              │ ✗           │ ✗                │
└──────────────┴────────────────┴─────────────┴──────────────────┘
```

Key findings:

1. **`value` is NOT restored by `Restore()`** — intentional. For `call/cc` re-invocation, the value register is set by the caller (the argument passed to the escape closure). `PopContinuation()` DOES restore `value` (normal function return semantics).

2. **`evals` deep-copy differs**: `Restore()` calls `evals.Copy()` (continuations can be re-invoked, so the stack must be isolated). `PopContinuation()` does direct assignment (single-use consumption).

3. **`threadID` is saved but never restored** — it's a context property. The continuation remembers which thread created it (for cross-thread continuation safety checks), but restoring a continuation doesn't change the current thread.

4. **`windingStack` and `promptTag` are NEVER touched** by `SaveContinuation`, `Restore`, or `PopContinuation`. They exist on `MachineContinuation` for separate lifecycle management (set directly during delimited continuation operations via `MachineContinuation.Copy()` and `DeepCopy()`).

### Revised Design

The save/restore asymmetry means a simple `p.vmState = cont.vmState` would be incorrect — it would restore fields that shouldn't be restored (`value` in `Restore`, `threadID` in both). The `vmState` extraction is still valuable for **structural documentation** (which fields are shared) but cannot simplify `Restore`/`PopContinuation` to a single assignment.

A more honest extraction:

```go
// vmState holds the execution state shared between MachineContext and
// MachineContinuation. Fields in this struct exist in both types but are
// NOT uniformly copied — see the per-method field tables in the doc comments
// for Restore, PopContinuation, and SaveContinuation.
type vmState struct {
    env          *environment.EnvironmentFrame
    template     *NativeTemplate
    value        MultipleValues
    evals        *Stack
    pc           int
    windingStack WindingStack
    promptTag    *PromptTag
    threadID     uint64
}
```

`Restore` and `PopContinuation` remain explicit about which fields they handle — the struct extraction doesn't collapse them to a single assignment, but it does:

- Make `MachineContinuation` a 3-field struct (`vmState` + `parent` + `promptHandler`)
- Make adding a new shared field impossible to forget in one type
- Document the shared-field set explicitly in one place

### Files Modified

| File | Changes |
|------|---------|
| `machine/vm_state.go` | New file: `vmState` struct |
| `machine/machine_context.go` | Embed `vmState`; `Restore`/`PopContinuation` remain explicit per-field |
| `machine/machine_continuation.go` | Embed `vmState`, simplify constructors and `Copy`/`DeepCopy` |

### Verification

```bash
go test ./machine/... ./registry/...
make lint
```

### Risk

Medium — embedding changes field access from `p.env` to `p.vmState.env` (though Go's embedding makes `p.env` still work). The main risk is accessor methods and tests that reference fields by name in struct literals — these will need updating to use the embedded form.

### Impact

- Shared-field set documented structurally (not just by convention)
- `MachineContinuation` reduced from 10 named fields to 3 (`vmState`, `parent`, `promptHandler`)
- New shared fields automatically appear in both types
- Does NOT simplify `Restore`/`PopContinuation` to single assignments (the per-field asymmetry is real and must be preserved)

---

## Phase 7: CompileTimeCallContext.env Audit (Low Risk, Low Impact) ✅ DONE

### Problem

`CompileTimeCallContext` carries `env *environment.EnvironmentFrame` as a field. `CompileTimeContinuation` (the receiver on all compile methods) also carries `env`. Every compilation call passes both, each containing `env`.

```
State space: 2 env fields × any value = N^2
Semantic space: 1 env value = N
```

### Investigation Required

Determine whether `CompileTimeCallContext.env` is ever different from `CompileTimeContinuation.env`. Search for call sites where a different env is passed:

```bash
grep -n 'NewCompileTimeCallContext' machine/*.go | grep -v test
```

If they always agree, remove `env` from `CompileTimeCallContext`. If they sometimes differ, document when and why.

### Investigation Result

`ctctx.env` is never read anywhere in the codebase — confirmed by grep. The field is written at construction and copied through `NotInTail()`/`NotInExpression()` but never consumed. It is pure dead state.

### Files Modified

| File | Changes |
|------|---------|
| `machine/compile_time_call_context.go` | Remove `env` field, remove `env` parameter from constructor, remove from copy methods |
| ~30 production + test files | Remove 4th argument from `NewCompileTimeCallContext()` calls |
| `machine/coverage_improvement_test.go` | Remove assertion on `newCtx.env` |
| `machine/compile_quasisyntax_test.go` | Replace `ccnt, env := newTestCompiler()` with `ccnt, _ :=` (5 sites) |

### Impact

- Dead field removed from a value-type struct passed by copy through the entire compiler
- Constructor signature simplified: `NewCompileTimeCallContext(ctx, inTail, inExpression)` — no more env parameter
- State space reduced from `ctx × env × inTail × inExpression` to `ctx × inTail × inExpression`

---

## Evaluation: Go 1.24 `unique.Handle` and `maphash.Comparable` for Symbols

Go 1.24 introduces two features that appear relevant to Wile's symbol interning and hashing. This section evaluates both against Wile's actual architecture.

### Current Architecture

```
Parser ──→ env.InternSymbol(NewSymbol(name)) ──→ TopLevelEnvironment
string->symbol ──→ env.InternSymbol(sym) ──────→ TopLevelEnvironment
                                                       │
                                            map[Symbol]*Symbol + sync.RWMutex
                                            (per-VM, with parent delegation)
```

Two comparison regimes coexist:

| Context | Comparison | Why |
|---------|-----------|-----|
| Internal (compiler, expander, match) | `.Key ==` (string) | Anticipates future string comparison optimization; works without interning |
| User-facing (`eq?`, environment lookup) | `*Symbol` pointer identity | R7RS §6.5 requires `(eq? 'foo 'foo) → #t` for interned symbols |

Per-VM interning is intentional: two `Engine` instances in the same Go process must have separate symbol identity spaces. This is a sandboxing property — symbols from one VM cannot be `eq?`-equal to symbols from another.

### `unique.Handle[string]` — Does Not Fit

`unique.Handle` (Go 1.24, `unique` package) provides process-global canonical interning with pointer-equality semantics and automatic GC cleanup via internal weak pointers.

**The fatal conflict is scope.** `unique.Make("foo")` returns the same `Handle` regardless of which goroutine, which VM, or which universe called it. This is process-global by design — the entire point is that equal strings always produce the same pointer.

```
unique.Handle:    process-global identity
                  unique.Make("foo") == unique.Make("foo")  ← always, everywhere

Wile Symbols:     per-VM identity
                  vm1.Intern("foo") != vm2.Intern("foo")   ← intentional isolation
```

If Wile adopted `unique.Handle[string]` as the interning substrate, symbol identity would leak across VM instances. A symbol created in a sandboxed VM would be `eq?`-equal to one in the host VM. This violates the isolation invariant.

**Could we use Handle internally but compare by VM-tagged wrapper?** Possible but backwards — we'd add complexity to recover an invariant we currently get for free. The current `map[Symbol]*Symbol` per `TopLevelEnvironment` is simple, correct, and has the right scope.

**What unique.Handle does well that we don't need:**

| Feature | unique.Handle | Wile's needs |
|---------|--------------|-------------|
| Automatic GC of unused entries | Yes (weak pointers) | Not needed — symbols are cheap, and a Scheme session's symbol table is small enough to hold forever |
| Lock-free concurrent reads | Yes (hash-trie) | RWMutex is adequate — interning happens at parse time and `string->symbol`, not in hot loops |
| String cloning (substring leak prevention) | Yes | Not relevant — symbol keys are standalone strings, not substrings of larger buffers |
| Process-global deduplication | Yes | **Unwanted** — violates per-VM isolation |

**Verdict: Do not adopt.** The per-VM isolation invariant is non-negotiable. `unique.Handle` is the wrong scope.

### `maphash.Comparable` — Partial Fit, Not Worth It

`maphash.Comparable[T]` (Go 1.24) exposes Go's runtime hash function (AES on amd64/arm64, Wyhash elsewhere) for any `comparable` type. It replaces the need for hand-written hash functions like FNV-1a.

**Where it could replace custom hashing:**

Wile's `Hashable` interface requires `HashCode() uint64` for Scheme hash table keys. Currently implemented with FNV-1a via `hashString`/`hashUint64` in `values/hash.go`. Types implementing `Hashable`: Integer, BigInteger, Float, Rational, Boolean, Character, Symbol, Byte, String.

`maphash.Comparable` could replace FNV-1a for these types — but with trade-offs:

**1. Process-local seeds break determinism.**

`maphash.Comparable` requires a `Seed` that produces process-unique hashes. Different process runs produce different hashes for the same value. This is fine for Go's built-in maps (which also randomize), and fine for Wile's `Hashtable` (which uses hashes only for bucket indexing at runtime). But it means:

- Hash values cannot appear in error messages or debugging output reproducibly
- If Wile ever serializes hash tables (e.g., for image-based persistence), stored hashes would be invalid on reload
- Test assertions on specific hash values would break across runs

The current FNV-1a produces deterministic hashes. This isn't load-bearing today, but it's a property we'd lose.

**2. Heap escape for non-string values.**

`maphash.Comparable` forces non-string arguments to escape to the heap (documented Go limitation). For `Integer{Value: 42}`, this means a heap allocation per hash call. The current `hashUint64` operates on a bare `uint64` with zero allocation. For hot paths (hash table lookups with integer keys), this adds GC pressure.

Strings are explicitly exempted from this escape — `abi.EscapeNonString` skips them — so symbol hashing (string-based) would be fine.

**3. The hash functions are 27 lines total.**

`values/hash.go` is 27 lines. Two functions. FNV-1a is well-understood, deterministic, and has no external dependency. Replacing it with `maphash.Comparable` would:

- Add a Go 1.24 minimum version requirement (currently Go 1.23)
- Require a `maphash.Seed` to be created and stored somewhere (per-Hashtable? global?)
- Not improve hash distribution in any way that matters for Wile's workloads (symbol tables and small hash tables, not millions-of-keys scenarios where FNV-1a's weaknesses matter)

**4. Doesn't help with `equal?`-based hash tables.**

Scheme's `equal?` operates on recursive structures (lists, vectors). Go's `comparable` constraint excludes these. Wile would still need custom hashing for any `equal?`-based hash table. `maphash.Comparable` only helps with `eq?`/`eqv?`-based tables — which are already the fast path.

**Verdict: Not worth adopting.** The benefit (delete 27 lines of hash code) doesn't justify the cost (Go 1.24 minimum, process-local seeds, heap escapes for non-strings, still need custom hashing for `equal?`). Revisit if/when Go 1.24 becomes the minimum version for other reasons.

### Future-Facing Notes

**If the `.Key ==` comparisons become a bottleneck**, the right optimization is not `unique.Handle` but rather trusting the existing interning invariant and switching internal comparisons from `.Key ==` to pointer `==`.

Wile already interns at parse boundaries via the two-gate design (see `plans/TOP_LEVEL_ENVIRONMENT.md` §Symbol Interning Gates):

```
Source text ──→ Parser.wrapSyntaxSymbol() ──→ env.InternSymbol() ──→ interned *Symbol
                     (gate 1)                                              │
                                                                    compiler, expander,
Runtime ──→ string->symbol primitive ──→ env.InternSymbol()         match — all receive
                  (gate 2)                                          already-interned symbols
```

Every symbol reaching the compiler, expander, and pattern matcher has already passed through one of these two gates. The internal `.Key ==` comparisons are a conservative choice — they work correctly regardless of whether the symbol is interned. The optimization path is:

1. **Close known gate violations** (see below) so the invariant holds universally
2. **Switch** internal comparisons from `.Key ==` to pointer `==` at sites shown in profiling
3. **Document** the stronger invariant: "all symbols in the system are interned; pointer comparison is sound"

This is a discipline change (trust the invariant), not an infrastructure change (new interning mechanism). It preserves per-VM isolation while getting O(1) comparison at every site.

**Known violations of the two-gate invariant** (symbols created via `values.NewSymbol` without interning):

| Location | What | Impact |
|----------|------|--------|
| ~~`values/thread.go` `StateSymbol()`~~ | ~~Returns fresh symbols each call~~ | **CLOSED** — now returns `SymbolThread*` singletons |
| ~~`values/mutex.go` `StateValue()`~~ | ~~Returns fresh symbols each call~~ | **CLOSED** — now returns `SymbolMutex*` singletons |
| ~~`internal/extensions/threads/prim_threads.go`~~ | ~~`NewSymbol("primordial")` each call~~ | **CLOSED** — now returns `values.SymbolPrimordial` |
| `ffi.go:858` | Struct field names as symbols in reflection | Used as dict keys; works via `EqualTo()` but lacks `eq?` identity |
| `value.go:89` public `NewSymbol()` | Embedding API returns uninterned symbols | May be intentional — Go callers may not have an env to intern against |
| `internal/syntax/syntax_symbol.go:49` | `NewSyntaxSymbol` creates uninterned underlying symbol | Acceptable if syntax values use `.Key ==` internally |

The thread/mutex state symbols are the most actionable — they should be package-level singletons (one `NewSymbol` at init, reused on every call). The public API (`value.go`) is a design question: should embedding callers be required to intern? The ffi and syntax cases are lower priority.

**If Go's minimum version advances to 1.24 for unrelated reasons**, reconsider `maphash.Comparable` specifically for `Symbol.HashCode()` (strings don't heap-escape). The other `Hashable` types (Integer, Byte, Boolean, Character) would still be better served by `hashUint64` to avoid heap allocation.

---

## Deferred: Numeric Tower Dispatch (Not Recommended)

The 332 type-switch branches across 7 numeric types are **intentional** architecture. The previous `Tower*` dispatch layer was removed because it added indirection without benefit. The N x N dispatch is the explicit design choice — directness over DRY.

Do not reintroduce a dispatch table. If the repetition becomes painful (e.g., adding an 8th numeric type), consider codegen from a specification table rather than runtime indirection.

---

## Phase 8: CreateGlobalBinding / MaybeCreateOwnGlobalBinding Deduplication ✅ DONE

### Problem

`EnvironmentFrame` has two methods with identical bodies:

```go
// environment/environment_frame.go:554-564
func (p *EnvironmentFrame) CreateGlobalBinding(key *values.Symbol, bt BindingType) (*GlobalIndex, bool)

// environment/environment_frame.go:572-582
func (p *EnvironmentFrame) MaybeCreateOwnGlobalBinding(key *values.Symbol, bt BindingType) (*GlobalIndex, bool)
```

Both: intern the key, check `global.keys`, return existing index if found, create and append a new binding if not. Byte-for-byte identical implementations.

The naming suggests an intended distinction that never materialized:
- `CreateGlobalBinding` — "always create" (but it doesn't — returns false if exists)
- `MaybeCreateOwnGlobalBinding` — "create if not exists, this frame only" (identical behavior)

### Call Sites

| Method | Count | Locations |
|--------|-------|-----------|
| `CreateGlobalBinding` | 3 | `compile_time_continuation.go`, `compile_validated.go` (×2) |
| `MaybeCreateOwnGlobalBinding` | 15 | `engine.go` (×2), `registry/apply.go` (×3), `library.go` (×2), `compile_time_continuation.go` (×3), `extensions/io/register.go`, `phase_registry.go`, `compile_define_for_syntax.go`, `expander_time_continuation.go` (×2) |

### Design

Keep `MaybeCreateOwnGlobalBinding` (more descriptive: "Maybe" = idempotent, "Own" = no parent walk). Delete `CreateGlobalBinding`. Update 3 call sites.

### Files Modified

| File | Changes |
|------|---------|
| `environment/environment_frame.go` | Delete `CreateGlobalBinding` |
| `machine/compile_time_continuation.go` | Rename call site |
| `machine/compile_validated.go` | Rename 2 call sites |

### Verification

```bash
go test ./environment/... ./machine/...
make lint
```

### Impact

- 1 redundant method eliminated
- API surface reduced — callers no longer need to choose between identical methods

---

## Phase 9: Delete `globalSymbolInterns` ✅ DONE

### Problem

`values/symbol_intern.go` maintained a **process-global** symbol interning table that contradicted the per-VM isolation invariant established in `TopLevelEnvironment`.

### Investigation Result

Audit found **zero production callers** of `values.InternSymbol()`. The migration to per-VM interning via `TopLevelEnvironment.InternSymbol()` was already 100% complete. All 35+ production interning calls go through `env.InternSymbol()`. The global map and mutex were pure dead state, called only by 5 self-tests.

### Resolution

Deleted `values/symbol_intern.go` (88 lines) and its 5 self-tests from `values/symbol_test.go`. Process-global mutable state eliminated from the `values` package.

---

## Execution Order

Phases are independent and can be executed in any order. Recommended sequence by risk/impact:

```
Phase 1 (Port Base)          ─── ✅ DONE (commit b99e98a)
Phase 2 (AsList)             ─── ✅ DONE (commit b99e98a)
Phase 3 (ErrMachineHalt)     ─── ✅ DONE (commit b99e98a)
Phase 4 (syntax-case)        ─── ✅ DONE (commit f5fd749)
Phase 5 (Env Delegation)     ─── ✅ DONE (commit f5fd749)
Phase 6 (VM State Struct)    ─── ✅ DONE (commit 61682c2)
Phase 7 (CallContext env)    ─── ✅ DONE (commit f5fd749)
Phase 8 (Global Binding Dup) ─── ✅ DONE (commit 61682c2)
Phase 9 (globalSymbolInterns) ── ✅ DONE (commit a310a22)
```

All phases complete.
