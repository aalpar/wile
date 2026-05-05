# Iteration Idioms in Wile

Wile uses four distinct iteration shapes across its Go codebase. Each
serves a different concern, and they are not interchangeable. New code
should pick the shape that matches its boundary, not default to whatever
the nearest file uses.

## 1. Scheme/Go boundary: `ForEach(ctx, fn) (Value, error)`

Used by types that participate in Scheme list discipline:
`values.Tuple` (`Pair`, `EmptyList`), `internal/syntax.SyntaxValue`,
and `registry/helpers.ForEachList`.

```go
ForEach(ctx context.Context,
        fn func(ctx context.Context, i int, hasNext bool, v Value) error,
       ) (Value, error)
```

**Why all four channels are load-bearing:**

| Channel | Carries |
|---|---|
| `ctx` | Scheme-level cancellation (timeouts, `thread-terminate!`) |
| `error` | Scheme exceptions, `ErrPromptAbort`, `ErrExceptionEscape` |
| `i, hasNext` | Required by primitives that special-case the last element (e.g. `for-each` body that prints separators) |
| return `Value` | Scheme improper-list discipline — the final cdr, which may be a non-`EmptyList` |

`iter.Seq` would discard all four. **Do not migrate.** Use this shape
only when the iteration is *Scheme iteration implemented in Go*, not
plain Go iteration.

## 2. Pure-Go traversal: `iter.Seq[T]` / `iter.Seq2[K, V]`

The default for Go-internal iteration on Wile value types as of 2026.

```go
func (p *T) All() iter.Seq[ElemT]
func (p *T) Pairs() iter.Seq2[KeyT, ValT]   // when both key+value matter
```

Examples: `values.CharSet.All`, `values.CharSet.Codepoints`.

**Why:**

- Matches Go stdlib (`slices.Values`, `slices.All`, `maps.All`,
  `strings.SplitSeq`).
- Range-over-func is the natural call shape:
  `for r := range cs.All() { ... }`.
- Early-exit via `break`; no error/context ceremony.
- The iterator function may close over the receiver and yield directly
  from internal state — no defensive copy required when the type is
  immutable.

**Cost note:** the closure escapes to the heap (one allocation per
call to the accessor). For collections of size *n*, this beats a
defensive `O(n)` slice copy. For *n=0* it's strictly worse than a nil
slice. The trade-off favors `iter.Seq` for any *n ≥ 1* and for any
caller that may early-exit.

**Naming:** `All()` for the primary iteration; type-specific names
like `Codepoints()`, `Keys()`, `Values()` when the element type
demands it. Mirrors `slices.All` / `maps.All` / `slices.Values`.

## 3. Mutex-protected snapshot: `Foo() []T` returning a fresh copy

Used by concurrent accessors:
`environment.GlobalEnvironmentFrame.Bindings`,
`environment.GlobalEnvironmentFrame.Keys`,
`coverage.Collector.Entries`, `registry.Registry.Bindings`.

```go
func (p *T) Foo() []ElemT {
    p.mu.RLock()
    defer p.mu.RUnlock()
    return slices.Clone(p.entries)
}
```

**Why `iter.Seq` is the wrong tool here:** the snapshot **is** the
safety mechanism. The accessor takes the lock, clones the data,
releases the lock, and the caller iterates the disconnected copy
without holding the lock. Three failure modes if you try to convert
to `iter.Seq`:

1. Hold the lock through the iterator's lifetime — caller `break`
   leaks the lock.
2. Re-acquire per element — perf catastrophe and read-during-write
   hazards.
3. Snapshot internally then yield — identical to the slice form, just
   with an extra closure allocation.

(3) works but provides no benefit. Keep the snapshot pattern.

**Use when:** the type is concurrently mutated and the iterator must
not observe in-flight changes.

## 4. Error-propagating callback: `Foo(fn) error`

Used by single sites where mid-iteration error-out is required:
`values.Hashtable.Entries`.

```go
func (p *T) Foo(fn func(K, V) error) error
```

**Why:** when the caller wants to fail the entire iteration on a
structured error (not just `break`), and the error must surface
naturally. `iter.Seq2` doesn't have an error channel; rewriting forces
callers to capture errors in closures, which is messier than the
direct callback shape.

**Use when:** a single dominant call site needs structured
error-during-iteration. If the pattern recurs at multiple sites,
prefer materializing a snapshot or using `iter.Seq2` with a captured
error variable.

## Decision tree

```
Does the iteration cross the Scheme/Go FFI boundary?
├── YES → 1. ForEach(ctx, fn) (Value, error)
└── NO  → Does the underlying data need a lock for safe reads?
         ├── YES → 3. Mutex-protected snapshot ([]T return)
         └── NO  → Must the iterator body fail with an error
                   the iterator itself reports?
                   ├── YES → 4. Error-propagating callback (fn func) error
                   └── NO  → 2. iter.Seq[T] (default for new code)
```

## Migration policy

Existing code that doesn't fit the chosen shape is migrated
**opportunistically, not as a sweep**. Triggers:

- A 2nd or 3rd consumer of an iteration accessor reveals friction
  (defensive copy is in a hot path, caller wants early-exit but only
  has slice-copy form).
- A new value type is added — start it on `iter.Seq` from day one.
- A subsystem is refactored for unrelated reasons and the iteration
  shape is in the diff anyway.

Do not file PRs whose sole purpose is converting Tier 3 to Tier 2 — the
mechanical cost outweighs the convention benefit when there's no
underlying friction.

See `plans/2026-05-05-iter-seq-cascade.md` for the active selective
migration list.
