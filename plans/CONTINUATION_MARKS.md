# Continuation Marks

Racket-style per-frame key-value annotations on the continuation chain.

## Motivation

Continuation marks attach metadata to stack frames without mutation or
dynamic-wind overhead. They enable:

- **Stack inspection** — collect values from every frame (profiling, debugging)
- **Parameter-like scoping** — without global mutable cells or dynamic-wind thunks
- **Exception context** — structured annotations visible to error handlers
- **Tail-position awareness** — same-frame replacement gives correct tail behavior for free

## Existing Infrastructure

| Asset | Location | Relevance |
|-------|----------|-----------|
| Continuation chain | `machine/machine_continuation.go` | Walk for `continuation-mark-set->list` |
| `vmState` embedding | `machine/vm_state.go` | Adding `marks` field propagates to both `MachineContext` and `MachineContinuation` |
| Prompt tags | `machine/prompt_tag.go` | Delimit mark collection scope |
| `CaptureStackTrace` | `machine/machine_context.go` | Reference walk pattern (live frame + chain) |
| Continuation pool | `machine/pool.go` | Zero-on-release already works for nil maps |
| `Copy()` / `DeepCopy()` | `machine/machine_continuation.go` | Must copy marks map |

## R7RS / Racket API Surface

```scheme
;; Core form (compiler support required)
(with-continuation-mark key val body)

;; Mark retrieval
(current-continuation-marks)              ; → mark-set
(current-continuation-marks prompt-tag)   ; → mark-set (delimited)
(continuation-marks cont)                 ; → mark-set from captured continuation

;; Mark set operations
(continuation-mark-set->list mark-set key)          ; → list of values
(continuation-mark-set->list mark-set key prompt-tag)
(continuation-mark-set-first mark-set key)          ; → nearest value or #f
(continuation-mark-set-first mark-set key prompt-tag default)

;; Predicate
(continuation-mark-set? v)

;; Convenience
(call-with-immediate-continuation-mark key proc)
(call-with-immediate-continuation-mark key proc default)
```

## Data Structure

```go
// In vmState (vm_state.go)
marks map[values.Value]values.Value  // nil when no marks (common case)
```

Lazy allocation: most frames carry no marks, so `nil` map is the zero-cost
default. `with-continuation-mark` allocates on first use.

### Representation: `ContinuationMarkSet`

A new `values.Value` type holding a snapshot of marks collected from a chain
walk. Needs design:

- Flat list of `(key . value)` pairs per frame? Or map per frame?
- Immutable snapshot vs lazy walk?

TODO: decide representation

## Phases

### Phase 1: Data Structure + `with-continuation-mark`

- Add `marks` field to `vmState`
- Add `SetMark(key, val)` / `GetMark(key)` methods on `MachineContext`
- Compiler support: `with-continuation-mark` as special form
  - Sets mark on current frame, evaluates body, restores
  - Tail-position: if body is in tail position, mark stays on same frame
    (no `SaveContinuation`)
- Update `Copy()` to shallow-copy marks map
- Update `releaseContinuation` — already zeroes struct, nil map is fine

### Phase 2: Mark Collection

- `ContinuationMarkSet` value type
- `current-continuation-marks` — walk chain, collect marks, return set
- `continuation-mark-set->list` — extract values for a given key
- `continuation-mark-set-first` — first (nearest) value for a key
- Prompt-delimited collection — stop walk at matching `promptTag`

### Phase 3: Integration + Convenience

- `call-with-immediate-continuation-mark` ✓ — in `registry/core/prim_cont_marks.go`. Uses `GetImmediateMark` which checks `mc.marks` (tail) then `mc.cont.marks` (non-tail).
- `continuation-mark-set?` predicate ✓ — already done in Phase 2
- `continuation-marks` on captured continuations — **deferred**: requires a new `CapturedContinuation` value type. `call/cc` returns a `ForeignClosure` (opaque Go closure); there is no way to extract the continuation chain without a dedicated continuation object type. When this type is added, `CollectMarksFromContinuation` (in `machine/continuation_mark_set.go`) is already in place.
- Consider: reimplement `parameterize` using marks (optional, breaking)

## Key Semantics

### Tail-position mark replacement

```scheme
(with-continuation-mark 'k 1
  (with-continuation-mark 'k 2
    (continuation-mark-set->list (current-continuation-marks) 'k)))
;; => (2)   — NOT (2 1), because both are on the same frame
```

The inner `with-continuation-mark` replaces `'k` on the current frame when
the body is in tail position. This is the defining feature: marks don't
accumulate on the same frame, they replace.

### Non-tail position

```scheme
(with-continuation-mark 'k 1
  (list (with-continuation-mark 'k 2
          (continuation-mark-set->list (current-continuation-marks) 'k))))
;; => ((2 1))   — two frames, two marks
```

The `list` call creates a new frame (via `SaveContinuation`), so both marks
are visible on separate frames.

### Prompt delimiting

```scheme
(call-with-continuation-prompt
  (lambda ()
    (with-continuation-mark 'k 'inner
      (continuation-mark-set->list (current-continuation-marks) 'k)))
  tag)
;; Marks below the prompt boundary are NOT collected
```

## Open Questions

- Should marks be exposed as an extension (opt-in) or core?
- `ContinuationMarkSet` representation — eager snapshot vs lazy reference?
- Interaction with `call/cc`: marks are part of the captured continuation;
  how does `Copy()` vs `DeepCopy()` interact?
- Performance: any concern about map allocation pressure on hot paths?
  Consider: small-map optimization (inline 1-2 entries)?
- Should `parameterize` eventually be reimplemented on top of marks?
  Racket does this, but it's a breaking change if parameter mutation
  semantics differ.

## References

- Flatt, Dybvig — "Adding Delimited and Composable Control to a
  Production Programming Language" (ICFP 2007)
- Racket Reference: §1.1.12 "Continuation Marks"
- Clinger, Hartheimer, Ost — "Implementation Strategies for
  Continuations" (1999) — marks as frame annotations
