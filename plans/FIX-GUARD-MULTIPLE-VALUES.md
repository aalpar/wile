# FIX-GUARD-MULTIPLE-VALUES

**Status**: Draft (Phase 1)
**TODO item**: #3 — `guard` body drops multiple values

---

## Problem

The `guard` macro in `registry/core/bootstrap.scm:186–188` cannot propagate
multiple values from its body to the caller. The current body path is:

```scheme
(lambda ()
  (let ((result (begin e1 e2 ...)))
    (guard-k (lambda () result))))
```

**Verified behavior** (run `(guard (e (#f)) (values 1 2))`):

```
Exception: exception: #<error-object "expected 1 arguments, got 2: wrong number of arguments">
```

The failure mode is a hard VM error — not silent truncation. `let` expands to
`((lambda (result) ...) (values 1 2))`. The VM sees a 1-arity lambda applied
to 2 argument values and raises a wrong-argument-count error.

Two failure modes after the fix must not regress:

1. **`(values v1 v2 ...)` in body → hard error.** The above confirms the
   current failure is arity mismatch at lambda application. Fix eliminates this
   by never passing multiple values to a single-parameter binder.

2. **Multiple values cannot propagate through the thunk channel.** Even a
   body that doesn't error (single-value return) can't be replaced by a
   multi-value return without the `call-with-values` pattern, because the thunk
   wrapping `guard-k`'s argument must re-emit values when called.

---

## Root Cause

`let` compiles to `((lambda (result) ...) (begin e1 e2 ...))`. A lambda with
arity 1 receiving multiple values is a hard error in both R7RS and Wile.

The R7RS §7.3 reference implementation has the exact same code, making this a
known spec limitation — not a Wile-specific bug. We are deliberately going
beyond the reference implementation.

---

## Fix

**File**: `registry/core/bootstrap.scm`
**Location**: lines 186–188 (the normal-return path in `guard`)

Replace:
```scheme
(lambda ()
  (let ((result (begin e1 e2 ...)))
    (guard-k (lambda () result)))))
```

With:
```scheme
(lambda ()
  (call-with-values
   (lambda () e1 e2 ...)
   (lambda results
     (guard-k (lambda () (apply values results)))))))
```

### Why this works

- `call-with-values` captures all values produced by the body thunk into the
  rest-arg list `results`.
- `(apply values results)` re-emits them when the outer `((call/cc ...))` calls
  the winning thunk.
- Zero values (`(values)`) → `results = ()` → `(apply values '())` → `(values)`. ✓
- One value → `results = (v)` → `(apply values '(v))` → `v`. ✓
- N values → forwarded intact. ✓

### What does NOT need to change

`guard-aux` handler paths are already correct:

- `(begin result ...)` in `else` and test clauses returns multiple values
  naturally — the `begin` body IS the thunk body, so multiple values propagate.
- `(proc t)` in `=>` clauses propagates whatever `proc` returns.

Both paths go through `guard-k` as a thunk content, so the thunk call site
already handles whatever `guard-aux` returns.

### Prerequisites at runtime

Both `call-with-values` and `apply` are core runtime primitives (registered in
`control.go`). They are always present when `guard` is used. The `(lambda
results ...)` rest-arg is a heap-allocated proper list — not backed by the
`restArgBuf` that variadic *primitives* use — so there is no aliasing risk.

---

## Tests

**File**: `test/scheme/exceptions-test.scm`
**Location**: add a new `test-group` after "guard normal execution"

```scheme
;; ── Guard: multiple-value body ────────────────────────────────────

(test-group "guard body multiple values"
  ;; two values propagate through guard when no exception is raised
  (define-values (a b)
    (guard (exn (else (values -1 -1)))
      (values 1 2)))
  (test 1 a)
  (test 2 b)

  ;; zero values (unusual but must not error)
  ;; Use call-with-values to consume the zero-value return.
  (test #t
    (call-with-values
     (lambda ()
       (guard (exn (else 'caught))
         (values)))
     (lambda () #t)))

  ;; single value still works (regression)
  (test 42
    (guard (exn (else 'caught))
      42))

  ;; multiple values with multi-expression body (e1 e2 are sequenced,
  ;; last expression produces multiple values)
  (define-values (x y z)
    (guard (exn (else (values -1 -1 -1)))
      (define ignored 0)
      (values 10 20 30)))
  (test 10 x)
  (test 20 y)
  (test 30 z))
```

---

## Documentation

Update `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md`:

- The summary currently says "One known limitation exists." After this fix, add
  a note that `guard` now propagates multiple values from the body, unlike the
  R7RS §7.3 reference implementation.

This is an extension beyond R7RS — a feature, not a deviation. Document it
briefly so future contributors know the behavior is intentional.

---

## Implementation Sequence

1. Verify current error behavior: run `(guard (e (#f)) (values 1 2))` in REPL
2. Edit `registry/core/bootstrap.scm` lines 186–188 (3-line change → 4 lines)
3. Run `make test` — existing guard tests must still pass
4. Add the new `test-group "guard body multiple values"` to `test/scheme/exceptions-test.scm`
5. Run `./dist/$(go env GOOS)/$(go env GOARCH)/scheme test/scheme/exceptions-test.scm` to verify
6. Update `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` summary section
7. Run `make lint`

**Estimated diff**: ~10 lines changed across 3 files.

---

## Open Questions

- [x] **Exact current error verified**: `"expected 1 arguments, got 2: wrong
  number of arguments"` — lambda arity mismatch, not a multiple-values error.
- [ ] **Zero-value test**: `(values)` returning zero values from `guard` — is
  this meaningful? The test above uses a `call-with-values` wrapper to consume
  it. If zero-value `guard` returns have no practical use case, skip that test
  case.
- [ ] **R7RS_SEMANTIC_DIFFERENCES.md**: Should this appear in the summary as
  "Wile extends guard to propagate multiple values" or just be omitted (since
  it's strictly more correct)?
