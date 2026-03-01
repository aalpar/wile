# E1: Circular Structure Crashes (Compiler + Display)

**Source:** `plans/R7RS-CONFORMANCE-REVIEW.md` E1, `plans/R7RS-CONFORMANCE-FIXES.md` (excluded)
**Status:** Complete
**Scope:** Compiler (datum-label literals) + values display (all circular pairs)

---

## Problem

Two sources of circular `*values.Pair` structures, both crash downstream code:

### Source 1: Datum labels in quoted literals

```scheme
'#1=(a . #1#)           ;; Go stack overflow in compiler
(equal? '#1=(a . #1#) '#2=(a . #2#))  ;; Go stack overflow in compiler
```

The parser correctly creates circular structures via datum labels (`#n=` / `#n#`),
but the compiler recurses infinitely when walking them.

### Source 2: Runtime mutation via `set-car!` / `set-cdr!`

```scheme
(define x (list 'a 'b 'c))
(set-cdr! (cddr x) x)    ;; x is now circular
(display x)               ;; Go stack overflow in SchemeString()
```

`set-car!` and `set-cdr!` are registered primitives (`registry/core/pairs.go:30-32`,
implemented in `registry/core/prim_pairs.go:64-78`, exported from `(scheme base)`).
Circular structures created at runtime reach `SchemeString()` / `String()` through
REPL display, error messages, and Go's `fmt` package.

## What Already Works

The datum label infrastructure and some cycle-aware code paths are complete:

| Component | Status | Key Location |
|-----------|--------|-------------|
| Tokenizer | Working | `internal/tokenizer/tokenizer.go:752-825` |
| Parser | Working | `internal/parser/parser.go:744-810` (pre-registers placeholder before reading) |
| Syntax types | Working | `internal/syntax/syntax_datum_{label,assignment}.go` |
| `UnwrapAllShared` | Working | `internal/syntax/syntax_value.go:168` (cache-based cycle handling) |
| Writer (`write`) | Working | `values/scheme_writer.go` (two-pass: `findShared` + `filterToCircular`) |
| `equal?` | Working | `values/utils.go:pairEqualToDeep` (visited map) |
| `list?` | Working | `values/pair.go:IsList` (Floyd's algorithm) |

## What Crashes

### Crash Site 1: `internSymbolsInValue` (compiler)

```
machine/compile_time_continuation.go:352-386
```

Called from:
- `compile_validated.go:695` — compiling `quote` literals
- `compile_time_continuation_quasiquote.go:34` — compiling quasiquote datums
- `compile_time_continuation.go:400` — compiling self-evaluating expressions

Walks `Car()` / `Cdr()` recursively with zero cycle detection. On a circular pair,
line 361 (`cdr := p.internSymbolsInValue(val.Cdr())`) recurses infinitely.

**Reachable only from datum-label literals** — `set-car!`/`set-cdr!` operate at
runtime, after compilation.

### Crash Site 2: `DeduplicateLiteral` / `deduplicatePair` (literal pool)

```
machine/native_template.go:422-434, 439-450
```

Called after `internSymbolsInValue` returns (if it ever did). Same pattern:
recursive `Car()` / `Cdr()` walk with no cycle detection.

**Reachable only from datum-label literals** — same reasoning as Crash Site 1.

### Crash Site 3: `Pair.SchemeString()` / `Pair.String()` (display)

```
values/pair.go:236-263, 280-299
```

No cycle detection. Called by REPL display, error messages, debugging, Go `fmt`.
The existing cycle-aware writer in `scheme_writer.go` is NOT used by these methods.

**Reachable from both sources** — datum-label literals (if they survive compilation)
AND runtime-created circular structures via `set-cdr!`/`set-car!`.

**Crash mechanism:** Both `SchemeString()` and `String()` call `ForEach()`
(`values/pair.go:180-202`), which iterates via `pr[1].(*Pair)` type assertion
with no cycle detection. On a circular cdr chain (e.g., `#1=(a . #1#)` or
`(set-cdr! x x)`), `ForEach` loops infinitely — the crash is in the iteration,
not in recursive calls on car elements.

For circular car structures (e.g., `#1=((#1#) . b)`), the crash would instead
be in the recursive `v.SchemeString()` / `stringValue(v)` calls on car elements.

**`String()` is NOT fixed transitively by fixing `SchemeString()`.** Both methods
call `ForEach` independently. `String()` uses `stringValue()` (`pair.go:265`),
which calls `strnr.String()` (via `fmt.Stringer` interface) for nested pairs —
NOT `SchemeString()`. Both methods need independent fixes.

## Design Decision: Compile-Time Rejection + Runtime Display Protection

### For datum-label literals (compiler): Reject at compile time

Detect circular structures in `internSymbolsInValue` using a visited set.
When a cycle is detected, return a compile-time error:
`"compile: circular datum label in quoted literal"`.

**Trade-offs:**
- Simpler — the compiler doesn't need to preserve circular structure identity
  through symbol interning and literal deduplication
- Deviates from R7RS — the spec allows `'#1=(a . #1#)` as a valid quoted datum
  that evaluates to a circular pair at runtime (§2.4)
- Acceptable — full circular literal support would require making `internSymbolsInValue`
  and `deduplicatePair` structure-preserving (placeholder + mutation pattern like
  `UnwrapAllShared`), which is significant complexity for an edge case

### For runtime circular structures (display): Add cycle detection

`SchemeString()` and `String()` **must** handle circular pairs because
`set-car!`/`set-cdr!` can create them at runtime. This is not defense in depth —
it is a required fix for a reachable crash.

**Approach:** Add a simple visited set directly to `SchemeString()` and `String()`
rather than routing through `WriteValueToString()`.

Rationale: `WriteValueToString` always emits datum-label notation (`#0=(a . #0#)`)
which is the `write` format (R7RS §6.13.3). `SchemeString()` is the primary format
for Go interop (`fmt.Stringer`), error messages, and debugging. Changing its output
format to include datum labels would be a visible behavior change in non-circular cases
(the two-pass `findShared` scan touches every pair even when there are no cycles).
A direct visited set adds cycle protection without changing output format for the
99.99% non-circular case.

When a cycle is detected, emit `...` as the cycle marker (e.g., `(a b c ...)`) rather
than datum-label notation. This is a pragmatic choice for display/debug contexts where
the exact shared structure is less important than not crashing.

---

## Phases

### Phase 1: Compiler Cycle Detection (datum-label crash fix)

**Goal:** `'#1=(a . #1#)` produces a clear compile error instead of stack overflow.

#### 1a. Add visited set to `internSymbolsInValue`

File: `machine/compile_time_continuation.go`

Add a `visited map[*values.Pair]bool` parameter (or create a wrapper that initializes it).
When a pair is seen twice, return a compile-time error.

```go
func (p *CompileTimeContinuation) internSymbolsInValue(v values.Value) (values.Value, error) {
    return p.internSymbolsInValueWithVisited(v, nil)
}

func (p *CompileTimeContinuation) internSymbolsInValueWithVisited(
    v values.Value, visited map[*values.Pair]bool,
) (values.Value, error) {
    switch val := v.(type) {
    case *values.Pair:
        if val == nil {
            return nil, nil
        }
        if visited == nil {
            visited = make(map[*values.Pair]bool)
        }
        if visited[val] {
            return nil, values.WrapForeignErrorf(
                values.ErrInvalidSyntax,
                "compile: circular datum label in quoted literal",
            )
        }
        visited[val] = true
        car, err := p.internSymbolsInValueWithVisited(val.Car(), visited)
        if err != nil {
            return nil, err
        }
        cdr, err := p.internSymbolsInValueWithVisited(val.Cdr(), visited)
        if err != nil {
            return nil, err
        }
        if car == val.Car() && cdr == val.Cdr() {
            return val, nil
        }
        return values.NewCons(car, cdr), nil
    // ... other cases unchanged but return (val, nil)
    }
}
```

**Signature change:** `internSymbolsInValue` currently returns `values.Value`.
Adding error return requires updating all 3 call sites:
- `compile_validated.go:695`
- `compile_time_continuation_quasiquote.go:34`
- `compile_time_continuation.go:400`

#### 1b. Add visited set to `DeduplicateLiteral` / `deduplicatePair`

File: `machine/native_template.go`

Same pattern. Although `internSymbolsInValue` would catch circular pairs first
(and error), `DeduplicateLiteral` should also be protected for defense in depth.

Since circular structures are now rejected at compile time, `deduplicatePair` can
simply skip already-visited pairs (return them unchanged) rather than erroring.

#### 1c. Tests

- Test that `'#1=(a . #1#)` produces a compile error (not a crash)
- Test that `(equal? '#1=(a . #1#) '#2=(a . #2#))` produces a compile error
- Test that non-circular datum labels still work: `'#0=(a b) => (a b)`
- Test that datum label references work: `'(#0=a #0#) => (a a)`
- Test that circular vector datum labels are handled gracefully by the parser
  (the parser does NOT pre-register placeholders for vectors — `parser.go:777-783` —
  so `#0=#(a #0#)` is a forward reference, which is "an error" per R7RS §2.4)

### Phase 2: Display Cycle Protection (runtime crash fix)

**Goal:** `Pair.SchemeString()` and `Pair.String()` terminate on circular structures,
whether created via datum labels or `set-cdr!`/`set-car!`.

This is a **required fix**, not defense in depth. Runtime mutation can create circular
pairs that reach these methods through normal program execution.

#### 2a. Add visited set to `Pair.SchemeString()`

File: `values/pair.go`

Replace the current `ForEach`-based implementation with a visited-set walk.
Use `map[*Pair]bool` for pointer identity cycle detection.

When a cycle is detected in the cdr chain, emit `...` as the cycle marker.
When a cycle is detected in a car element, emit `...` for that element.

Example outputs:
- `(set-cdr! x x)` where `x = (list 'a)` → `(a ...)`
- Self-referencing car: `(a (... . b) c)` or similar

This preserves the current output format for all non-circular pairs (no datum labels,
no two-pass scan overhead).

#### 2b. Add visited set to `Pair.String()` independently

File: `values/pair.go`

`String()` calls `ForEach` independently and uses `stringValue()` which dispatches
through `fmt.Stringer` (calling `String()`, not `SchemeString()`). It needs its own
visited set. Same `...` marker on cycle detection.

#### 2c. Tests

- Unit test: construct circular pair in Go via `SetCdr`, call `SchemeString()`,
  verify it terminates and produces output with `...` marker
- Unit test: same for `String()`
- Integration test: `(let ((x (list 'a))) (set-cdr! x x) (write x))` —
  verify `write` (which uses `SchemeWriter`) produces datum-label output
- Integration test: `(let ((x (list 'a))) (set-cdr! x x) (display x))` —
  verify `display` terminates (uses `DisplayValueToString`, already cycle-aware)
- Unit test: non-circular pairs produce identical output to current behavior
  (regression guard — the visited set must not change output for normal pairs)

---

## Out of Scope

| Item | Why |
|------|-----|
| Full circular literal support (Option B) | Requires structure-preserving interning and deduplication; significant complexity for edge case |
| `write-simple` infinite loop on cycles | Per R7RS, `write-simple` may loop (§6.13.3) |
| Forward datum label references | R7RS says "is an error" (§2.4) — no requirement to detect |
| Expander cycle detection | Expander operates on `SyntaxPair`, not `*values.Pair`; `UnwrapAllShared` handles circular syntax-level structures via its cache |
| `ForEach` cycle detection | `ForEach` is a general iteration primitive used throughout the codebase; adding cycle detection there would add overhead to every list operation. Instead, cycle protection is added at the display boundary (`SchemeString`/`String`) where crashes are observable |

## Files Modified

| File | Change |
|------|--------|
| `machine/compile_time_continuation.go` | Add visited set to `internSymbolsInValue`, add error return |
| `machine/compile_validated.go` | Update call site (error handling) |
| `machine/compile_time_continuation_quasiquote.go` | Update call site (error handling) |
| `machine/native_template.go` | Add visited set to `deduplicatePair` |
| `values/pair.go` | Add visited-set cycle detection to `SchemeString()` and `String()` independently |
| + test files | New tests for both phases |
