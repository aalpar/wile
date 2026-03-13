# Native Forms Migration Plan

Migrate Foreign (Go) primitives to native Scheme forms where the Scheme implementation is equivalent or superior. Four tiers, ordered by urgency.

**Branch:** `refactor/native-forms-migration` (one sub-branch per phase)

**References:**
- `TODO.md` line 122: "Reduce number of Go primitives"
- `bootstrap.scm` line 316-317: continuation-correctness precedent (`map`/`for-each`)
- R7RS §6.4 (`member`/`assoc`), §6.7 (`string-for-each`), §6.10 (`vector-map`/`vector-for-each`)

---

## Phase 0: Bootstrap File Split

**Branch:** `refactor/native-forms-migration-phase0`

Split `bootstrap.scm` into two files with clean separation of concerns.

### Files

| Old | New | Contents |
|-----|-----|----------|
| `bootstrap.scm` | `bootstrap_macros.scm` | All `define-syntax` forms |
| — | `bootstrap_procedures.scm` | All `define` forms (`map`, `for-each`, plus future additions) |

`do` stays in `bootstrap_macros.scm` (it is `define-syntax`).
`map` and `for-each` move to `bootstrap_procedures.scm` (they are `define`).

### Loading Order

Macros first, procedures second. Procedures use macros (`let`, `case-lambda`, `begin`, `and`).

### Steps

1. Rename `registry/core/bootstrap.scm` → `registry/core/bootstrap_macros.scm`
2. Create `registry/core/bootstrap_procedures.scm` with `map` and `for-each` moved from the macros file
3. Update `registry/core/bootstrap.go`:
   - Two `//go:embed` directives
   - Two `AddMacroSource` calls, macros first
4. Update `registry/core/register_test.go` to expect 2 macro sources
5. Update header comments in both `.scm` files to reflect the split

### Exit Criteria

- `make test` passes
- `make lint && make covercheck` clean
- `map`/`for-each` still work (tested by existing tests)

---

## Phase 1: Continuation Correctness

**Branch:** `refactor/native-forms-migration-phase1`
**Depends on:** Phase 0

Fix `call/cc`-inside-callback truncation. These Go implementations use `sub.ApplyCallable()` + `sub.Run()` in Go `for` loops. If `call/cc` is called inside the callback, the captured continuation cannot re-enter the Go loop — the Go stack frame is gone. This is the same architectural limitation already fixed for `map`/`for-each` (see `bootstrap.scm` line 316-317).

### Functions to Convert

| Function | Source | Extension dependency |
|----------|--------|---------------------|
| `vector-map` | `registry/core/prim_vectors.go` | None (core) |
| `vector-for-each` | `registry/core/prim_vectors.go` | None (core) |
| `string-map` | `internal/extensions/all/prim_strings.go` | None (uses core `string-ref`, `string-length`, `make-string`, `string-set!`) |
| `string-for-each` | `internal/extensions/all/prim_strings.go` | None (uses core `string-ref`, `string-length`) |
| `member` | `registry/core/prim_lists.go` | None (core) |
| `assoc` | `registry/core/prim_lists.go` | None (core) |

`memq`, `memv`, `assq`, `assv` stay in Go — they never invoke user closures, so no continuation issue.

### Scheme Implementations

All definitions go in `registry/core/bootstrap_procedures.scm`.

```scheme
;; Vector higher-order operations
;; Implemented in Scheme so that iteration produces capturable Scheme
;; continuation frames (enabling call/cc inside callbacks).

(define (vector-map f . vecs)
  (let ((len (apply min (map vector-length vecs))))
    (let ((result (make-vector len)))
      (let loop ((i 0))
        (if (< i len)
            (begin
              (vector-set! result i
                (apply f (map (lambda (v) (vector-ref v i)) vecs)))
              (loop (+ i 1)))
            result)))))

(define (vector-for-each f . vecs)
  (let ((len (apply min (map vector-length vecs))))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (apply f (map (lambda (v) (vector-ref v i)) vecs))
            (loop (+ i 1)))))))

;; String higher-order operations

(define (string-map f . strs)
  (let ((len (apply min (map string-length strs))))
    (let ((result (make-string len)))
      (let loop ((i 0))
        (if (< i len)
            (begin
              (string-set! result i
                (apply f (map (lambda (s) (string-ref s i)) strs)))
              (loop (+ i 1)))
            result)))))

(define (string-for-each f . strs)
  (let ((len (apply min (map string-length strs))))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (apply f (map (lambda (s) (string-ref s i)) strs))
            (loop (+ i 1)))))))

;; List search with optional comparator
;; Default path uses equal?. Custom comparator path must be Scheme
;; to produce capturable continuation frames.

(define member
  (case-lambda
    ((obj lst) (member obj lst equal?))
    ((obj lst compare)
     (let loop ((lst lst))
       (cond
         ((null? lst) #f)
         ((compare obj (car lst)) lst)
         (else (loop (cdr lst))))))))

(define assoc
  (case-lambda
    ((obj alist) (assoc obj alist equal?))
    ((obj alist compare)
     (let loop ((alist alist))
       (cond
         ((null? alist) #f)
         ((compare obj (caar alist)) (car alist))
         (else (loop (cdr alist))))))))
```

### Per-Function Migration Steps

For each function:

1. Remove the `PrimitiveSpec` entry from the Go registration array
2. Add the Scheme `define` to `bootstrap_procedures.scm`
3. Run `make test` — all existing tests must pass
4. Remove the `Prim*` Go function
5. Remove corresponding tests from `prim_*_test.go`
6. Run `make test` again
7. Run `make lint && make covercheck`

### New Correctness Tests

Add integration tests that verify `call/cc` works inside callbacks. These tests validate the architectural fix — they should fail against the old Go implementation and pass with the Scheme version.

```scheme
;; vector-map + call/cc
(let ((k #f))
  (let ((v (vector-map (lambda (x)
                         (if (= x 2)
                             (call/cc (lambda (c) (set! k c) x))
                             x))
                       '#(1 2 3))))
    (if k
        (let ((saved-k k))
          (set! k #f)
          (saved-k 99))
        v)))
;; => #(1 99 3)

;; member + call/cc with custom comparator
(let ((k #f))
  (let ((result (member 2 '(1 2 3)
                        (lambda (a b)
                          (if (and (= b 2) k)
                              (= a b)
                              (begin
                                (when (= b 2) (call/cc (lambda (c) (set! k c))))
                                (= a b)))))))
    result))
;; => (2 3)
```

Place these in `integration/` or as table-driven Go tests using `engine.Eval`.

### Expand-Time Note

All converted functions are currently registered at `PhaseRuntime|PhaseExpand`. No existing code uses `syntax-case`, `define-for-syntax`, or `begin-for-syntax` — expand-time availability is unused infrastructure. Loss of expand-time availability is documented, not blocking. If expand-time usage appears before this phase executes, keep thin Go wrappers at `PhaseExpand`.

### Exit Criteria

- All existing tests pass
- New `call/cc`-inside-callback tests pass
- `make lint && make covercheck` clean
- Go functions and their tests removed

---

## Phase 2: Trivial Predicates

**Branch:** `refactor/native-forms-migration-phase2`
**Depends on:** Phase 0

Replace 9 facade primitives with one-line Scheme definitions. These are pure compositions of existing primitives with no Go-specific dependency.

### Functions to Convert

| Function | Scheme definition |
|----------|-------------------|
| `not` | `(define (not x) (if x #f #t))` |
| `zero?` | `(define (zero? z) (= z 0))` |
| `positive?` | `(define (positive? x) (> x 0))` |
| `negative?` | `(define (negative? x) (< x 0))` |
| `exact-integer?` | `(define (exact-integer? x) (and (exact? x) (integer? x)))` |
| `list?` | See below |
| `boolean=?` | See below |
| `symbol=?` | See below |
| `square` | `(define (square x) (* x x))` |

### Non-Trivial Definitions

```scheme
;; list? — must detect cycles (R7RS §6.4: "Returns #t if obj is a proper list")
;; Uses tortoise-and-hare for cycle detection.
(define (list? x)
  (let loop ((slow x) (fast x))
    (cond
      ((null? fast) #t)
      ((not (pair? fast)) #f)
      ((null? (cdr fast)) #t)
      ((not (pair? (cdr fast))) #f)
      ((eq? slow (cdr fast)) #f)
      (else (loop (cdr slow) (cddr fast))))))

;; boolean=? — variadic, all args must be booleans and equal
(define (boolean=? b1 b2 . rest)
  (if (not (boolean? b1))
      (error "boolean=?: not a boolean" b1)
      (let loop ((prev b1) (args (cons b2 rest)))
        (if (null? args) #t
            (let ((curr (car args)))
              (if (not (boolean? curr))
                  (error "boolean=?: not a boolean" curr)
                  (and (eq? prev curr)
                       (loop curr (cdr args)))))))))

;; symbol=? — same pattern
(define (symbol=? s1 s2 . rest)
  (if (not (symbol? s1))
      (error "symbol=?: not a symbol" s1)
      (let loop ((prev s1) (args (cons s2 rest)))
        (if (null? args) #t
            (let ((curr (car args)))
              (if (not (symbol? curr))
                  (error "symbol=?: not a symbol" curr)
                  (and (eq? prev curr)
                       (loop curr (cdr args)))))))))
```

### Precondition Check

Before removing each Go primitive, verify no expand-time usage:

```bash
grep -rn 'syntax-case\|define-for-syntax\|begin-for-syntax' lib/ test/ examples/
```

Currently zero hits. If this changes, keep thin Go wrappers at `PhaseExpand`.

### Migration Steps

Same per-function steps as Phase 1. No new correctness tests needed — these are simple substitutions covered by existing test suites.

### Source Files Affected

| Function | Registration file | Implementation file | Test file |
|----------|------------------|--------------------|-----------|
| `not` | `registry/core/equality.go` | `registry/core/prim_equality.go` | `registry/core/prim_equality_test.go` |
| `zero?`, `positive?`, `negative?` | `registry/core/predicates.go` | `registry/core/prim_predicates.go` | `registry/core/prim_predicates_test.go` |
| `exact-integer?`, `list?` | same | same | same |
| `boolean=?`, `symbol=?` | `registry/core/equality.go` | `registry/core/prim_equality.go` | `registry/core/prim_equality_test.go` |
| `square` | `extensions/math/transcendental.go` | `extensions/math/prim_transcendental.go` | `extensions/math/prim_transcendental_test.go` |

### Exit Criteria

- All existing tests pass
- `make lint && make covercheck` clean
- Go functions and their tests removed

---

## Phase 3: List Algorithms (Benchmark-Gated)

**Branch:** `refactor/native-forms-migration-phase3`
**Status:** Partially complete. `call-with-port` migrated to Scheme (in io extension
macro source). All 6 list algorithms benchmarked and **kept in Go** (4-9x slower on
short lists; all exceed the 20% micro-benchmark gate). See benchmark results below.
`call-with-input-file`/`call-with-output-file` kept in Go (files extension must remain
independently loadable without io; these functions' Go implementations own their security
checks).
**Depends on:** Phase 0

Convert pure-algorithmic operations to Scheme. Each conversion is gated by benchmark measurements.

### Candidates

| Function | Source | Notes |
|----------|--------|-------|
| `make-list` | `registry/core/prim_lists.go` | Build list of n copies |
| `list-copy` | same | Copy spine |
| `list-tail` | same | k iterations of `cdr` |
| `reverse` | same | Accumulator pattern |
| `length` | same | Counter walk |
| `append` | same | Right-fold concatenation |
| `call-with-port` | `internal/extensions/io/prim_ports.go` | Must handle multiple return values |
| `call-with-input-file` | `extensions/files/prim_files.go` | Delegates to `open-input-file` + `call-with-port` |
| `call-with-output-file` | `extensions/files/prim_files.go` | Delegates to `open-output-file` + `call-with-port` |

### Benchmark Protocol

**Before conversion (per candidate):**

1. Ensure a micro-benchmark exists in `prim_bench_test.go` for the candidate. If missing, add one.
2. Run `make bench` — record baseline numbers for the candidate.
3. Run `make bench-gabriel` — record baseline (Gabriel benchmarks use lists heavily).

**After conversion (per candidate):**

4. Re-run `make bench` — compare candidate's micro-benchmark against baseline.
5. Re-run `make bench-gabriel` — compare all Gabriel benchmarks against baseline.

**Gate thresholds:**

| Metric | Threshold | Action if exceeded |
|--------|-----------|-------------------|
| Micro-benchmark | > 20% slower | Keep Go version, document why |
| Any Gabriel benchmark | > 5% slower | Investigate; keep Go if attributable |

The 20%/5% split reflects that micro-benchmarks amplify per-call overhead while Gabriel benchmarks measure real workloads.

### Scheme Implementations

```scheme
;; List algorithms

(define (make-list k . fill)
  (let ((f (if (null? fill) #f (car fill))))
    (let loop ((i 0) (result '()))
      (if (< i k)
          (loop (+ i 1) (cons f result))
          result))))

(define (list-copy lst)
  (if (null? lst) '()
      (cons (car lst) (list-copy (cdr lst)))))

(define (list-tail lst k)
  (if (zero? k) lst
      (list-tail (cdr lst) (- k 1))))

(define (reverse lst)
  (let loop ((lst lst) (acc '()))
    (if (null? lst) acc
        (loop (cdr lst) (cons (car lst) acc)))))

(define (length lst)
  (let loop ((lst lst) (n 0))
    (if (null? lst) n
        (loop (cdr lst) (+ n 1)))))

(define (append . lsts)
  (cond
    ((null? lsts) '())
    ((null? (cdr lsts)) (car lsts))
    (else
     (let append2 ((a (car lsts)) (b (apply append (cdr lsts))))
       (if (null? a) b
           (cons (car a) (append2 (cdr a) b)))))))

;; Port resource management
;; R7RS §6.13.1: "the values yielded by proc are returned"
;; Must preserve multiple return values.

(define (call-with-port port proc)
  (let ((results (call-with-values (lambda () (proc port)) list)))
    (close-port port)
    (apply values results)))

;; File operations — delegate to open-*-file + call-with-port
;; Security is enforced by open-input-file / open-output-file
;; (security.Check at prim_files.go openFile helper).

(define (call-with-input-file filename proc)
  (call-with-port (open-input-file filename) proc))

(define (call-with-output-file filename proc)
  (call-with-port (open-output-file filename) proc))
```

### Security Note

`call-with-input-file` and `call-with-output-file` currently have their own `security.Check` in `callWithFile` (`prim_files.go:150`). When delegating to `open-input-file`/`open-output-file`, the security check happens inside the `openFile` helper (`prim_files.go:38`). Verify by running security integration tests that file access is still gated.

### `call-with-port` Placement

Unlike the list algorithms, `call-with-port` depends on `close-port` which is in the I/O extension. Two options:

- (a) Define in `bootstrap_procedures.scm` — works only if the I/O extension is loaded
- (b) Define via the I/O extension's own `AddMacroSource`

Recommend (b) for `call-with-port` and the files extension's own macro source for `call-with-input-file`/`call-with-output-file`. This preserves the invariant that bootstrap code depends only on core primitives.

### Benchmark Results (Apple M4 Max, 2026-03-13)

All 6 candidates failed the 20% micro-benchmark gate. All kept in Go.

| Function | Baseline (Go) | Scheme | Change | Decision |
|----------|--------------|--------|--------|----------|
| `length` | 199 ns | 1787 ns | +797% | **kept in Go** |
| `append` | 392 ns | 1816 ns | +363% | **kept in Go** |
| `reverse` | 278 ns | 2006 ns | +620% | **kept in Go** |
| `make-list` | 285 ns | 2255 ns | +690% | **kept in Go** |
| `list-tail` | 191 ns | 1188 ns | +519% | **kept in Go** |
| `list-copy` | 268 ns | 1849 ns | +588% | **kept in Go** |

Root cause: for 5-element lists, each per-element Scheme step (car/cdr/null?/+) is a
foreign-function dispatch. The Go versions use tight native loops; the Scheme versions
pay VM overhead per element that dominates on short lists. The benchmark gate exists
precisely to catch this: 4-9x slower is far outside the 20% threshold.

The benchmark cases (Reverse, MakeList, ListTail, ListCopy) were added to
`registry/core/prim_bench_test.go` and remain as fixtures for future re-evaluation
if the VM dispatch overhead is reduced.

### Unblocking Path: List Primitive Opcodes

**Thesis:** These 6 functions cannot be migrated to Scheme because they fail the
performance gate. They fail the gate because their per-element sub-operations
(`car`, `cdr`, `null?`, `cons`, `+`, `<`) are ForeignFunction calls, each incurring
the full dispatch overhead: argument extraction from `MachineContext`, Go interface
type assertion, result boxing via `SetValue`, and the indirect function call itself.
For a 5-element list, this overhead is paid 5× per operation, dominating the total
cost and producing the observed 4-9× regression.

Two approaches exist to reduce this overhead, both worth exploring:

#### Approach A: Promoted Opcodes

Promote hot-path primitives to VM opcodes (`OpCar`, `OpCdr`, `OpNullQ`, `OpCons`,
`OpFixnumAdd`, `OpFixnumLT`). Each would execute inline in `MachineContext.Run()`
without ForeignFunction dispatch — a direct stack pop, type assertion, and stack
push, all within the Go frame already holding `mc`. Per-element cost would drop
from ~300 ns to an estimated ~30-50 ns (based on existing opcode dispatch for
`OpPush`/`OpPop`).

Prior art: `OpEqQ`, `OpVectorQ`, `OpVectorRef` already follow this pattern
(`call_promoted.go`). Each has a tail variant and a `callPromotedFallback` path
for when the binding has been reassigned via `set!`.

**Candidates:**

| Current Primitive | Proposed Opcode | Hot Loop Role |
|-------------------|----------------|---------------|
| `car` (PrimCar) | `OpCar` | Element access per step |
| `cdr` (PrimCdr) | `OpCdr` | List traversal per step |
| `null?` (PrimNullQ) | `OpNullQ` | Termination check per step |
| `cons` (PrimCons) | `OpCons` | Result construction per step |
| `+` on fixnums | `OpFixnumAdd` | Counter increment `(+ n 1)` |
| `<` on fixnums | `OpFixnumLT` | Bound check `(< i k)` |

The compiler would emit these opcodes when the callee is statically known (a
global binding that hasn't been `set!`'d). Fallback to ForeignFunction dispatch
when the binding is dynamic or shadowed.

**Cost**: 6 opcodes × 2 (non-tail + tail) = 12 new switch cases, expanding the
`Run()` dispatch from 37 to 49 cases. Risk: L1 icache pressure from a larger
`Run()` function body degrades all opcode dispatch, not just the new ones.

#### Approach B: Lightweight Cached Foreign Call

Instead of expanding the switch, specialize `callForeignCached` itself. The
current path does `Drain()` + `checkArity()` + `bindArgs()` + env swap even
though the compiler already knows the arity and parameter layout. A "direct"
variant would skip these steps.

The bottleneck in `callForeignCached` is not arg collection (step 3 below) but
the **post-collection ceremony** (steps 4–8):

```
callForeignCached (current):
  1. cachedBindings[arg].Value()     binding lookup
  2. type-assert *ForeignClosure     branch
  3. Drain()                         zero-alloc, gets []Value view
  4. checkArity()                    compare + branch           ← skip
  5. bindArgs(bnds, vs, ...)         loop: SetValue per arg     ← skip
  6. mc.env = fcls.env               env swap                   ← skip
  7. fcls.fn(mc)                     INDIRECT function call
  8. template change check           branch                     ← skip
  9. RestoreAndRelease               continuation management
```

Steps 4–6 and 8 are unnecessary when the compiler knows the callee at compile
time. A new calling convention where the foreign function reads args directly
from a stack view (instead of `mc.Arg()` reading from environment bindings)
would eliminate them. This captures most of the promoted-op savings in a single
new opcode (`OpCallForeignDirect`) rather than 12, keeping the switch compact.

**Cost**: 1 new opcode (+ tail variant = 2 switch cases). Requires eligible
`Prim*` functions to support a second calling convention (stack-arg reader), or
a wrapper that adapts the stack view to the existing `mc.Arg()` interface.

#### Evaluation Protocol

The approaches are not mutually exclusive. Use this protocol to evaluate either:

**Phase 1 — Measure icache cost (applies to Approach A only):**

1. Add the new case arms to `Run()` but do NOT change the compiler to emit them.
2. Run Gabriel benchmarks. Compare against baseline.
3. If existing benchmarks regress > 1-2%, the switch is too big — Approach A
   is not viable regardless of per-call savings. Fall back to Approach B.

**Phase 2 — Measure dispatch savings:**

4. Enable the compiler to emit the new opcodes (A) or the direct call (B).
5. Run the Phase 3 list algorithm micro-benchmarks (`prim_bench_test.go`):
   Length, Append, Reverse, MakeList, ListTail, ListCopy.
6. Run Gabriel benchmarks for net workload impact.
7. Calculate: `(opcode_frequency × per_call_savings) - (total_ops × cache_penalty)`.

**Phase 3 — Re-attempt Scheme migration:**

8. Convert the 6 list algorithms to Scheme (definitions already written above).
9. Re-run Phase 3 micro-benchmarks. If all pass the 20% gate, the migration
   succeeds. If some still fail, keep those in Go and document the numbers.

The opcode hit counters (`mc.counters.opcodeHits[instr.Op]++` at
`machine_context.go:282`) provide frequency data for step 7 without additional
instrumentation.

This is tracked in `TODO.md` as **List primitive opcodes**.

### Per-Candidate Migration Steps

1. Verify/add micro-benchmark
2. Record baseline (`make bench`, `make bench-gabriel`)
3. Remove Go registration, add Scheme definition
4. Re-run benchmarks, compare against gate thresholds
5. If gate passes: remove Go function and tests, commit
6. If gate fails: revert, document in this plan as "kept in Go: [reason]"

### Exit Criteria

- All converted functions pass existing tests
- No benchmark regression beyond gate thresholds
- Unconverted functions documented with benchmark data
- `make lint && make covercheck` clean

---

## Phase 4: CxR Consolidation

**Branch:** `refactor/native-forms-migration-phase4`
**Depends on:** Phase 0

Replace 28 Go CxR primitives with Scheme definitions. The same 28 functions already exist in `lib/wile/cxr.sld` — the Go primitives are redundant infrastructure.

### Scheme Definitions

Add to `bootstrap_procedures.scm`:

```scheme
;; CxR accessors (R7RS §6.4, also in (scheme cxr) library)
;; 2-level
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
;; 3-level
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
;; 4-level
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))
```

### Go Code to Remove

From `registry/core/prim_pairs.go`:
- `cxrEntries` table (~30 lines)
- `makeCxrPrimitive` function
- `cxrHelper` function
- CxR registration loop in `registry/core/pairs.go`

From `registry/core/prim_pairs_test.go`:
- CxR test cases

### Verification

`(import (scheme cxr))` must still work — `lib/wile/cxr.sld` has its own definitions that shadow the bootstrap versions when the library is imported.

### Exit Criteria

- `cadr`, `caddr`, etc. work in the base environment (bootstrap definitions)
- `(import (scheme cxr))` works (library definitions)
- All existing tests pass
- `make lint && make covercheck` clean
- Go CxR infrastructure removed

---

## Cross-Cutting Concerns

### Phase Independence

Phases 1-4 all depend on Phase 0 but are independent of each other. They can be executed in any order after Phase 0 and merged independently.

Recommended execution order: Phase 0 → Phase 1 (correctness fix, highest priority) → Phase 2 (lowest risk) → Phase 4 (straightforward) → Phase 3 (needs benchmark infrastructure).

### Go Code Cleanup

After each phase, run `make lint` to catch:
- Unused imports
- Orphaned helper functions
- Empty test files

### Documentation Updates

- Update `TODO.md` line 122 after each phase to reflect progress
- Update `CHANGELOG.md` with the migration (one entry per phase)

### Expand-Time Availability

All converted functions are currently registered at `PhaseRuntime|PhaseExpand`. Moving to Scheme `define` removes expand-time availability. As of the plan's writing date, no code in the repository uses `syntax-case`, `define-for-syntax`, or `begin-for-syntax` — expand-time availability is unused infrastructure.

**If expand-time usage appears before a phase executes:** Keep thin Go wrappers at `PhaseExpand` that delegate to the Scheme definitions, or find a mechanism to register Scheme definitions in the expand environment.

---

## Summary

| Phase | Functions | Migrated | Kept in Go | Motivation |
|-------|-----------|----------|------------|------------|
| 0 | Bootstrap split | — | — | Infrastructure |
| 1 | `vector-map`, `vector-for-each`, `string-map`, `string-for-each`, `member`, `assoc` | 6 | 0 | Continuation correctness |
| 2 | `not`, `zero?`, `positive?`, `negative?`, `exact-integer?`, `list?`, `boolean=?`, `symbol=?`, `square` | 9 | 0 | Eliminate facades |
| 3 | `call-with-port` | 1 | 8 | Simplify (benchmark-gated) |
| 4 | 28 CxR accessors | 28 | 0 | Eliminate duplication |
| **Total** | | **44** | **8** | |

Phase 3 kept in Go:
- 6 list algorithms (`make-list`, `list-copy`, `list-tail`, `reverse`, `length`, `append`): all 4-9× slower in Scheme — ForeignFunction dispatch per element dominates. Unblocked by future list primitive opcodes (see above).
- `call-with-input-file`, `call-with-output-file`: files extension must be independently loadable without io; Go implementations own their security checks. `callWithFile` single-value bug fixed (`SetValue` → `SetValues`).

Net reduction: 44 Go primitive implementations replaced by ~120 lines of Scheme.
