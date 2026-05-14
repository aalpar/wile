# Primitive Annotation Audit — Phase 4 (Axis C) Findings: Lists

**Status**: Complete. 2 findings, both resolved (1 code, 1 documentation of pre-existing deviation).
**Category**: R7RS §6.4 Pairs and lists (14 primitives in `registry/core/lists.go` + `registry/core/pairs.go`; `member`/`assoc` in `bootstrap_procedures.scm`).
**Plan**: `plans/2026-04-19-primitive-annotation-audit.md` — Phase 4 (Axis C).
**Prior categories**: bytevectors (2 code), strings (2 code + 1 doc), ports (0 code + 2 doc).

## Scope

| Primitive | Arity | File |
|---|---|---|
| `cons`, `car`, `cdr`, `set-car!`, `set-cdr!` | 2, 1, 1, 2, 2 | `registry/core/pairs.go` |
| `list`, `make-list` | 0+, 1–2 | `registry/core/lists.go` |
| `append`, `reverse`, `length` | 0+, 1, 1 | `registry/core/lists.go` |
| `list-ref`, `list-set!`, `list-tail`, `list-copy` | 2, 3, 2, 1 | `registry/core/lists.go` |
| `memq`, `memv`, `assq`, `assv` | 2, 2, 2, 2 | `registry/core/lists.go` |
| `member`, `assoc` | 2–3, 2–3 | `bootstrap_procedures.scm` (Scheme) |
| `pair?`, `null?`, `list?` | 1 each | `registry/core/predicates.go` + bootstrap |

Out of axis-C scope this session: CxR accessors (`caar`..`cddddr`), `map`, `for-each` — all implemented in `bootstrap_procedures.scm` as Scheme.

## Finding E.1 — `append` ParamTypes rejects the spec-allowed last-arg shape

**Severity:** medium (Phase-2 time bomb + spec-allowed calls would fail validation). **Status:** fixed.

R7RS §6.4 on `append`:

> Returns a list consisting of the elements of the first list followed by the elements of the other lists. [...] The last argument, if there is one, can be of any type.

So `(append '(1 2) 'x) → (1 2 . x)` is R7RS-valid.

Old registration (`registry/core/lists.go:35`):

```go
{Name: "append", ParamCount: 1, IsVariadic: true, ...
    ParamTypes: []values.TypeConstraint{values.TypeList}, ReturnType: values.TypeList},
```

For a `ParamCount: 1, IsVariadic: true` primitive (0 fixed + variadic rest), `ParamTypes[0]` describes the type of each rest-list element. Declaring `TypeList` says "every arg must be a list" — which is **strictly more restrictive than R7RS**.

When Extension Contracts Phase 2 wires `ParamTypes → SetValidator`, the call `(append '(1 2) 'x)` — R7RS-valid — would be rejected at validation time. The impl already handles this case correctly (at `prim_lists.go:78`, the last arg becomes the final cdr regardless of type).

### Fix

`ParamTypes[0]: TypeList → TypeAny`, with a comment explaining the widening. Per audit §6 Option A: soundness (accept spec-valid inputs) beats precision (reject spec-invalid inputs) when no refinement type can express "list for non-last args, any for last arg."

Non-last args that aren't lists are rejected by the impl's per-element `values.Tuple` assertion (`prim_lists.go:110–113`). Widening the ParamType doesn't change that; it just moves the check from validation time to execution time for the non-last args.

## Finding E.2 — mutable pair/vector literals, undocumented R7RS deviation

**Severity:** medium (real spec deviation, affects portability). **Status:** documented, not fixed.

R7RS §6.4 on `set-car!` and §6.8 on `vector-set!`: "It is an error to attempt to store in a literal."

R7RS §1.3.2 clarifies: "it is an error" cases are **not required to be detected** by implementations, though detection is encouraged.

Wile's behavior:

```scheme
(set-car! '(a b c) 999)        ; silently succeeds — mutates the literal
(vector-set! '#(1 2 3) 0 'x)   ; silently succeeds
(list-set! '(a b c) 1 'x)      ; silently succeeds (via set-car!)
(string-set! "abc" 0 #\X)      ; correctly raises an error
```

The asymmetry between pair/vector (mutable) and string (immutable) is the unusual part. Wile's `*values.String` has a dedicated immutability flag; `*values.Pair` and `*values.Vector` have no such flag.

Compounding factor — structure sharing across quoted literals:

```scheme
(eq? '(a b c) '(a b c))   ; => #t  (wile shares structure)
```

Two textually distinct `'(a b c)` literals in the same program are `eq?` and point to the same allocation. Mutating one is visible through the other. R7RS permits this sharing but combined with silent-mutation it creates nasty spooky-action-at-a-distance bugs.

### Why documentation, not a code fix

Adding immutability tracking to `*values.Pair` and `*values.Vector` requires:

1. New flag field on each type (+1 word per allocation — hot cost, given pairs are the most common allocation in Scheme)
2. Mutation primitives check the flag (`PrimSetCar`, `PrimSetCdr`, `PrimVectorSet`, `PrimVectorFill`, `PrimListSet`)
3. Compiler marks quoted-literal constants as immutable (`compile_quote.go` path — needs audit of whether quoted data is ever reused as scratch)
4. Regression tests for mutation error + correct construction path

4–8 hours minimum, with allocation cost on the hot path, for a deviation R7RS explicitly permits (§1.3.2).

Given no demand signal, the right action is to **document** the deviation so users know to construct with `list`/`cons`/`make-vector`/`vector-copy` before mutating. Done in `docs/reference/r7rs-differences.md` as deviation #4.

### What was *almost* added as finding E.3 but wasn't

- **`list-set!` ParamTypes[0] = `TypePair`** — correct. `list-set!` requires a `*values.Pair` because it uses `SetCar`, and the empty list is not a `*Pair`. Any non-empty list *is* a `*Pair` (the first cell). No leak.
- **`make-list` no-fill case returns `#f`** — R7RS says "unspecified". `#f` is a valid instantiation of "unspecified". Stricter than R7RS, not a deviation.
- **`length` on improper list raises** — R7RS §6.4 requires a proper list; raising is compliant.
- **`list-tail` with `k > (length list)`** — raises. R7RS §6.4: "It is an error if list has fewer than k elements." Detection is compliant.

## Positive verification (parallel-case pattern)

`memq` / `memv` / `assq` / `assv` have identical shape:

```
ParamCount: 2, ParamTypes: [TypeAny, TypeList], ReturnType: TypeAny
```

Consistent. No drift.

`member` and `assoc` are in `bootstrap_procedures.scm` (Scheme, not Go) because they accept an optional 3rd comparator that requires capturable continuation frames per registry/core CLAUDE.md. Out of Go-audit scope.

## Not-findings observed

- **`append` zero-arity**: `(append)` returns `()`. R7RS §6.4: "If no arguments are supplied, `()` is returned." ✓
- **`list-ref` and `list-tail` indexing**: 0-based. R7RS §6.4. ✓
- **`memq`/`memv`/`assq`/`assv` fail-match**: return `#f`. R7RS. ✓
- **`memq` on improper list**: raises. Stricter than some Schemes but R7RS-compliant ("it is an error if list is not a proper list").
- **`list-copy` is shallow**: documented. R7RS: "Returns a newly allocated copy of the given list, with the same elements." Elements shared. ✓

## Phase 4 scoreboard after 4 categories

| Category | Code findings | Doc findings |
|---|---|---|
| bytevectors | 2 | 0 |
| strings | 2 | 1 |
| ports | 0 | 2 |
| lists | 1 | 1 |

The code-finding count is now back up from 0 (ports) to 1 (lists) — not a declining pattern. Hypothesis revision: the true predictor is **file age and registration density**, not category size. Ports is young (recent FileResolver + embedding work); lists includes both recent Tier 5 refactoring (list-copy, list-set!) and legacy core (append, reverse, length).

## Next categories

- **characters** (R7RS §6.6) — small surface (~20), Unicode edge cases, parallel with strings.
- **numbers** (R7RS §6.2) — last. Largest surface and densest test coverage.
- **control** (R7RS §6.10) — small, semantically tricky.
- **exceptions** (R7RS §6.11) — small, covered by Phase 1 A.2.
- **records / promises** (R7RS §5.5, 4.2.5) — not R7RS-small core, but wile implements as SRFI-9 compatible. Worth a pass.
