# Primitive Annotation Audit — Phase 1 Findings

**Status**: First pass complete. 13 findings from 321 verified examples across 475 primitives.
**Harness**: `audit_annotations_test.go` (report-only).
**Plan**: `plans/2026-04-19-primitive-annotation-audit.md`.

## Summary

| Bucket | Count |
|---|---|
| Verified (type and value both match) | 321 |
| Type-mismatch (annotation is wrong) | 1 |
| Value-mismatch (docstring example is stale or prose is wrong) | 6 |
| Eval-error (example can't run as-is) | 6 |
| Opaque `#<...>` expected (skipped, trusted type check) | 9 |
| Wrapped examples (top form isn't the primitive) | 72 |

Roughly **14% of eligible examples are wrapped** (tests a wrapper, not the primitive itself). These are "uncheckable by this harness" but not errors — they exercise the primitive indirectly.

## The 13 findings, categorized

### Category A — real bugs in code or annotation (3)

**A.1 `bytevector-u8-ref`** — `registry/core/byte_vectors.go`
Declared `ReturnType: TypeByte`. Actual returns `*values.Integer:20` for `(bytevector-u8-ref #u8(10 20 30) 1)`.
R7RS §6.9: "byte values are integers 0..255" — the impl is correct per spec, the annotation lies.
**Fix**: change `ReturnType: values.TypeByte` → `values.TypeInteger`. Possibly tighten the param-type for the bytevector arg too.

**A.2 `procedure-arity` on variadic foreign `+`** — `registry/core/prim_reflection.go`
Docstring example: `(procedure-arity +)  => (0 . #t)`. Actual: `(0 . #f)`.
The `#t` represents "variadic". `+` *is* variadic (registered with `IsVariadic: true` in `arithmetic.go:25`). The impl is dropping the variadic flag when computing arity for a foreign procedure. **Impl bug, not doc bug.**

**A.3 `procedure-type` on a lambda** — `registry/core/prim_reflection.go`
Docstring vocabulary: "closure, foreign, case-lambda, parameter, or continuation".
Actual: returns symbol `lambda` for `(lambda (x) x)`.
The returned symbol doesn't match the documented vocabulary. Either the impl should return `closure` or the vocabulary should be extended. **Likely impl bug.**

### Category B — docstring prose is stale (3)

**B.1 `inexact 1/3`** — `registry/core/arithmetic.go`
Docstring: `=> 0.3333333333333333`. Actual: `0.33333333333333333334` (BigFloat, more precision).
Impl returns BigFloat for exact-rational inputs; docstring reflects a historical Float return. **Doc fix.**

**B.2 `namespace-name (make-namespace)`** — `extensions/namespace/...`
Docstring: `=> #f`. Actual: `"namespace"` (String).
Default name seems to have changed; docstring wasn't updated. **Doc fix.**

**B.3 `procedure-arity car`** — `registry/core/reflection.go`
Docstring example: `=> (1 . #f)` (a pair). Actual: `*values.Integer:1`.
Inconsistent with `procedure-arity`'s own comment ("returns a pair for ordinary closures ... an integer for composable continuations"). `car` is a foreign closure, so why does it return an integer?
Either **impl bug** (foreign closures should also return a pair) or **impl inconsistency** (the pair/integer/list/#f union is wider than documented in the code comment). Related to A.2.

### Category C — docstring typos / incomplete examples (6)

**C.1–C.3 Continuation-mark example claims reassessed.**
The original finding described these as "stray `))` at end of example." Re-verification showed parens balance in all three. Running each example individually:
- `call-with-immediate-continuation-mark` — works (returns 42).
- `continuation-mark-set->list` — works (returns `(1)`).
- `continuation-mark-set-first` — **fails**, but not as a typo: the example passes `#f` as `mark-set`, and the impl rejects non-mark-set values. This is a semantic bug in the example (Category A shape), not a paren typo.
**Fix applied**: `continuation-mark-set-first` example updated to pass `(current-continuation-marks)` as the mark-set (see `registry/core/cont_marks.go`). The other two left as-is — they worked.
Root cause of the original mis-classification: the audit harness does not verify wrapped examples (top form ≠ primitive), so Phase 1 triage inspected by eye. Lesson logged; Phase 3 tooling (axis B) should verify wrapped examples automatically.

**C.4–C.6 Undefined `ctx` placeholder** — three error-context primitives:
- `error-context-marks`
- `error-context-source`
- `error-context-stack-trace`
Examples reference free variable `ctx` which isn't bound anywhere.
**Fix**: change to semicolon-prefixed sketches (`;; (error-context-marks ctx)  =>  #f`) so the harness skips them, or restructure as a self-contained example.
**Status**: applied in commit `0e2c0138` (docs(registry): fix docstring drift surfaced by primitive audit).

### Category D — harness limitation (1)

**D.1 `read-token`** — `internal/extensions/io/...`
Docstring: `=> hello`. Actual: `*tokenizer.SimpleToken:<simple-token "hello" {0 0 0}:{5 5 0} 69>`.
`read-token` returns a Go-side token object with a rich display. The docstring simplified it to just the lexeme. This is a harness limitation: the token object's `SchemeString` doesn't round-trip through the reader and the example was written informally.
**Either**: make the example `(token-text (read-token ...))  => "hello"`, or accept as a known wrapped case.

## Recommended sequence

Category C is the shortest path to a clean first signal (trivial textual fixes). Category A needs investigation — each finding is a micro-bug with its own root cause. Category B is between. Category D is one line of docstring or a harness exception.

Suggested order:
1. **C** — *complete*. C.4–C.6 fixed in `0e2c0138`. C.1–C.3 reassessed: C.1/C.2 were false positives, C.3 fixed (`current-continuation-marks` substituted for `#f`).
2. **B** — *complete*. B.1 (`inexact 1/3` → `inexact 1/4`), B.2 (`namespace-name`), B.3 (`procedure-arity`) all fixed in `0e2c0138`.
3. **D** — *complete*. D.1 (`read-token`) fixed in `0e2c0138` via `;;` skip-marker prefix.
4. **A.1** — fix `bytevector-u8-ref` annotation (one-line annotation change + possibly a comment). **Open.**
5. **A.2, A.3** — investigate `procedure-arity` and `procedure-type`. These are related (both in `prim_reflection.go`) and may share a root cause. **Open.**

Current audit harness state: **0 findings** from self-call verification (`prims=475 with-examples=251 examples=403 self-call=328 verified=328`). Remaining work is Category A (impl bugs, not doc drift) and Phase 3 wrapped-example coverage.

## What Phase 1 did not catch (by design)

- 72 wrapped examples — top form isn't the primitive itself. Phase 3 addresses via impl-side analysis.
- 221 primitives with no examples. Report from `primsWithExamples=254` vs `totalPrims=475`. About half the surface. Phase 3's static analysis covers this gap.
- ParamType annotations — only ReturnType is checked. Could be extended with a second pass that fuzzes input types against declared `ParamTypes`.
