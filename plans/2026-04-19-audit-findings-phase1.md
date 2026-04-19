# Primitive Annotation Audit — Phase 1 Findings

**Status**: Phase 2 triage complete. All 13 original findings resolved. Audit harness reports 0 findings on re-run (`prims=475 with-examples=251 examples=403 self-call=328 verified=328`).
**Harness**: repo-root `audit_annotations_test.go` (report-only; not in `registry/core/` where the primitives themselves live).
**Plan**: `plans/2026-04-19-primitive-annotation-audit.md`.

## Post-resolution note (2026-04-19)

All 13 findings below were resolved across commits `0c1e8cfa`, `bd2876c4`, `0e2c0138` (2026-04-18) and `da2a4fd9` (2026-04-19). Several findings (C.1, C.2) were reassessed as false positives — the original "stray `))`" description did not survive paren re-counting. The original categorization is preserved for historical reference.

Next work is Phase 3 (axis B tooling — static return-type analysis for branches no docstring example exercises) and Phase 4 (axis C — R7RS compliance, category by category). Neither is blocked on these findings.

## Summary (pre-resolution snapshot)

The table below is the original triage snapshot from the first harness run, before the 2026-04-18 fix batch and before the harness gained multi-line-example + `;;`-skip support in commit `212c534a`. Numbers differ from the post-resolution header above (`examples=403`, `verified=328`, `with-examples=251`) because the harness revision is different, not because the findings were unresolved. Kept for historical reference only; current state is the header.

| Bucket | Count (pre-resolution) |
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
**Status**: fixed in `0c1e8cfa`.

**A.2 `procedure-arity` on variadic foreign `+`** — `registry/core/prim_reflection.go`
Docstring example: `(procedure-arity +)  => (0 . #t)`. Actual: `(0 . #f)`.
The `#t` represents "variadic". `+` *is* variadic (registered with `IsVariadic: true` in `arithmetic.go:25`). The impl is dropping the variadic flag when computing arity for a foreign procedure. **Impl bug, not doc bug.**
**Status**: resolved as doc bug in `0e2c0138` — re-reading the impl, `(min . #f)` is the *correct* shape ("min required, no upper limit"); `#t` in the old example was backwards. Docstring rewritten to document the actual `integer | (min . #f)` convention.

**A.3 `procedure-type` on a lambda** — `registry/core/prim_reflection.go`
Docstring vocabulary: "closure, foreign, case-lambda, parameter, or continuation".
Actual: returns symbol `lambda` for `(lambda (x) x)`.
The returned symbol doesn't match the documented vocabulary. Either the impl should return `closure` or the vocabulary should be extended. **Likely impl bug.**
**Status**: fixed in `bd2876c4` — impl changed to return `closure` for `*machine.MachineClosure`, matching the docstring vocabulary. `TestProcedureType/closure` passes.

### Category B — docstring prose is stale (3)

**B.1 `inexact 1/3`** — `registry/core/arithmetic.go`
Docstring: `=> 0.3333333333333333`. Actual: `0.33333333333333333334` (BigFloat, more precision).
Impl returns BigFloat for exact-rational inputs; docstring reflects a historical Float return. **Doc fix.**
**Status**: fixed in `0e2c0138` — example switched to `(inexact 1/4)` → `0.25`, which is exactly representable in both Float and BigFloat and round-trips cleanly.

**B.2 `namespace-name (make-namespace)`** — `extensions/namespace/...`
Docstring: `=> #f`. Actual: `"namespace"` (String).
Default name seems to have changed; docstring wasn't updated. **Doc fix.**
**Status**: fixed in `0e2c0138` — example + prose updated to reflect `"namespace"` default.

**B.3 `procedure-arity car`** — `registry/core/reflection.go`
Docstring example: `=> (1 . #f)` (a pair). Actual: `*values.Integer:1`.
Inconsistent with `procedure-arity`'s own comment ("returns a pair for ordinary closures ... an integer for composable continuations"). `car` is a foreign closure, so why does it return an integer?
Either **impl bug** (foreign closures should also return a pair) or **impl inconsistency** (the pair/integer/list/#f union is wider than documented in the code comment). Related to A.2.
**Status**: fixed in `0e2c0138` — docstring rewritten to match actual behavior: fixed arities return an integer; variadic arities return `(min . #f)`. Integer-for-`car` is now documented, not anomalous.

### Category C — docstring typos / incomplete examples (6 originally; post-triage: 4 C-shaped, 1 A-shaped, 2 false positives)

**C.1–C.3 Continuation-mark example claims reassessed.**
The original finding described these as "stray `))` at end of example." Re-verification showed parens balance in all three. Running each example individually:
- `call-with-immediate-continuation-mark` — works (returns 42).
- `continuation-mark-set->list` — works (returns `(1)`).
- `continuation-mark-set-first` — **fails**, but not as a typo: the example passes `#f` as `mark-set`, and the impl rejects non-mark-set values. This is a semantic bug in the example (Category A shape), not a paren typo.
**Fix applied**: `continuation-mark-set-first` example updated to pass `(current-continuation-marks)` as the mark-set (see `registry/core/cont_marks.go`). The other two left as-is — they worked.
Root cause of the original mis-classification: the audit harness does not verify wrapped examples (top form ≠ primitive), so Phase 1 triage inspected by eye. Lesson logged. Fix scope: axis A harness extension (multi-form eval of wrapped examples), not axis B (which is annotation ↔ impl static analysis — a separate gap).

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
**Status**: fixed in `0e2c0138` — example prefixed with `;;` and annotated with an "opaque return" note so the harness skips it.

## Resolution summary

All 13 findings resolved. Harness state: **0 findings** (`prims=475 with-examples=251 examples=403 self-call=328 verified=328`).

| Finding | Resolution | Commit |
|---|---|---|
| A.1 `bytevector-u8-ref` | `TypeByte` → `TypeInteger` | `0c1e8cfa` |
| A.2 `procedure-arity +` | Docstring rewritten; `(0 . #f)` is the correct shape | `0e2c0138` |
| A.3 `procedure-type lambda` | Impl changed to return `closure` for `*MachineClosure` | `bd2876c4` |
| B.1 `inexact 1/3` | Switched to `(inexact 1/4)` | `0e2c0138` |
| B.2 `namespace-name` | Docstring updated to `"namespace"` | `0e2c0138` |
| B.3 `procedure-arity car` | Docstring rewritten to document integer-or-pair union | `0e2c0138` |
| C.1 `call-with-immediate-continuation-mark` | False positive — example already correct | — |
| C.2 `continuation-mark-set->list` | False positive — example already correct | — |
| C.3 `continuation-mark-set-first` | `#f` → `(current-continuation-marks)` | `da2a4fd9` |
| C.4–C.6 `error-context-*` | `;;` skip-marker prefix | `0e2c0138` |
| D.1 `read-token` | `;;` skip-marker prefix | `0e2c0138` |

Phase 2 closed. Next work is Phase 3 (axis B — static return-type analysis) per the parent plan.

## What Phase 1 did not catch (by design)

- **Wrapped examples** — top form ≠ primitive. The harness only verifies self-call examples. Fix is an axis A harness extension: eval multi-form examples and isolate the primitive's actual call. Pre-resolution snapshot had 72; post-resolution self-call count is 328 of 403 examples, so ~75 remain wrapped.
- **Primitives with no examples** — roughly half the 475-primitive surface. Post-resolution: 251 have examples, ~224 do not. Axis B (static return-path analysis, Phase 3) is the right tool here: it can verify annotations against impl return paths without requiring a docstring example.
- **ParamType annotations** — only ReturnType is checked. Could be extended with a second pass that fuzzes input types against declared `ParamTypes`.
