# Primitive Annotation Audit — Phase 4 (Axis C) Findings: Control

**Status**: Complete. 0 findings.
**Category**: R7RS §6.10 Control features (8 primitives in `registry/core/control.go`; `dynamic-wind` is a compiled form, not a primitive).
**Plan**: `plans/2026-04-19-primitive-annotation-audit.md` — Phase 4 (Axis C).
**Prior categories**: bytevectors (2), strings (2+1), ports (0+2), lists (1+1), characters (2), exceptions (0+1 cross).

## Scope

| Primitive | R7RS | File |
|---|---|---|
| `apply` | §6.10 | `control.go` |
| `call-with-current-continuation`, `call/cc` | §6.10 | `control.go` |
| `values`, `call-with-values` | §6.10 | `control.go` |
| `call-with-exit` | Wile extension | `control.go` |
| `call-with-continuation-barrier` | Wile extension (Racket-compat) | `control.go` |
| `dynamic-wind` | §6.10 | compiled form (`machine/compile_validated.go`) |
| `procedure?` | §6.10 | `predicates.go` (cross-cat) |
| `map`, `for-each` | §6.10 | `bootstrap_procedures.scm` (Scheme) |

Prompt-tag / composable-continuation primitives (R7RS-large / Racket-compat) in `registry/core/prompts.go` live outside §6.10 strictly and are out of scope this session.

## Positive verification

Everything in §6.10 is R7RS-compliant:

- **`apply` arity**: minimum 2 args (proc + args-list). Both compile-time validation and runtime impl enforce. `(apply +)` caught at compile; indirect calls caught at runtime.
- **`apply` non-list final arg**: `(apply + 1 2 3)` raises "final argument must be a list". Correct per R7RS §6.10.
- **`apply` intermediates**: `(apply list 1 2 3 '(4 5)) → (1 2 3 4 5)` correctly prepends.
- **`values` zero-values**: `(call-with-values (λ () (values)) list) → ()`. Correct R7RS §6.10.
- **`values` single-value coercion**: `(+ 1 (values 2)) → 3`. Correct for the "one value where one expected" case.
- **`values` multi**: `(call-with-values (λ () (values 1 2 3)) +) → 6`. Correct.
- **`call-with-values` arity mismatch**: consumer arity ≠ producer count → raises. Correct.
- **`call/cc` type check**: `(call/cc 42) → error`. Correct.
- **`dynamic-wind` ordering**: before/body/after execute in R7RS order, including when captured continuations fire.
- **`call-with-continuation-barrier`**: successfully blocks post-body escape attempts. Correct per Racket semantics.
- **`call-with-exit`**: both early-exit and normal-return paths work.

## No class-recurrence

| Finding class | Recurred here? |
|---|---|
| B.1 (internal-type leak) | No — every ParamType is user-facing (Procedure/Any) |
| B.5 (docstring lie about error semantics) | No — all docstrings match impl |
| C.1 (ParamCount vs R7RS minimum arity) | No — `apply` ParamCount=2 is the correct "1 fixed + rest where rest is proc-args" shape |
| E.1 (variadic TypeList too strict) | No |
| F.1/F.2 (spec-enumerated categories missed) | N/A (no category enumeration in §6.10) |
| G.1 (asymmetric behavior within a type family) | No |

Phase 1's escape-mechanism unification (PR #418) produced clean, correct primitives with matching annotations. The category is in the post-refactor-maintained state — no drift.

## Phase 4 scoreboard after 7 categories

| Category | Code | Doc | Cross |
|---|---|---|---|
| bytevectors | 2 | 0 | — |
| strings | 2 | 1 | — |
| ports | 0 | 2 | — |
| lists | 1 | 1 | — |
| characters | 2 | 0 | — |
| exceptions | 0 | 0 | 1 |
| control | 0 | 0 | — |

**Running total**: 7 code + 4 doc + 1 cross-category, across 7 categories.

Code-finding distribution by category age/density:

| Density | Categories | Code findings |
|---|---|---|
| Dense-legacy | bytevectors, characters | 4 |
| Mixed | strings, lists | 3 |
| Recent/Homogeneous | ports, exceptions, control | 0 |

The pattern is cleaner now: **categories last touched by a dedicated cleanup PR tend to 0 findings**. Ports (Phase 1 B.4 file-resolver work + recent extraction), exceptions (Phase 1 A.2), and control (PR #418 UNIFY-ESCAPE-MECHANISMS) all hit zero.

Prediction for remaining categories:
- **numbers** (R7RS §6.2) — mixed history. Some types have recent cleanup (numeric tower PRs) but primitive surface is large and older. Expect 2–4 findings, likely concentrated in transcendental / conversion / exactness annotations.
- **records / promises** — SRFI-9 shape; records were recently touched (PR #566 OpaqueValue). Expect 0–1.

## Next

Likely sequence: **records/promises** (quick), then **numbers** (finale). Or straight to **numbers** if you prefer depth-first.
