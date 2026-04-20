# Primitive Annotation Audit — Phase 4 (Axis C) Findings: Records & Promises

**Status**: Complete. 0 code findings, 0 doc findings.
**Category**: R7RS §5.5 Records + R7RS §4.2.5 Promises (13 primitives in `internal/extensions/all/register.go` `addRecords` + `addPromises`).
**Plan**: `plans/2026-04-19-primitive-annotation-audit.md` — Phase 4 (Axis C).
**Prior categories**: bytevectors (2), strings (2+1), ports (0+2), lists (1+1), characters (2), exceptions (0+1 cross), control (0).

## Scope

### Records — R7RS §5.5 (SRFI-9 style)

| Primitive | R7RS source |
|---|---|
| `make-record-type` | helper (SRFI-9 style) |
| `record-type?`, `record?`, `record-type` | predicates + accessor |
| `record-constructor`, `record-predicate`, `record-accessor`, `record-modifier` | generators |
| `make-opaque-record-type` | Wile extension |

`define-record-type` (R7RS §5.5) is a macro in `bootstrap_macros.scm` that expands to these helpers. It's out of Go-audit scope.

### Promises — R7RS §4.2.5

| Primitive | R7RS source |
|---|---|
| `promise?`, `make-promise`, `force` | §4.2.5 |
| `%make-lazy-promise` | internal (used by `delay` macro) |

`delay`, `delay-force` (R7RS §4.2.5) are macros in `bootstrap_macros.scm`.

## Verification

### Records

All R7RS §5.5 + Wile opaque-records behaviors verified:

- **Constructor/predicate/accessor/modifier round-trip**: type + constructor + predicate + accessor + modifier all cooperate correctly. `(pt? p1) → #t`, `(get-x p1) → 3`, `(set-x! p1 99)` then `(get-x p1) → 99`.
- **`record?` returns `#t` for non-opaque records and `#f` for other values**, including opaque records. Matches docstring.
- **`record-type`** returns the correct `RecordType` for non-opaque records and **errors on opaque records**. Matches docstring "Errors if the record's type is opaque."
- **`record-constructor` on non-rtd first arg**: raises. Correct. (`TypeAny` ParamType relies on impl-level `RequireArg` for validation — see known-gap note below.)
- **`record-accessor` on missing field**: raises. Correct.
- **Opaque records**: `record?` returns `#f`, `record-type` raises, but constructor/accessor/modifier still work. The opacity is visibility-only, not functional.

### Promises

All R7RS §4.2.5 behaviors verified:

- **`(force (delay (+ 1 2))) → 3`** basic evaluation ✓
- **`(force 42) → 42`** non-promise returns unchanged per docstring ✓
- **`(make-promise p)` on existing promise returns the same object (eq?)** ✓
- **Memoization**: `(let ((p (delay (begin (set! c (+ c 1)) c)))) (force p) (force p))` — `c` incremented exactly once. R7RS-compliant.
- **`delay-force` tail-promise chain**: `(force (delay-force (delay 42))) → 42`. Correct per R7RS §4.2.5 (avoids unbounded recursion in chained promises).
- **`(force (delay (delay 99))) → 99`**: nested delay chain fully forced. Correct R7RS semantics.
- **`promise?`**: true for `(delay ...)` and `(make-promise ...)`, false for plain values.

## Known gap (not a new finding)

The `addRecords` registration carries an explicit TODO comment (`register.go:77`):

> TODO(contracts): `*values.Record` and `*values.RecordType` have no ValueType enum entries, so record-* primitives fall back to TypeAny for those positions. Impl-level RequireArg still rejects mismatches.

Every record-argument position (rtd, record instance) declares `TypeAny` instead of a record-specific type. This is a **TypeConstraint vocabulary gap**, not an annotation lie — impl-level validation is correct. The fix is vocabulary extension: add `TypeRecord`, `TypeRecordType` enum entries.

This is already captured strategically:

- `plans/2026-04-19-axis-b-inventory.md` §6 defers TypeConstraint vocabulary extensions to a separate plan.
- Record-type arguments would follow the axis-b decision (widen to `TypeAny` for now, catalog for future `TypeRecord` / `TypeRecordType` constructors).

No action this session.

## No class-recurrence

| Finding class | Recurred? |
|---|---|
| B.1 (internal-type leak) | No — record TypeAny is a known gap, not a leak |
| B.5 (docstring lie about errors) | No — every error promise delivered |
| C.1 (ParamCount vs R7RS minimum) | No |
| E.1 (variadic TypeList too strict) | No |
| F.1/F.2 (spec-enumerated categories missed) | No |
| G.1 (asymmetric behavior) | No |

PR #566 (OpaqueValue) and SRFI-9 record work held up cleanly.

## Phase 4 scoreboard after 8 categories

| Category | Code | Doc | Cross |
|---|---|---|---|
| bytevectors | 2 | 0 | — |
| strings | 2 | 1 | — |
| ports | 0 | 2 | — |
| lists | 1 | 1 | — |
| characters | 2 | 0 | — |
| exceptions | 0 | 0 | 1 |
| control | 0 | 0 | — |
| records/promises | 0 | 0 | — |

**Running total**: 7 code + 4 doc + 1 cross-category, across 8 categories.

Prediction accuracy check: predicted 0–1 for this category; result 0. Four of eight categories produced zero code findings. The three "zero" categories after Phase 1 cleanup (ports, exceptions, control) plus this one (records/promises, where the structural issue is already captured as a TODO) form a cluster — they share the property that a recent dedicated cleanup PR touched the surface.

## One category remaining

**numbers** (R7RS §6.2) — the finale. Predicted 2–4 code findings. Dense surface, mixed-age history (numeric tower PRs recent but many primitives date to early Wile). Schedule whenever depth-first focus is available.
