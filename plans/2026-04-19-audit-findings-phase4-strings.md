# Primitive Annotation Audit — Phase 4 (Axis C) Findings: Strings

**Status**: Complete. 3 findings, all resolved.
**Category**: R7RS §6.7 Strings (18 primitives across `registry/core/strings.go` + `internal/extensions/all/prim_strings.go`).
**Plan**: `plans/2026-04-19-primitive-annotation-audit.md` — Phase 4 (Axis C: implementation vs published standard).
**Prior category**: bytevectors (`plans/2026-04-19-audit-findings-phase4-bytevectors.md`).

## Scope

R7RS §6.7 primitives in wile:

**Core (`registry/core/strings.go`):**

| Primitive | Arity | R7RS section |
|---|---|---|
| `string?` | 1 | predicate (core/predicates.go) |
| `string` | 0+ | §6.7 |
| `make-string` | 1–2 | §6.7 |
| `string-length` | 1 | §6.7 |
| `string-ref` | 2 | §6.7 |
| `string-set!` | 3 | §6.7 |
| `substring` | 3 | §6.7 |
| `string-copy` | 1–3 | §6.7 |
| `string-append` | 0+ | §6.7 |
| `string->list` | 1–3 | §6.7 |
| `list->string` | 1 | §6.7 |
| `symbol->string` | 1 | §6.5 (cross-ref) |
| `string->symbol` | 1 | §6.5 (cross-ref) |
| `string=?`/`<?`/`>?`/`<=?`/`>=?` | 2+ | §6.7 |

**Extended (`internal/extensions/all/prim_strings.go`):**

| Primitive | Arity | R7RS section |
|---|---|---|
| `string-copy!` | 3–5 | §6.7 |
| `string-fill!` | 2–4 | §6.7 |
| `string-upcase` | 1 | §6.7 |
| `string-downcase` | 1 | §6.7 |
| `string-foldcase` | 1 | §6.7 |
| `string-ci=?`/`<?`/`>?`/`<=?`/`>=?` | 2+ | §6.7 |

## Finding C.0 — stale CLAUDE.md claim about `string-ci=?`

**Severity:** low (doc drift only). **Status:** fixed.

`internal/extensions/all/CLAUDE.local.md` claimed:

> **string-ci=?**: Uses `strings.EqualFold` which does full case folding, while other string-ci comparisons use `strings.ToLower` (simple case mapping). This can produce inconsistent results for edge cases

Current code (`prim_strings.go:162–166`) uses `getCaseFolded` (which wraps `golang.org/x/text/cases.Fold()`) uniformly across all five `string-ci*?` comparisons. `strings.EqualFold` and `strings.ToLower` appear nowhere in the package.

### Verified consistency

```scheme
(string-ci=? "straße" "STRASSE")   ; => #t  (ß folds to "ss")
(string-ci<? "straße" "STRASSE")   ; => #f  (equal under fold → not strictly less)
```

Both comparisons agree because both call `getCaseFolded` first.

### Fix

Two line edits in `CLAUDE.local.md`: updated the tabulated entry and the "Gotchas" section to reflect current reality. No code change.

## Finding C.1 — `string-copy!` declared `ParamCount=2` but requires 3 fixed args

**Severity:** medium (Phase-2 readiness + inconsistency). **Status:** fixed.

R7RS §6.7: `(string-copy! to at from [start [end]])` — 3 required, 2 optional.

Old registration (`internal/extensions/all/register.go:141`):

```go
{Name: "string-copy!", ParamCount: 2, IsVariadic: true, ...
    ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeAny}, ...}
```

`ParamCount: 2, IsVariadic: true` means "1 fixed arg + variadic rest" per the FFI convention. So `at` and `from` were packed into the rest list alongside optional `start`/`end`, and the impl had to unpack them manually (15 lines of tuple navigation).

### Parallel case (already correct)

`bytevector-copy!` — same R7RS shape — was already registered as `ParamCount: 4, IsVariadic: true` with 4 `ParamTypes` slots. That's the reference pattern.

### Concrete consequences

1. **Arity error lives at impl level, not dispatch level.** Old error path: impl inspects rest list, raises if empty. New error path: VM dispatch catches "expected at least 3, got N" before the impl runs.

2. **Phase-2 type validation loses precision on `at` and `from`.** With the old `ParamTypes: [TypeString, TypeAny]`, validation only checks `to` as string; `at` and `from` are typed `TypeAny` because they live in the rest list's untyped bag. With the new `[TypeString, TypeExactInteger, TypeString, TypeExactInteger]`, all three required positions carry their real constraints.

### Fix

Registration: `ParamCount: 2 → 4`, 4 `ParamTypes` slots, 4 `ParamNames`.
Impl: rewrote to use `RequireArg[T]` at positions 0–2, `mc.Arg(3)` as the rest for `[start [end]]`. Dropped 15 lines of tuple-unpacking boilerplate.

Existing test suite (`TestStringCopyTo`, 11 success + 5 error cases) passes unchanged.

## Finding C.2 — `string-fill!` declared `ParamCount=2` but requires 2 fixed args

**Severity:** medium (same class as C.1). **Status:** fixed.

R7RS §6.7: `(string-fill! string fill [start [end]])` — 2 required, 2 optional.

Old registration (`internal/extensions/all/register.go:144`):

```go
{Name: "string-fill!", ParamCount: 2, IsVariadic: true, ...
    ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeAny}, ...}
```

`ParamCount: 2, IsVariadic: true` = "1 fixed + rest". `fill` was packed into the rest list. Same class as C.1.

### Fix

Registration: `ParamCount: 2 → 3`, 3 `ParamTypes` slots (`TypeString, TypeCharacter, TypeExactInteger`), 3 `ParamNames` (`string, fill, start`).
Impl: use `RequireArg[T]` at positions 0–1, `mc.Arg(2)` as the rest for `[start [end]]`.

Existing test suite (`TestStringFill`, 5 success + 4 error cases) passes unchanged.

## Not-findings

### R7RS behaviors verified present

- **`string-set!` on literal raises** — `(string-set! "hello" 0 #\H)` raises per R7RS "It is an error to attempt to store in a literal string." Verified.
- **`list->string` rejects non-character elements** — R7RS "It is an error if any element of list is not a character." Verified.
- **`make-string` with no fill char** — default-fills rather than leaving contents unspecified. Stricter than R7RS (R7RS leaves contents unspecified), but compatible: any concrete filler is a valid instantiation of "unspecified".
- **`string-copy` and `substring` both return mutable strings** — R7RS doesn't mandate mutability either way. `string-copy` docstring advertises "mutable copy"; `substring` docstring is silent but behavior is consistent.
- **`substring` is strict 3-arg** — R7RS §6.7 defines only the 3-arg form. Wile matches (R7RS-small does not define `substring [start]` or `substring [start end]` variants).

### Unicode case-mapping considerations out of scope

- `string-upcase`, `string-downcase`, `string-foldcase` use `golang.org/x/text/cases` for full Unicode case mapping, which can change string length (e.g., `ß → SS`). That's R7RS-compliant (R7RS §6.7: "If the argument... contains no uppercase/lowercase letters... may be returned rather than a newly allocated copy").
- Consistent across the `ci` comparison family (see C.0).
- Character-level case mapping (`char-upcase`, `char-downcase`, etc.) lives in §6.6 and is out of scope for this session.

### The ParamType→internal-type bug class (B.1) does not recur

Every user-facing byte/character/int argument uses the appropriate user-facing `TypeConstraint`. No `TypeByte`-style leaks in strings.

## Phase 4 methodology — cumulative lessons

Across bytevectors + strings (two sessions, two hours total), the most productive lenses have been:

1. **ParamType scan against impl `RequireArg[T]`** — surfaces B.1-class bugs (annotation declares T but impl extracts U).
2. **ParamCount vs R7RS minimum arity** — surfaces C.1/C.2-class bugs (dispatch-level arity enforcement absent).
3. **Parallel-case comparison** — whenever two primitives share R7RS shape (e.g., `bytevector-copy!` ↔ `string-copy!`, `bytevector-fill!` would parallel `string-fill!`), inconsistent registration is almost always a real finding.
4. **Stale CLAUDE.md gotchas** — low-value per finding, but easy to verify and fix in the same pass.

Next categories ordered by expected signal density:

- **ports** (R7RS §6.13) — known `char-ready?`/`u8-ready?` deviation (documented); parallel shape between textual and binary ports invites C.1/C.2-style drift; large surface (~30 primitives).
- **characters** (R7RS §6.6) — Unicode case mapping edge cases, parallel family with strings.
- **numbers** (R7RS §6.2) — largest surface; schedule last because existing test coverage is densest.
- **lists** (R7RS §6.4) — largely mechanical but `list-tail`/`assq`/`member` have R7RS edge cases.
