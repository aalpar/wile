# Primitive Annotation Audit — Phase 4 (Axis C) Findings: Bytevectors

**Status**: Complete. 2 findings, both resolved.
**Category**: R7RS §6.9 Bytevectors (11 primitives).
**Plan**: `plans/2026-04-19-primitive-annotation-audit.md` — Phase 4 (Axis C: implementation vs published standard), one category per session.
**Prior phases**: Axis A (example harness) — 0 live findings. Axis B (wile-goast SSA narrowing) — 0 new annotation lies; recommendations in `plans/2026-04-19-axis-b-inventory.md:566`.

## Scope

R7RS §6.9 primitives, as registered in `registry/core/byte_vectors.go`:

| Primitive | Arity | R7RS domain | R7RS codomain |
|---|---|---|---|
| `bytevector?` | 1 | any | boolean |
| `make-bytevector` | 1–2 | k, byte | bytevector |
| `bytevector` | 0+ | byte ... | bytevector |
| `bytevector-length` | 1 | bytevector | exact-integer |
| `bytevector-u8-ref` | 2 | bytevector, k | byte |
| `bytevector-u8-set!` | 3 | bytevector, k, byte | unspecified |
| `bytevector-copy` | 1–3 | bytevector [start [end]] | bytevector |
| `bytevector-copy!` | 3–5 | to, at, from [start [end]] | unspecified |
| `bytevector-append` | 0+ | bytevector ... | bytevector |
| `utf8->string` | 1–3 | bytevector [start [end]] | string |
| `string->utf8` | 1–3 | string [start [end]] | bytevector |

R7RS semantic point: "byte" is **not a distinct type**; it is an exact integer in range [0, 255]. Wile has an internal `*values.Byte` type used for in-vector storage, but there is no Scheme-level constructor for it — every user-facing "byte" argument is an exact integer.

## Finding B.1 — `TypeByte` annotations on user-facing byte parameters

**Severity:** high (Phase-2 time bomb). **Status:** fixed.

Three `ParamTypes` entries declared `values.TypeByte` for parameters that the implementation reads as `*values.Integer`:

| Primitive | Position | Before | After | Impl site |
|---|---|---|---|---|
| `make-bytevector` | param 1 (fill byte) | `TypeByte` | `TypeExactInteger` | `OptionalArg[*values.Integer]` (`prim_byte_vectors.go:36`) |
| `bytevector` | param 0 (rest of bytes) | `TypeByte` | `TypeExactInteger` | `RequireType[*values.Integer]` via `ForEach` (`prim_byte_vectors.go:68`) |
| `bytevector-u8-set!` | param 2 (byte value) | `TypeByte` | `TypeExactInteger` | `RequireType[*values.Integer]` (`prim_byte_vectors.go:111`) |

### Why this lied

`values.TypeByte` maps to `makeCheck[*values.Byte]` (`values/value_type.go:216`) — it only matches the internal `*values.Byte` type. Scheme integer literals (`0`, `42`, `255`) parse as `*values.Integer`. There is no Scheme-level primitive that constructs a `*values.Byte`; the type is purely internal to bytevector storage.

### Why it's load-bearing for Phase 2

Tier 2 `Extension API contracts Phase 2` (`plans/2026-03-26-extension-contracts-phase2-impl.md`) wires `ParamTypes` → `SetValidator`. The moment that lands, every correct call — `(bytevector 1 2 3)`, `(make-bytevector 10 0)`, `(bytevector-u8-set! bv 0 42)` — starts being **wrongly rejected** because the argument is `*values.Integer` where `*values.Byte` is required.

Runtime range validation (0–255) continues to live in `values.ValidateByteValue` (`values/byte.go:70`). That is an **invariant check**, not a type check; it logically belongs at the impl level because the type vocabulary has no refinement-type constructor. If `TypeExactIntegerInRange(0,255)` is ever added (a refinement constraint — strict superset of the `TypeMaybe` case from axis-b §6), `ValidateByteValue` becomes dead.

### Fix

Commits 1 edit to `registry/core/byte_vectors.go`. Verified against `TestAuditPrimitiveAnnotations` — zero findings after change (328 verified).

## Finding B.5 — `utf8->string` silently accepts invalid UTF-8

**Severity:** medium (spec deviation + docstring lie). **Status:** fixed.

R7RS §6.9 on `utf8->string`:

> It is an error if bytevector between start and end is not a well-formed UTF-8 string.

The existing docstring (`registry/core/byte_vectors.go:53`) makes the promise explicit:

> Decodes BYTEVECTOR as UTF-8 to a string from START to end. Raises an error on invalid UTF-8.

The existing implementation (`registry/core/prim_byte_vectors.go:227–231` before fix) performed `string(bytes)`, which is Go's byte-sequence-to-string conversion — it preserves invalid UTF-8 rather than rejecting it. Concrete reproducer:

```scheme
(utf8->string #u8(255 254 253))   ; => "\xff\xfe\xfd"   — no error raised
```

Subsequent character-level operations on that string produce `U+FFFD REPLACEMENT CHARACTER` silently, so errors surface far from the cause.

### Fix

Validate with `unicode/utf8.Valid` before converting:

```go
if !utf8.Valid(bytes) {
    return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
        "utf8->string: bytevector is not well-formed UTF-8")
}
mc.SetValue(values.NewString(string(bytes)))
```

Added `TestUtf8ToStringInvalid` table covering 7 invalidity classes:

1. Lone continuation byte (`#x80`)
2. Two lone continuation bytes (`255 254 253`)
3. Truncated 2-byte sequence (`#xc3`)
4. Truncated 3-byte sequence (`#xe4 #xbd`)
5. Truncated 4-byte sequence (`#xf0 #x9f #x98`)
6. Overlong encoding (`#xc0 #x80`)
7. Surrogate half `U+D800` (`#xed #xa0 #x80`)

All seven raise. Go's `utf8.Valid` handles each correctly by construction.

### What this does *not* change

- `string->utf8` is unaffected. Scheme strings are well-formed Unicode by construction in wile (`*values.String` wraps a Go `string` produced via validated rune-level APIs), so encoding cannot fail.
- Surrogate-pair behavior in other primitives (`char->integer`, `integer->char`, `string-ref` on strings with surrogates) is out of scope for this category.

## Not-findings

### Annotations that looked wrong but aren't

- **`bytevector-u8-ref` declared `TypeInteger`** — R7RS §6.9 says "returns the k-th byte". "byte" in R7RS is an integer in [0,255], not a distinct type. `TypeInteger` is sound. (Was fixed from `TypeByte` in PR commit `0c1e8cfa` during Phase 1.)
- **`bytevector-length` declared `TypeExactInteger` narrowed `integer`** — `TypeExactInteger` is an alias for `TypeInteger` (`values/value_type.go:201`). Cosmetic axis-B output quirk.

### Spec-deviations flagged and dismissed

- **Asymmetry `bytevector-u8-ref` returns `*Integer` vs `bytevector-u8-set!` formerly took `*Byte`** — the ref side was already `*Integer` end-to-end; the asymmetry was entirely on the set side. B.1 closes it.
- **`bytevector-copy!` overlap handling** — R7RS §6.9 suggests correct handling of overlapping source/destination regions. Go's `copy()` on slices handles this correctly by construction (it detects overlap and chooses direction). Verified by inspection of `prim_byte_vectors.go:171`. No action.
- **`bytevector-append` zero-arity** — R7RS §6.9 does not specify zero-arity explicitly; wile returns `#u8()`. Consistent with `(string-append) → ""` and `(vector-append) → #()`. No action.

## Phase 4 methodology notes (for future sessions)

The bytevector category produced two findings in approximately 45 minutes of single-session work. Axis-B inventory was the best starting point — the `TypeByte` entries stood out immediately when scanning declared-vs-narrowed in the Single bucket. Recommended shape for subsequent categories:

1. **Enumerate R7RS primitives in category** — one table, one row per primitive, arity + domain + codomain.
2. **Walk registration file** — check each `ParamTypes` entry against `makeCheck[T]` to catch user-facing type references to internal types (the B.1 class).
3. **Walk docstring examples** — the harness catches contradictions, but the prose around `=>` sometimes promises error semantics (the B.5 class) that code reviewers missed. These are the hardest to find mechanically.
4. **Spot-verify suspicious impls with `mcp__wile__eval`** — 2 evals confirmed both findings before any code change.
5. **Diff count kept small** — 1 file edit + 1 test addition + 1 doc. Commit discipline: per-finding atomicity.

Next categories to schedule: **strings** (R7RS §6.7; known Unicode case-mapping inconsistency per `internal/extensions/all/CLAUDE.local.md` gotchas), **ports** (R7RS §6.13; `char-ready?`/`u8-ready?` documented deviation plus untested edge cases), **numbers** (R7RS §6.2; largest surface, most existing coverage — schedule late).
