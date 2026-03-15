# Remove Symbol Interning Implementation Plan

**Status:** Complete
**Verified:** 2026-03-14 — all tasks confirmed against current sources; zero `InternSymbol` references in Go source

**Goal:** Remove symbol canonicalization (interning) and compare symbols by string key instead of pointer identity.

**Architecture:** `eq?`/`eqv?`/`memq`/`assq` compare `*Symbol` by `.Key` string via `helpers.EqIdentity`. `InternSymbol` removed from all environment types. `symbolInterns` map + mutex removed from `TopLevelEnvironment`. `SymbolInterner` interface deleted. ~50 call sites removed.

**Benchmark evidence:** `InternThenEq` 16.9 ns/op vs `AllocThenStringEqEscaped` 14.1 ns/op — net 17% faster amortized. See `environment/intern_bench_test.go`.

---

## Task Summary

| Task | Description | Status |
|------|-------------|--------|
| 1 | Add `helpers.EqIdentity` + symbol case to `Eqv` | Complete |
| 2 | Wire `eq?`, `memq`, `assq` to use `EqIdentity` | Complete |
| 3 | Remove `InternSymbol` from environment types | Complete |
| 4 | Remove all ~50 `InternSymbol` call sites | Complete |
| 5 | Update `native_template.go` stale comment | Complete |
| 6 | Clean up tests and benchmarks | Complete |
| 7 | Update documentation | Complete |

---

## Task 1: Add `helpers.EqIdentity` — Complete

**Evidence:** `EqIdentity` at `registry/helpers/equality.go:89`. Compares symbols by `.Key` field, all other types by pointer identity. `Eqv` has `*values.Symbol` case at line 80.

---

## Task 2: Wire `eq?`, `memq`, `assq` — Complete

**Evidence:**
- `PrimEqQ` uses `helpers.EqIdentity` (`registry/core/prim_equality.go:29`)
- `PrimMemq` passes `helpers.EqIdentity` to `MemberLookup` (`registry/core/prim_lists.go:344`)
- `PrimAssq` passes `helpers.EqIdentity` to `AssocLookup` (`registry/core/prim_lists.go:355`)

---

## Task 3: Remove `InternSymbol` from environment types — Complete

**Evidence:**
- `TopLevelEnvironment`: no `symbolInterns` field, no `InternSymbol` method, no `SymbolInternCount`
- `EnvironmentFrame`: no `InternSymbol` method
- `GlobalEnvironmentFrame`: no `InternSymbol` method; `CreateGlobalBinding`, `GetGlobalIndex`, `GetOwnGlobalBinding` use symbol keys directly
- `SymbolInterner` interface: deleted from `internal/syntax/syntax_symbol.go`

Note: `InternSyntax` (syntax object interning for macro expansion) is a separate system and remains.

---

## Task 4: Remove all ~50 call sites — Complete

**Evidence:** `grep -r "InternSymbol" --include="*.go"` returns zero matches across the entire codebase.

---

## Task 5: Update native_template.go comment — Complete

**Evidence:** Comment at `machine/native_template.go:45` references "bypassing the runtime environment lookup path" — no mention of `InternSymbol`.

---

## Task 6: Clean up tests and benchmarks — Complete

**Evidence:**
- `environment/intern_bench_test.go`: interning benchmarks removed; retains string-comparison benchmarks
- No `TestTopLevelEnvironment_SymbolInternCount` in test files
- Zero `InternSymbol` calls in any test file

---

## Task 7: Update documentation — Complete

**Evidence (CLAUDE.md files):**
- `environment/CLAUDE.local.md:260`: correctly states "No per-VM symbol interning exists"
- `values/CLAUDE.local.md`: no "Interned per-VM" claims
- `machine/CLAUDE.local.md`: no "Symbol interning" in Gotchas

**Stale references fixed during 2026-03-14 audit:**
- `BIBLIOGRAPHY.md:149,436` — updated to reflect removal; historical references retained
- `docs/design/EMBEDDING.md:36,177` — changed "symbol interning" to "syntax interning"
- `plans/ARCHITECTURE.md:567,578` — introspection primitives plan updated to use `GetBinding` directly instead of `InternSymbol`

---

## Verification Checklist

- [x] Zero `InternSymbol` references in Go source files
- [x] No `symbolInterns` field in `TopLevelEnvironment`
- [x] `eq?` compares symbols by `.Key` via `helpers.EqIdentity`
- [x] `memq`/`assq` use `helpers.EqIdentity`
- [x] `eqv?` has `*values.Symbol` case comparing by `.Key`
- [x] Documentation updated across CLAUDE.md, BIBLIOGRAPHY.md, EMBEDDING.md
