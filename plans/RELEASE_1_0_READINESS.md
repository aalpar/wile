# Wile 1.0 Release Readiness Assessment

**Status:** ✅ COMPLETE — v1.0.0 released 2026-02-04
**Original assessment date:** 2026-02-03

---

## Executive Summary

Wile v1.0.0 was released on 2026-02-04. All "Must-Do" blocking items were resolved. This document is retained as historical reference for the release assessment process.

Wile is a well-architected Scheme implementation with a clean embedding API, comprehensive standard library coverage, and solid testing infrastructure.

---

## 1. Blocking: Correctness Issues

These produce wrong behavior and undermine trust in the implementation.

### 1.1 ~~BigInteger Overflow Promotion~~ — RESOLVED

Implemented in commit `312cf48` ("feat: integer overflow promotion and short float exponent suffixes"). The helpers `addInt64`, `subInt64`, `mulInt64`, and `negateInt64` in `go/values/integer.go:97-136` detect overflow and promote to `BigInteger`. All `Integer` arithmetic methods (`Add`, `Subtract`, `Multiply`, `Abs`, `Negate`) use these helpers. Tests in `go/values/integer_test.go` verify `MaxInt64+1`, `MinInt64-1`, `MaxInt64*2`, `MinInt64*2`, `Negate(MinInt64)`, and `Abs(MinInt64)` all produce `*BigInteger`. The TODO.md entry is **stale and should be closed.**

### 1.2 ~~R7RS Conformance Test Suite Is Skipped~~ — RESOLVED

Re-activated in commit `c487f61` ("fix: re-activate R7RS conformance tests and fix short exponent complex parsing"). The test is now running in CI with a 5-minute timeout.

### 1.3 ~~Parser Panic on Datum Comments~~ — RESOLVED

Fixed in commit `a619591` ("fixup comment parsing error"). Added proper EOF handling for datum comments and fixed case-insensitive prefix matching. Existing datum comment tests pass. No reproducing test case for the original panic was ever identified; the parser now has proper error handling at all datum comment code paths.

### 1.4 ~~Unterminated Extended Symbol Bug~~ — RESOLVED

Fixed in commit `829ef8d` ("fix: error on unterminated extended symbols and strings"), merged via PR #73. Both `readExtendedSymbol()` and `readString()` now convert `io.EOF` to `TokenizerError` at all exit points (`go/tokenizer/tokenizer.go:2003-2004`, `2018-2019`). Tests in `go/tokenizer/edge_cases_test.go` (`TestUnterminatedExtendedSymbol`, `TestExtendedSymbolEscapeErrors`) verify that `|foo`, `|foo\`, and `|foo\x` all produce proper errors. The TODO.md entry is **stale and should be closed.**

---

## 2. Blocking: Completeness Issues

### 2.1 Missing Numeric Features

| Feature | R7RS Section | Status |
|---------|-------------|--------|
| ~~Inexact digit placeholder (`1.2###`)~~ | ~~§7.1.1~~ | **RESOLVED** — implemented in `5ee4a04` |
| ~~Non-decimal base fractions (`#x10/2`)~~ | ~~§7.1.1~~ | **RESOLVED** — implemented in tokenizer |
| ~~`(read)` returning `eof-object` on empty port~~ | ~~§6.13.2~~ | **RESOLVED** — implemented in `prim_read_write.go`, tested in `prim_eof_object_test.go` |

All numeric feature gaps in this section have been resolved.

### 2.2 ~~REPL Startup Message to stdout~~ — RESOLVED

`setupSignals()` moved to REPL-only path (no longer runs in file execution mode). Added `--quiet`/`-q` flag to suppress informational messages. The `log.Printf` for file reading is also gated on `!opts.Quiet`.

---

## 3. Blocking: Release Hygiene

### 3.1 ~~Documentation Consistency~~ — RESOLVED

README usage example is correct (`--file` with double dash). TODO.md, conformance plan, and semantic differences doc are now consistent: all R7RS conformance issues are resolved. Documentation reconciled.

### 3.2 ~~CHANGELOG~~ — RESOLVED

CHANGELOG.md created in commit `bf7f4fd`.

### 3.3 Extension Test Coverage

**Packages with 0% test coverage (no test files at all):**

| Package | Risk |
|---------|------|
| `extensions/eval` | Medium — eval is core functionality |
| `extensions/exceptions` | Medium — exception handling is user-facing |
| ~~`extensions/files`~~ | ~~High~~ — **RESOLVED**: 10 tests covering all primitives |
| `extensions/math` | Medium — math extensions affect numeric correctness |
| `extensions/system` | Low — system interface |
| `extensions/threads` | Low — documented as experimental |
| `forms` | Low — covered indirectly by integration tests |
| `repl` | Low — UI code |
| `cmd` | Low — thin entry point |

The `extensions/files` gap is the most concerning. File I/O primitives without unit tests is a risk for a 1.0.

### 3.4 Core Package Coverage

| Package | Coverage | Assessment |
|---------|----------|------------|
| `define_syntax` | 97.4% | Excellent |
| `utils` | 86.6% | Good |
| `registry/core` | 85.6% | Good |
| `tokenizer` | 83.0% | Good |
| `syntax` | 79.2% | Acceptable |
| `validate` | 74.8% | Acceptable |
| `environment` | 73.2% | Needs improvement |
| `parser` | 70.1% | Needs improvement |
| `wile` | 70.1% | Acceptable for API package |
| `machine` | 65.9% | Below target — this is the largest package |
| `registry` | 62.7% | Below target |
| `match` | 61.5% | Below target |
| `values` | 57.9% | Below target — fundamental types |
| `runtime` | 51.4% | Below target |

For a 1.0, the `values` and `machine` packages are the foundation. 57.9% and 65.9% coverage respectively means substantial code paths are untested.

---

## 4. Non-Blocking but Recommended

### 4.1 Code Quality Issues

- **Parser error return:** `go/parser/parser.go:134` has a `FIXME: rework error return` — not a crash risk but tech debt in a core path.
- **`machine_context.go` and `operation_apply.go`:** Marked as needing unit tests via FIXME comments.
- **EmptyList/Void type safety:** Three files note these should be proper types, not sentinel values. Not a correctness issue but a design debt.

### 4.2 Missing Embedding API Features

Per TODO.md, these are not implemented:
- BigInteger/BigFloat value constructors in the `wile` package
- Reflection API (list bound symbols, type predicates from Go)
- Event callbacks for expansion/compilation

These are nice-to-have for 1.0 but not blocking. The current API is functional and well-documented.

### 4.3 Performance

Performance is explicitly deprioritized per project vision. No benchmarks exist. This is fine for 1.0 given the stated positioning, but a benchmark suite would set a baseline for regressions.

---

## 5. What's Ready

These areas are solid and need no further work for 1.0:

| Area | Status |
|------|--------|
| Core Scheme semantics | Working correctly for common use cases |
| Hygienic macros (sets of scopes) | Fully implemented |
| Bytecode compiler + stack VM | Working with proper tail calls |
| Continuations + `dynamic-wind` | Implemented |
| Standard libraries (15/16 R7RS) | Comprehensive |
| Embedding API | Clean, documented, with examples |
| Dependencies | Minimal (5 direct), production-grade |
| CI pipeline | Lint + build + test on every push |
| Architecture | Clean separation, well-documented packages |
| Licensing | Apache 2.0, copyright headers present |

---

## 6. Proposed 1.0 Checklist

### Must-Do (Release Blockers)

- [x] ~~Verify BigInteger overflow promotion status~~ — Confirmed implemented in `312cf48`. Close stale TODO entry.
- [x] ~~Fix parser panic on datum comment edge cases~~ — Fixed in `a619591`. Parser has proper error handling at all datum comment paths.
- [x] ~~Fix `(read)` to return `eof-object` on empty port~~ — Implemented and tested in `prim_eof_object_test.go`.
- [x] ~~Fix unterminated `|...|` symbol to error instead of succeed~~ — Fixed in `829ef8d` (PR #73).
- [x] ~~Unskip `TestR7RSConformance`~~ — Re-activated in `c487f61`.
- [x] ~~Move REPL startup banner to stderr; add `--quiet` flag~~ — `setupSignals()` moved to REPL-only path, `--quiet`/`-q` flag added
- [x] ~~`eval` requires two arguments~~ — This is correct per R7RS §6.12; the environment-specifier is required, not optional.
- [x] ~~Reconcile documentation: TODO.md, conformance plan, semantic differences doc, README~~ — All consistent
- [x] ~~Fix README usage example (`-file` → `--file`)~~ — Already correct
- [x] ~~Create CHANGELOG.md or release notes~~ — Created in `bf7f4fd`
- [x] ~~Add tests for `extensions/files` package~~ — 10 tests covering all primitives
- [x] ~~Inexact digit placeholder and non-decimal base fractions~~ — Implemented in `5ee4a04`

### Should-Do (Strengthen the Release)

- [ ] Increase `values` package test coverage above 70%
- [ ] Increase `machine` package test coverage above 75%
- [ ] Add tests for `extensions/eval` and `extensions/exceptions`
- [ ] Add BigInteger/BigFloat constructors to `wile` public API
- [ ] Address `parser.go:134` FIXME for error return
- [ ] Add unit tests for `machine_context.go` functions marked with FIXME
- [ ] Create a basic benchmark suite for regression detection

### Nice-to-Have (Can Follow in 1.1)

- [ ] EmptyList/Void as proper types instead of sentinel values
- [ ] Reflection API for embedding
- [ ] Event callbacks for expansion/compilation
- [ ] Performance optimization phases (from OPTIMIZATION_PLAN.md)
- [ ] SRFI-18 threading

---

## 7. Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| ~~Silent integer overflow~~ | ~~Medium~~ | ~~High~~ | Resolved in `312cf48` |
| Parser crash on malformed input | Low | High | Fix datum comment panic |
| Users rely on R7RS claims that aren't met | Medium | Medium | Honest documentation |
| File I/O bugs in untested extensions | Low | Medium | Add extension tests |
| API breaking changes needed post-1.0 | Low | High | Review `wile` package API stability now |

---

## Conclusion

The project is roughly 90% of the way to a credible 1.0. The remaining work is concentrated in three areas: fixing a small number of correctness issues, adding tests for undertested packages, and reconciling documentation so it accurately reflects the implementation state. None of the gaps are architectural — the foundation is sound.
