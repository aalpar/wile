# Testing Plans

## Completed: Scheme Test Suite Expansion

**Status**: Complete (2026-02-25)

Extracted Scheme test cases from Go `*_test.go` files into pure Scheme test files, expanding both the canonical R7RS suite and Wile-specific feature test coverage.

### Results

**3,187 lines across 11 files**, organized by R7RS section in `test/scheme/`:

| File | Source Go tests | Lines | PR |
|------|----------------|-------|-----|
| `strings-test.scm` | `prim_strings_test.go` | 178 | #338 |
| `characters-test.scm` | `prim_characters_test.go` | 135 | #338 |
| `ports-test.scm` | `prim_ports_test.go`, `prim_read_write_test.go` | 663 | #340 |
| `numbers-test.scm` | `extensions/math/` tests, `prim_arithmetic_test.go` | 531 | #341 |
| `exceptions-test.scm` | `prim_exceptions_test.go`, `prim_exception_test.go` | 637 | #342 |
| `lazy-test.scm` | `prim_all_test.go`, `prim_promise_test.go` | 141 | #343 |
| `records-test.scm` | `prim_all_test.go` | 304 | #343 |
| `eval-test.scm` | `prim_eval_test.go` | 81 | #344 |
| `control-test.scm` | `prim_control_test.go` | 327 | #345 |
| `macros-test.scm` | `compile_syntax_rules_test.go`, `hygiene_test.go` | 157 | #345 |

Additional fixes shipped alongside:
- PR #339: Arity errors now catchable by Scheme exception handlers
- PR #342: Flaky pool capacity test relaxed for CI stability

### Two Test Tiers

| Tier | Location | Purpose |
|------|----------|---------|
| Canonical R7RS | `integration/testdata/r7rs-tests.scm` | R7RS conformance baseline |
| Feature tests | `test/scheme/<section>-test.scm` | Wile-specific coverage, error cases, edge cases |

### Scope

- Did NOT replace Go tests — different layer, different assertions.
- Did NOT make feature tests portable — `r7rs-tests.scm` is the portable suite.
- Did NOT write new test cases from scratch — extracted what exists in Go tests.
- Did NOT modify infrastructure — `run-all.sh`, `scheme_test.go`, and Makefile targets already work.
