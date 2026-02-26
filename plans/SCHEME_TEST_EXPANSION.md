# Scheme Test Suite Expansion

**Status**: Complete
**Created**: 2026-02-25
**Completed**: 2026-02-25

## Goal

Extract Scheme test cases from Go `*_test.go` files into pure Scheme test files, expanding both the canonical R7RS suite and Wile-specific feature test coverage. Boost confidence for Scheme developers evaluating Wile by making test coverage visible and runnable in the Scheme domain.

## Two Test Tiers

| Tier | Location | Runner | Purpose |
|------|----------|--------|---------|
| Canonical R7RS | `integration/testdata/r7rs-tests.scm` | `go test ./integration/...` | R7RS conformance baseline. Augmented with edge cases that test spec-defined behavior. |
| Feature tests | `test/scheme/<section>-test.scm` | `make test-scheme` (run-all.sh) | Wile-specific coverage extracted from Go tests. By R7RS section. Includes error cases. |

### Placement heuristic

- Tests a behavior described in R7RS spec → `r7rs-tests.scm`
- Tests an error condition, edge case, or Wile extension behavior → `test/scheme/*-test.scm`

## File conventions

Organized by R7RS section:

```
test/scheme/strings-test.scm        # 6.7 Strings
test/scheme/characters-test.scm     # 6.6 Characters
test/scheme/ports-test.scm          # 6.13 Input and output
test/scheme/numbers-test.scm        # 6.2 Numbers
test/scheme/exceptions-test.scm     # 6.11 Exceptions
test/scheme/lazy-test.scm           # 4.2.5 Delayed evaluation
test/scheme/records-test.scm        # SRFI-9 records
test/scheme/eval-test.scm           # 6.12 Environments and evaluation
test/scheme/control-test.scm        # 6.10 Control features
test/scheme/macros-test.scm         # 4.3 Macros
```

File template:

```scheme
;;; <section>-test.scm - <R7RS section> tests
;;;
;;; Edge cases and detailed coverage extracted from Go test suite.
;;; Complements the canonical R7RS tests in integration/testdata/r7rs-tests.scm.

(import (scheme base) ... (chibi test))

(test-begin "<section>")

(test-group "<subsection>"
  (test expected expr)
  (test-error (bad-expr)))

(test-end)
```

Extension features (math, threads, etc.) import `(wile <ext>)`. These are not portable across Scheme implementations.

## Approach

Incremental by R7RS section. One PR per section. Each PR:

1. Read all Go test files exercising that section's primitives
2. Create `test/scheme/<section>-test.scm` with extracted cases
3. Add cases that strengthen `r7rs-tests.scm` where they test spec behavior
4. Run `make test-scheme` to verify
5. PR

## Prioritized section order

| Order | File | Source Go tests | Lines | PR | Status |
|-------|------|----------------|-------|-----|--------|
| 1 | `strings-test.scm` | `prim_strings_test.go` | 178 | #338 | Done |
| 2 | `characters-test.scm` | `prim_characters_test.go` | 135 | #338 | Done |
| 3 | `ports-test.scm` | `prim_ports_test.go`, `prim_read_write_test.go` | 663 | #340 | Done |
| 4 | `numbers-test.scm` | `extensions/math/` tests, `prim_arithmetic_test.go`, `prim_numeric_predicate_test.go` | 531 | #341 | Done |
| 5 | `exceptions-test.scm` | `prim_exceptions_test.go`, `prim_exception_test.go` | 637 | #342 | Done |
| 6 | `lazy-test.scm` | `prim_all_test.go`, `prim_promise_test.go`, `prim_promise_extra_test.go` | 141 | #343 | Done |
| 7 | `records-test.scm` | `prim_all_test.go` | 304 | #343 | Done |
| 8 | `eval-test.scm` | `prim_eval_test.go` | 81 | #344 | Done |
| 9 | `control-test.scm` | `prim_control_test.go` | 327 | #345 | Done |
| 10 | `macros-test.scm` | `compile_syntax_rules_test.go`, `let_shadow_macro_test.go`, `hygiene_test.go` | 157 | #345 | Done |

**Total: 3,187 lines across 11 files (including smoke-test.scm).**

Additional fixes shipped alongside:
- PR #339: Arity errors now catchable by Scheme exception handlers (`guard`, `with-exception-handler`)
- PR #342: Flaky pool capacity test relaxed for CI stability

## Scope boundaries

- **Not replacing Go tests.** They stay — different layer, different assertions.
- **Not making feature tests portable.** `r7rs-tests.scm` is the portable suite; `test/scheme/` tests Wile.
- **Not writing new test cases from scratch.** Extracting what exists in Go tests. Gaps noted but not filled in this effort.
- **Not modifying infrastructure.** `run-all.sh`, `scheme_test.go`, and Makefile targets already work.
