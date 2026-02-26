# Scheme Test Suite Expansion

**Status**: In progress
**Created**: 2026-02-25

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

| Order | File | Source Go tests | Approx cases |
|-------|------|----------------|--------------|
| 1 | `strings-test.scm` | `prim_strings_test.go` | ~38 |
| 2 | `characters-test.scm` | `prim_characters_test.go` | ~15 |
| 3 | `ports-test.scm` | `prim_ports_test.go`, `prim_read_write_test.go` | ~60 |
| 4 | `numbers-test.scm` | `extensions/math/` tests | ~20 |
| 5 | `exceptions-test.scm` | `extensions/exceptions/` tests | ~10 |
| 6 | `lazy-test.scm` | `prim_all_test.go` (promises) | ~5-10 |
| 7 | `records-test.scm` | `prim_all_test.go` (records) | ~10 |
| 8 | `eval-test.scm` | `prim_eval_test.go` | ~15 |
| 9 | `control-test.scm` | `machine/` tests (dynamic-wind, call/cc) | ~25 |
| 10 | `macros-test.scm` | `machine/` tests (syntax-rules, syntax-case) | ~30 |

## Scope boundaries

- **Not replacing Go tests.** They stay — different layer, different assertions.
- **Not making feature tests portable.** `r7rs-tests.scm` is the portable suite; `test/scheme/` tests Wile.
- **Not writing new test cases from scratch.** Extracting what exists in Go tests. Gaps noted but not filled in this effort.
- **Not modifying infrastructure.** `run-all.sh`, `scheme_test.go`, and Makefile targets already work.
