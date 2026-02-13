# Scheme-Level Test Infrastructure Plan

**Status**: INFRASTRUCTURE COMPLETE, TEST CONTENT PENDING

## What Exists

- `test/` directory with README, runner scripts, Go integration
- Test discovery (`run-all.sh` finds `*-test.scm` files)
- Cross-implementation testing (`compare-schemes.sh`)
- CI integration (`make test-scheme`)
- `(chibi test)` framework
- Smoke test passing (`test/scheme/smoke-test.scm`)

## What's Missing

Comprehensive test files for:

| Category | Status |
|----------|--------|
| Numeric tower (exact/inexact, all types) | Not started |
| Macro hygiene edge cases | Not started |
| Continuation edge cases (escape, wind/unwind) | Not started |
| SRFI-1 list library | Not started |
| SRFI-18 threading | Not started |
| Quasisyntax/nested macros | Not started |
| Regression tests | Not started |
| Library-specific tests | Not started |

## Conventions

- **Library tests**: `lib/<library>/test/<module>-test.scm`
- **Core tests**: `test/scheme/<feature>-test.scm`
- **Regressions**: `test/regression/issue-<num>-<slug>.scm`
- **Discovery**: All `*-test.scm` files auto-discovered
- **Framework**: `(chibi test)` with `test-begin`/`test-end`/`test-group`
