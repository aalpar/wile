# Scheme-Level Test Suite

Automated test suite for Wile's Scheme implementation using the `(chibi test)` framework.

## Structure

```
test/
├── scheme/           # Core language tests
│                     # - Numeric tower, hygiene, continuations, etc.
├── regression/       # Bug regression tests (not yet populated)
│                     # - issue-XXX-*.scm for specific bug fixes
├── run-all.scm       # Scheme test runner (discovers and executes tests)
├── run-all.sh        # Shell wrapper for test execution
├── scheme_test.go    # Go integration test (runs Scheme suite via Go test)
└── README.md         # This file
```

Library-specific tests are colocated with their implementations (pattern, not yet populated):
```
lib/
└── srfi/1/
    ├── fold.scm
    └── test/
        └── fold-test.scm
```

## Running Tests

**Via Make (recommended):**
```bash
make test              # Run all tests (Go + Scheme)
make test-scheme       # Run only Scheme tests
```

**Testing against different Scheme implementations:**
```bash
# Test with Wile (default)
make test-scheme

# Test with Chez Scheme
make test-scheme SCHEME=chez-scheme

# Test with an older Wile version
make test-scheme SCHEME=./old-dist/wile

# Test with Chibi-Scheme
make test-scheme SCHEME=chibi-scheme

# Test with any other R7RS Scheme
make test-scheme SCHEME=/path/to/wile
```

**Directly via shell:**
```bash
./test/run-all.sh                    # Use default binary
SCHEME=chez-scheme ./test/run-all.sh # Override binary
```

**Via Scheme interpreter:**
```bash
./dist/wile -f test/run-all.scm
```

**Via Go test:**
```bash
go test ./test         # Runs TestSchemeTestSuite
```

## Writing Tests

All test files must match the pattern `*-test.scm` for automatic discovery.

### File Naming

| Location | Pattern | Example |
|----------|---------|---------|
| Library tests | `pkg/stdlib/lib/<library>/<module>-test.scm` | `pkg/stdlib/lib/wile/algebra/sat-test.scm` |
| Core tests | `test/scheme/<feature>-test.scm` | `test/scheme/numeric-tower-test.scm` |
| Regression tests | `test/regression/issue-<num>-<slug>-test.scm` | `test/regression/issue-123-macro-hygiene-test.scm` (directory not yet populated) |

### Test Template

```scheme
;;; <module>-test.scm - Unit tests for <module>
;;;
;;; Tests the <module> implementation for correctness and edge cases.

(import (scheme base)
        (chibi test)
        ;; Import module under test
        (srfi 1))

(test-begin "<module>")

(test-group "basic operations"
  (test '(1 2 3) (append '(1) '(2 3)))
  (test '() (append '() '())))

(test-group "edge cases"
  (test-error (car '()))
  (test-assert (null? '())))

(test-group "regression"
  ;; Issue #123: append should preserve exact list structure
  (test '(a b c) (append '(a) '(b c))))

(test-end)
```

### Test Macros

The `(chibi test)` framework provides:

| Macro | Purpose | Example |
|-------|---------|---------|
| `test` | Basic equality test | `(test expected-value expression)` |
| `test-assert` | Boolean assertion | `(test-assert (> 5 3))` |
| `test-error` | Expect an error | `(test-error (car '()))` |
| `test-group` | Group related tests | `(test-group "arithmetic" ...)` |
| `test-equal` | Custom comparator | `(test-equal my-equal? expected expr)` |

## Test Discovery

The `run-all.sh` script discovers all test files by searching for `*-test.scm` in:
- `test/` directory (recursive)
- `pkg/stdlib/lib/` directory (recursive)

Tests are executed in lexicographic order by file path.

## CI Integration

Scheme tests run as part of `make test` in CI. The Go test `TestSchemeTestSuite` in `scheme_test.go` executes the full Scheme test suite and fails the build if any tests fail.

## Adding New Tests

1. **For library code**: Create `pkg/stdlib/lib/<library>/<module>-test.scm`
2. **For core features**: Create `test/scheme/<feature>-test.scm`
3. **For bug fixes**: Create `test/regression/issue-<num>-<slug>-test.scm`
4. **Run tests**: `make test` or `./test/run-all.sh`

Every one of these ends in `-test.scm` because that suffix is what
`run-all.sh` matches. A file named without it is never discovered, and the
suite reports success while the test never ran.

Tests are automatically discovered—no need to register them in a central list.

## Cross-Implementation Testing

The test suite can run against any R7RS Scheme implementation:

```bash
# Compare multiple implementations
./test/compare-schemes.sh ./dist/darwin/arm64/wile chez-scheme chibi-scheme

# Compare Wile versions
cp ./dist/darwin/arm64/wile ./dist/darwin/arm64/wile-old
make build
./test/compare-schemes.sh ./dist/darwin/arm64/wile-old ./dist/darwin/arm64/wile
```

See `test/COMPATIBILITY.md` for detailed guidance on cross-implementation testing.

## See Also

- `test/COMPATIBILITY.md` — Cross-implementation testing guide
- `CONTRIBUTING.md` — Full test guidelines and conventions
- `lib/chibi/test.sld` — Test framework implementation
