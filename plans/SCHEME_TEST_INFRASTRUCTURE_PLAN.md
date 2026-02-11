# Scheme-Level Test Infrastructure Plan

## Problem Statement

Wile currently lacks a designated location and convention for Scheme-level unit tests. Testing is scattered across:
- `scm/` — Ad-hoc manual debugging tests (not automated)
- `integration/testdata/` — R7RS conformance tests (Go harness only)
- `examples/` — Per-feature test runners with inconsistent conventions
- No test coverage for SRFI implementations, library code, or Scheme-level regressions

**Goal**: Establish a canonical location and framework for Scheme-level unit tests with automated test discovery and execution.

## Design Principles

1. **Follow Go conventions** — Tests colocated with code where practical
2. **Use `(chibi test)`** — Leverage existing test framework, don't invent new one
3. **Automated discovery** — Test runner finds and executes all `*_test.scm` files
4. **CI integration** — Tests run as part of `make test`
5. **Self-documenting** — Test file names and locations make purpose obvious

## Directory Structure

### Option A: Colocated Tests (Go-style)

```
lib/
├── srfi/
│   ├── 1/
│   │   ├── constructors.scm
│   │   ├── constructors_test.scm
│   │   ├── fold.scm
│   │   └── fold_test.scm
│   └── 18/
│       ├── threads.scm
│       └── threads_test.scm
├── chibi/
│   ├── test.scm
│   └── diff.scm
└── wile/                          # New: Wile-specific libraries
    ├── helpers.scm
    └── helpers_test.scm
```

**Pros**: Tests next to implementation, easy to find, matches Go idioms
**Cons**: Clutters `lib/` structure

### Option B: Centralized Test Directory (Scheme-style)

```
test/
├── scheme/                        # Core language tests
│   ├── numeric-tower-test.scm
│   ├── hygiene-test.scm
│   ├── continuation-test.scm
│   └── macro-expansion-test.scm
├── srfi/                          # SRFI tests
│   ├── srfi-1-test.scm
│   └── srfi-18-test.scm
├── lib/                           # Library tests (mirrors lib/)
│   └── chibi/
│       └── diff-test.scm
├── regression/                    # Bug regression tests
│   ├── issue-123-continuation-escape.scm
│   └── issue-145-macro-hygiene.scm
└── run-all.scm                    # Test discovery and runner
```

**Pros**: Clean separation, conventional for Scheme implementations
**Cons**: Tests separated from code

### Option C: Hybrid (Recommended)

```
lib/
├── srfi/1/
│   ├── *.scm                      # Implementation
│   └── test/                      # Tests for this SRFI
│       ├── constructors-test.scm
│       └── fold-test.scm
test/
├── scheme/                        # Core language (not library-specific)
│   ├── numeric-tower-test.scm
│   └── hygiene-test.scm
├── regression/                    # Bug regression tests
│   └── issue-*.scm
└── run-all.scm                    # Discovers and runs all *-test.scm
```

**Rationale**: Library tests stay with libraries (Option A benefits), but core language and regression tests have dedicated space (Option B benefits).

## File Naming Convention

| Location | Pattern | Example |
|----------|---------|---------|
| Library tests | `<module>-test.scm` or `<file>_test.scm` | `fold-test.scm`, `constructors_test.scm` |
| Core tests | `<feature>-test.scm` | `numeric-tower-test.scm` |
| Regression tests | `issue-<num>-<slug>.scm` | `issue-123-continuation-escape.scm` |

**Rule**: All test files must be discoverable by glob pattern `**/*-test.scm` or `**/*_test.scm`.

## Test File Template

```scheme
#!/usr/bin/env scheme
;;; <module>-test.scm - Unit tests for <module>
;;;
;;; Tests the <module> implementation for correctness and edge cases.

(import (scheme base)
        (chibi test)
        ;; Import module under test
        (srfi 1))  ; example

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

## Test Runner Implementation

### `test/run-all.scm`

```scheme
#!/usr/bin/env scheme
;;; run-all.scm - Discover and run all Scheme-level unit tests
;;;
;;; Usage:
;;;   ./test/run-all.scm
;;;   scheme -f test/run-all.scm
;;;
;;; Discovers all *-test.scm and *_test.scm files and executes them.

(import (scheme base)
        (scheme file)
        (scheme write)
        (scheme process-context)
        (chibi test))

;; Test discovery: find all *-test.scm files
(define test-files
  '("test/scheme/numeric-tower-test.scm"
    "test/scheme/hygiene-test.scm"
    "test/regression/issue-123-continuation-escape.scm"
    "lib/srfi/1/test/fold-test.scm"))

(define (run-test-file path)
  (display "Running ")
  (display path)
  (newline)
  (load path))

(test-begin "Wile Scheme Tests")

(for-each run-test-file test-files)

(test-end)
(test-exit)
```

**Future enhancement**: Use `directory-list` (if available) or shell globs for automatic discovery.

### Shell wrapper: `test/run-all.sh`

```bash
#!/bin/bash
# Run all Scheme-level tests

set -e

SCHEME="${SCHEME:-./dist/scheme}"

if [ ! -x "$SCHEME" ]; then
    echo "Error: Scheme interpreter not found at $SCHEME"
    echo "Build it with: make build"
    exit 1
fi

# Discover all test files
TEST_FILES=$(find test lib -name '*-test.scm' -o -name '*_test.scm' | sort)

if [ -z "$TEST_FILES" ]; then
    echo "No test files found"
    exit 1
fi

echo "Discovered $(echo "$TEST_FILES" | wc -l) test file(s)"

# Run each test file
FAILED=0
for file in $TEST_FILES; do
    echo "▶ $file"
    if ! "$SCHEME" -f "$file"; then
        FAILED=$((FAILED + 1))
        echo "✗ FAILED: $file"
    fi
done

if [ $FAILED -eq 0 ]; then
    echo "✓ All tests passed"
    exit 0
else
    echo "✗ $FAILED test file(s) failed"
    exit 1
fi
```

## Go Integration

Add a Go test that runs the Scheme test suite:

### `test/scheme_test.go`

```go
package test

import (
	"os"
	"os/exec"
	"testing"

	"github.com/frankban/quicktest"
)

func TestSchemeTestSuite(t *testing.T) {
	c := quicktest.New(t)

	// Ensure scheme binary exists
	_, err := os.Stat("./dist/scheme")
	c.Assert(err, quicktest.IsNil, quicktest.Commentf("scheme binary not found; run 'make build' first"))

	// Run the test suite
	cmd := exec.Command("./test/run-all.sh")
	cmd.Env = append(os.Environ(), "SCHEME=./dist/scheme")

	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Logf("Test output:\n%s", output)
	}
	c.Assert(err, quicktest.IsNil, quicktest.Commentf("Scheme test suite failed"))
}
```

Update `Makefile`:

```makefile
test: build
	go test -v ./...
	./test/run-all.sh  # Run Scheme tests after Go tests
```

## Migration Strategy

### Phase 1: Infrastructure

1. Create `test/` directory structure
2. Implement `test/run-all.scm` and `test/run-all.sh`
3. Add Go integration test (`test/scheme_test.go`)
4. Update `Makefile` to run Scheme tests
5. Document in `CONTRIBUTING.md`

### Phase 2: Starter Tests

Create minimal examples to establish patterns:

1. `test/scheme/numeric-tower-test.scm` — Core numeric operations
2. `test/scheme/hygiene-test.scm` — Macro hygiene edge cases
3. `test/regression/issue-template.scm` — Template for bug reports
4. `lib/srfi/1/test/basic-test.scm` — SRFI-1 basic operations

### Phase 3: Migration

Migrate existing tests:

1. Review `scm/test_*.scm` files
2. Convert useful tests to `(chibi test)` format
3. Move to appropriate locations in `test/`
4. Delete obsolete `scm/test_*.scm` files (keep `scm/` for manual debugging if needed)
5. Extract testable examples from `examples/` into regression tests

### Phase 4: Coverage

Add test coverage for:

1. SRFI-1 list library (`lib/srfi/1/`)
2. SRFI-18 threading (`lib/srfi/18/`)
3. Numeric tower (exact/inexact, all numeric types)
4. Continuation edge cases (escape, wind/unwind)
5. Macro expansion (hygiene, nested macros, quasisyntax)

## Documentation Updates

### `CONTRIBUTING.md`

Add new section:

```markdown
## Scheme-Level Testing

Wile uses `(chibi test)` for Scheme-level unit tests. Tests are colocated with library code or placed in `test/` for core language features and regressions.

### Running Tests

```bash
make test              # Run all tests (Go + Scheme)
./test/run-all.sh      # Run only Scheme tests
```

### Writing Tests

1. **Library tests**: Place in `lib/<library>/test/<module>-test.scm`
2. **Core tests**: Place in `test/scheme/<feature>-test.scm`
3. **Regressions**: Place in `test/regression/issue-<num>-<slug>.scm`

Example:

```scheme
(import (scheme base)
        (chibi test)
        (srfi 1))

(test-begin "srfi-1-append")
(test '(1 2 3) (append '(1) '(2 3)))
(test-end)
```

### Test Discovery

All files matching `*-test.scm` or `*_test.scm` are automatically discovered and run.
```

### `test/README.md`

```markdown
# Scheme-Level Test Suite

Automated test suite for Wile's Scheme implementation.

## Structure

- `scheme/` — Core language tests (numeric tower, hygiene, continuations)
- `regression/` — Bug regression tests (issue-XXX-*.scm)
- `run-all.scm` — Test discovery and runner
- `run-all.sh` — Shell wrapper for test execution

## Running Tests

```bash
./test/run-all.sh
```

Or via Go integration:

```bash
go test ./test
```

## Writing Tests

See `CONTRIBUTING.md` for test file conventions and examples.
```

## Benefits

1. **Discoverability** — New contributors can find and run tests easily
2. **Automation** — Tests run in CI, preventing regressions
3. **Coverage** — Dedicated location for Scheme-level test coverage
4. **Consistency** — Standardized test framework (`chibi test`) and naming
5. **Documentation** — Tests serve as executable examples

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Tests slow down CI | Run Scheme tests in parallel with Go tests |
| Test discovery fragile | Use explicit file list initially, add glob later |
| Tests fail in CI but pass locally | Pin exact Scheme binary version in CI |
| Library tests clutter `lib/` | Use `lib/<lib>/test/` subdirectory pattern |

## Open Questions

1. **Should `scm/` be deleted or repurposed?**
   → Keep for manual debugging, document as "not automated"

2. **Should existing R7RS tests move to `test/`?**
   → No, they're integration tests with Go harness; leave in `integration/testdata/`

3. **How to handle tests requiring file I/O or external resources?**
   → Use `test/fixtures/` for test data files

4. **Should we add test coverage reporting?**
   → Future enhancement; not in initial implementation

## Success Criteria

- [ ] `test/` directory structure created
- [ ] Test runner (`run-all.scm`, `run-all.sh`) implemented
- [ ] Go integration test (`test/scheme_test.go`) added
- [ ] `make test` runs Scheme tests
- [ ] At least 3 example tests written (scheme, srfi, regression)
- [ ] `CONTRIBUTING.md` documents test conventions
- [ ] `test/README.md` created
- [ ] CI passes with new test infrastructure

## Timeline

**Estimated effort**: 1-2 sessions

- **Phase 1 (Infrastructure)**: 1 hour — Directory structure, runners, docs
- **Phase 2 (Starter Tests)**: 30 minutes — 3-5 example tests
- **Phase 3 (Migration)**: 30 minutes — Move/convert existing tests
- **Phase 4 (Coverage)**: Future work — Add comprehensive test coverage

## References

- SRFI-64: Test suite API (basis for `(chibi test)`)
- Chibi-Scheme test organization: https://github.com/ashinn/chibi-scheme/tree/master/tests
- Go testing conventions: https://go.dev/doc/tutorial/add-a-test
