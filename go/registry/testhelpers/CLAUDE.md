# CLAUDE.md

Package `testhelpers` provides shared test infrastructure for Scheme primitive tests.

## Purpose

Provides exported test helper functions that can be used across multiple packages for testing Scheme code execution. This package uses the `wile.Engine` internally, which employs the registry pattern for primitive registration.

## Key Functions

| Function | Purpose |
|----------|---------|
| `RunSchemeCode(t, code)` | Parse and execute Scheme code, return result |
| `RunSchemeCodeWithTimeout(t, code, timeout)` | Execute with timeout (for infinite loop protection) |
| `RunSchemeCodeWithContext(t, ctx, code)` | Execute with custom context |
| `RunSchemeCodeExpectTrue(t, code)` | Execute and assert result is `#t` |
| `RunSchemeCodeExpectFalse(t, code)` | Execute and assert result is `#f` |
| `RunSchemeCodeExpectError(t, code)` | Execute and assert an error occurs |

## Test Case Structs

```go
// For value comparison tests
type SchemeCodeTestCase struct {
    Name     string
    Code     string
    Expected values.Value
}

// For error tests
type SchemeCodeErrorTestCase struct {
    Name string
    Code string
}
```

## Usage Example

```go
import (
    "testing"
    qt "github.com/frankban/quicktest"
    "wile/registry/testhelpers"
    "wile/values"
)

func TestSomePrimitive(t *testing.T) {
    c := qt.New(t)

    result, err := testhelpers.RunSchemeCode(t, "(+ 1 2)")
    c.Assert(err, qt.IsNil)
    c.Assert(result, values.SchemeEquals, values.NewInteger(3))
}

func TestWithTimeout(t *testing.T) {
    _, err := testhelpers.RunSchemeCodeWithTimeout(t, "(let loop () (loop))", 100*time.Millisecond)
    // err will be context.DeadlineExceeded
}
```

## SchemeEquals Checker

The `values.SchemeEquals` checker (from `wile/values` package) provides quicktest integration for comparing Scheme values using structural equality.

## Architecture

This package uses `wile.NewEngine()` internally, which:
1. Creates a registry with core primitives
2. Applies the registry to create the environment
3. Loads bootstrap macros

This ensures tests run with the same environment as the production interpreter.

## Notes

- All functions accept `*testing.T` and call `t.Helper()` for proper stack traces
- Timeout functions use context cancellation which the VM checks on each iteration
- A fresh Engine is created for each test to ensure isolation
- Results are returned as `values.Value` for compatibility with `values.SchemeEquals`
