# registry/ — Primitives and Test Infrastructure

## Test Helpers

`registry/core/test_helpers_test.go`: `runSchemeCode(t, code)`, `runSchemeCodeExpectError`, `runSchemeCodeExpectTrue`, `runSchemeCodeExpectFalse`, `runSchemeCodeWithTimeout`, `runSchemeCodeWithEnv`.

Root tests (`wile_test.go`): `NewEngine()` directly. Assertions: `qt` (quicktest).

## Test File Naming Conventions

The standard Go convention is that tests for functions in `foo.go` belong in `foo_test.go`. This project follows that convention with legitimate consolidation patterns for large packages:

| Pattern | When Used | Example |
|---------|-----------|---------|
| **1:1 matching** | Small packages with few files | `environment/binding.go` -> `binding_test.go` |
| **Private function consolidation** | Files with only private functions | `internal/validate/validate_if.go` -> `validate_test.go` |
| **Thematic consolidation** | Many small related files | `primitives/prim_add.go`, `prim_subtract.go` -> `prim_arithmetic_test.go` |
| **Coverage files** | Additional edge case coverage | `internal/tokenizer/*_coverage_test.go` |

**Consolidation suffixes**: `_test.go`, `_internal_test.go`, `_extra_test.go`, `_coverage_test.go`, `_mutual_test.go`

## Test Structure: Table-Driven Tests Are Mandatory

**ALWAYS use table-driven tests.** Do NOT write a series of individual `t.Run` calls with inline logic. Every test function that exercises multiple inputs must use a `[]struct` test table iterated with a `for` loop. This is non-negotiable.

Table-driven tests are superior in every way that matters:
- **Adding a case is one line**, not a copy-paste of boilerplate
- **The data is separated from the mechanism** — you can read all cases at a glance without wading through repeated assertion calls
- **Test names come from the data**, not from hand-written strings scattered across the function
- **The assertion logic is written once** and cannot drift between cases

There are two standard table shapes in this project:

**1. Success cases** — Scheme code that should produce a specific value:
```go
func TestFoo(t *testing.T) {
    c := qt.New(t)
    engine := newEngine(t)
    tcs := []struct {
        name string
        code string
        want values.Value
    }{
        {"descriptive name", `(foo ...)`, values.TrueValue},
    }
    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            result := runSchemeCode(t, tc.code)
            c.Assert(result.Internal(), qt.Equals, tc.want)
        })
    }
}
```

**2. Error cases** — Scheme code that should produce an error:
```go
func TestFooErrors(t *testing.T) {
    engine := newEngine(t)
    tcs := []struct {
        name string
        code string
    }{
        {"wrong type", `(foo "not-a-number")`},
        {"wrong arity", `(foo 1 2 3)`},
    }
    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            runSchemeCodeExpectError(t, tc.code)
        })
    }
}
```

When a single test function needs both success and error cases, use two separate table loops within the same function, or split into `TestFoo` and `TestFooErrors`.

**Why this matters:** The duplication that remains in table-driven tests (the `for` loop + `t.Run` + assert) is *structural* — it's the test mechanism, written once. The duplication in scattered `t.Run` calls is *accidental* — the same mechanism copy-pasted with different data. Adding a table case is one line; adding a scattered `t.Run` case is three lines of boilerplate where the assertion logic can silently drift from its neighbors.

**The only exception** is a test that requires unique setup/teardown per case (e.g., subprocess execution, `t.Setenv`, file I/O). Even then, prefer a table with a setup callback over hand-unrolled `t.Run` calls.
