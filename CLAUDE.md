# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Wile is a Scheme interpreter/compiler in Go with hygienic macros. It compiles Scheme to bytecode and executes it on a stack-based virtual machine, implementing R7RS-style `syntax-rules` macros with a "sets of scopes" hygiene model (Flatt 2016).

## Development Environment

This project is developed on macOS with MacPorts. GNU tools are available at:
- `/opt/local/bin/` - MacPorts binaries
- `/opt/local/libexec/gnubin/` - GNU coreutils (prefer these over BSD variants)

Use `which` to locate binaries on the `$PATH`:
```bash
which gsed    # GNU sed (preferred over BSD sed)
which gawk    # GNU awk
```

## Build Commands

All commands run from the `go/` directory:

```bash
make build      # Build the scheme binary to ../dist/scheme
make test       # Run all tests (go test -v ./...)
make lint       # Run golangci-lint
make fix        # Run golangci-lint with --fix
make cover      # Run tests with coverage
make format     # Format code with golangci-lint
make tidy       # Tidy go.mod
```

Run a single test:
```bash
cd go && go test -v -run TestName ./package/...
```

Run the REPL:
```bash
./dist/scheme
```

Run a Scheme file:
```bash
./dist/scheme -file example.scm
```

## Architecture

```
Source → Tokenizer → Parser → Expander → Compiler → VM
```

| Package | Purpose |
|---------|---------|
| `machine/` | VM, compiler, macro expander (largest package) |
| `values/` | Scheme value types (69 types: numbers, pairs, ports, etc.) |
| `syntax/` | First-class syntax objects with scope sets for hygiene |
| `environment/` | Variable binding, scope chains, phase hierarchy |
| `match/` | Pattern matching engine for `syntax-rules` macros |
| `parser/` | Scheme parser with source location tracking |
| `tokenizer/` | Lexer with comprehensive R7RS token support |
| `validate/` | Form validation before compilation |
| `define_syntax/` | Macro definition handling |
| `runtime/` | Top-level environment with standard library bindings |
| `forms/` | Form specifications (lambda, if, define, etc.) |
| `cmd/` | REPL and file execution entry point |

**Key entry point**: `go/cmd/main.go` - REPL with readline support, file execution, debug commands.

**Standard library**: `lib/scheme/` contains R7RS library definitions (base.sld, r5rs.sld, etc.).

**Per-package docs**: Each Go package has its own `CLAUDE.md` with detailed type/function documentation and gotchas.

## Coding Conventions

See `CODING_STYLE.md` for the complete style guide. Key points:

**Receivers**: Always single-letter - `p` for standard, `c` for compiler-related types.

**Return variable**: Use `q` for values that will be returned.

**Constructors**: Use `New*` prefix, intermediate variable `q`:
```go
func NewInteger(v int64) *Integer {
    q := &Integer{Value: v}
    return q
}
```

**No if-assignments**: Never combine assignment and comparison:
```go
// Correct
err := doSomething()
if err != nil { ... }

// Avoid
if err := doSomething(); err != nil { ... }
```

**Errors**: Use `values.NewForeignError()`, `values.WrapForeignErrorf()`, or static errors from values package.

**Tests**: Table-driven with `quicktest` and `SchemeEquals` checker. Pattern: `Test{Type}_{Method}`.

**Imports**: Internal packages first, then standard library.

**Copyright headers**: Use the contents of `COPYRIGHT_HEADER` at the root of the repository for new Go files.

## Code Elegance

**Evaluate code like a mathematician evaluates proofs.** Scheme descends from lambda calculus; the implementation should reflect that lineage. Elegant code has:

- **Economy**: Every element earns its place. If removing something breaks nothing, it shouldn't exist.
- **Inevitability**: The best solution feels obvious in hindsight—"of course it's done this way."
- **Symmetry**: Similar operations have similar structure. Asymmetry signals either a deeper pattern being missed or a genuine essential difference worth investigating.
- **Generality without abstraction tax**: Solves the general case naturally, not by layering mechanisms. A macro expander that handles edge cases through special-casing has the wrong core abstraction.
- **Transparency**: The code reveals the underlying structure of the problem. Reading it teaches you something about the domain—reading the scope-set implementation should illuminate Flatt's model.

**Prefer algebraic thinking.** Code that composes like algebra—where you can substitute equals for equals, where operations have clear identities and inverses—is easier to reason about and extend. This matters especially in an interpreter: evaluation should be substitution-like, environments should compose cleanly, syntax transformations should be compositional.

**Distrust cleverness.** Clever code that requires explanation is usually worse than straightforward code that doesn't. Exception: when the cleverness maps directly to a known structure (monads, folds, fixed points, CPS transforms). In those cases, name the pattern explicitly.

**Notice when implementation fights the abstraction.** If code requires many special cases, null checks, or mode flags, the abstraction is probably wrong. Step back and ask what structure would make the problem trivial. The right representation often eliminates whole categories of bugs.

**Honor the Lisp tradition.** Prefer data over code, composition over inheritance, recursion over iteration when it matches the problem structure. When in doubt, ask: would this look natural written in Scheme itself?

**Refactoring discipline.** When fixing compile errors during refactoring, don't just make the minimal local fix. Step back and ask: does this change reveal redundant code? If you apply a pattern one way in file A, check if the same pattern applies in file B. Inconsistency between files handling the same logical situation signals incomplete thinking. "Fix the error" mode obscures opportunities that "understand the pattern" mode would catch.

## Test File Naming Conventions

The standard Go convention is that tests for functions in `foo.go` belong in `foo_test.go`. This project follows that convention with legitimate consolidation patterns for large packages:

| Pattern | When Used | Example |
|---------|-----------|---------|
| **1:1 matching** | Small packages with few files | `environment/binding.go` → `binding_test.go` |
| **Private function consolidation** | Files with only private functions | `validate/validate_if.go` → `validate_test.go` |
| **Thematic consolidation** | Many small related files | `primitives/prim_add.go`, `prim_subtract.go` → `prim_arithmetic_test.go` |
| **Coverage files** | Additional edge case coverage | `tokenizer/*_coverage_test.go` |

**Consolidation suffixes**: `_test.go`, `_internal_test.go`, `_extra_test.go`, `_coverage_test.go`, `_mutual_test.go`

See package-specific CLAUDE.md files for details on each package's test organization.

## GitHub Workflow

**Pull Request Merging**: The repository owner is an administrator. When merging PRs to master, use the `--admin` flag to bypass branch protection rules if necessary:

```bash
gh pr merge <PR> --merge --admin --delete-branch
```

Note: GitHub does not allow self-approval of PRs even with admin privileges, but admin merge ensures PRs can be merged without external approval.

## Important: Plan File Location

**Plans go in `wile/plans/`, NOT `wile/go/plans/`.**

When investigating R7RS conformance issues:
1. Document each test BEFORE running it in `plans/R7RS_TEST_INVESTIGATION_IN_PROGRESS.md`
2. Save the file
3. Run the test (max 15s timeout - Wile may hang)
4. Log results in the same file
5. Save again
6. Keep error summary at the top of the file
7. Use bisection technique to isolate errors

## References

- `TODO.md` - Pending tasks, missing R7RS features, future extensions (multithreading, POSIX API, Go FFI)
- `CODING_STYLE.md` - Comprehensive style guide
- `PRIMITIVES.md` - Complete primitives reference
- `BIBLIOGRAPHY.md` - Academic papers, specifications, and canonical references (Flatt 2016, R7RS, SRFIs, IEEE 754, Unicode)
- `plans/TESTING_PLAN.md` - Comprehensive primitive unit test implementation plan
- `plans/R7RS_SEMANTIC_DIFFERENCES.md` - Documented differences between implementation and R7RS specification
- `plans/R7RS_CONFORMANCE_PLAN.md` - R7RS conformance roadmap
- `plans/OPTIMIZATION_PLAN.md` - Performance optimization roadmap
- `plans/SYSTEMATIC_DEBUG_LOGGING.md` - Methodology for debugging complex issues with targeted debug logging

**Plan file naming**: Use `UPPERCASE_WITH_UNDERSCORES.md` (e.g., `OPTIMIZATION_PLAN.md`, `TESTING_PLAN.md`).

## R7RS Conformance

This project aims to implement R7RS-small. Key resources:

| Source | URL |
|--------|-----|
| R7RS-small PDF | https://small.r7rs.org/attachment/r7rs.pdf |
| R7RS Corrected (HTML) | https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html |
| R7RS-large Wiki (in progress) | https://codeberg.org/scheme/r7rs/wiki |

**Testing policy**: Tests that conform to R7RS must not be removed or reverted. If a test fails but correctly reflects R7RS behavior, the implementation must be fixed—not the test. See `go/registry/core/CLAUDE.md` for detailed test organization.

### R7RS Specification Comments

Functions implementing R7RS-specified behavior must include comments citing the relevant specification section. This ensures traceability and helps maintain conformance.

**Format**: Use `R7RS §X.Y.Z` notation in doc comments.

**Example**:
```go
// Add returns the sum of this integer and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
func (p *Integer) Add(o Number) Number {
```

**When to include R7RS citations**:
- Type definitions for Scheme value types (Integer, Pair, etc.)
- Arithmetic and comparison operations
- Type predicates and conversions (exact, inexact, integer?, etc.)
- Primitive procedure implementations
- Exactness preservation/contagion behavior
- Any behavior specified by R7RS sections 4-6

**Key R7RS sections**:
| Section | Topic |
|---------|-------|
| §4.1-4.3 | Expressions, syntax |
| §5.1-5.5 | Program structure, definitions |
| §6.1 | Equivalence predicates |
| §6.2 | Numbers (tower, exactness, operations) |
| §6.3 | Booleans, pairs, lists, symbols, characters, strings, vectors |
| §6.4 | Bytevectors |
| §6.5 | Control features |
| §6.6 | Exceptions |
| §6.7-6.13 | Environments, I/O, system interface |
