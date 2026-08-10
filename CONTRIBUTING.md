# Contributing to Wile

Thank you for your interest in contributing to Wile! This document provides guidelines for contributing to the project.

## Getting Started

### Prerequisites

- Go 1.24 or later
- Git
- Basic familiarity with Scheme (R7RS) or Lisp
- For Go contributors: Understanding of interpreters, compilers, or virtual machines helpful but not required

### Finding Work

1. Browse [open issues](https://github.com/aalpar/wile/issues)
2. Look for issues labeled:
   - `good-first-issue` — Great for newcomers to the project
   - `help-wanted` — High-priority items needing contributors
   - `documentation` — Writing examples, guides, or improving docs
3. Read the issue description and any design notes linked from it for context
4. Comment on the issue to claim it or ask questions

### Development Setup

```bash
# Clone the repository
git clone https://github.com/aalpar/wile.git
cd wile

# Build the interpreter
make build

# Run tests
make test

# Run the REPL
./dist/darwin/arm64/wile  # macOS ARM64
./dist/linux/amd64/wile   # Linux x86-64
```

## Development Workflow

### 1. Create a Feature Branch

**Never commit directly to `master`.** All changes must go through feature branches and pull requests.

```bash
git checkout -b feature/descriptive-name

# Examples:
#   feature/authorization-framework
#   fix/continuation-escape-handling
#   docs/scheme-examples
```

### 2. Make Your Changes

Follow the coding conventions in `CODING_STYLE.md`:

**Critical Rules:**
- **Run `make lint` before committing** — must pass with 0 issues
- **All tests must pass** (`make test`)
- **Use table-driven tests** for all new test functions
- **Never use single-line function definitions**
- **Use `errors.Is`/`errors.As`** for error comparisons (never `==` or `!=`)

**Key Patterns:**
- Read files before editing them
- Use existing error sentinels (`werr/werr.go`) before creating new ones
- Wrap errors with context at boundaries: `werr.WrapForeignErrorf(sentinel, "where: what failed")`
- Follow GNU flag conventions (double-dash for long flags: `--verbose`, not `-verbose`)

### 3. Write Tests

#### Go Tests

- **Table-driven tests are mandatory** — see `CODING_STYLE.md` § "Test Structure"
- Test files follow `foo.go` → `foo_test.go` convention
- Use `qt` (quicktest) for assertions
- Run tests with: `go test -v ./package/...`

#### Scheme Tests

Wile uses `(chibi test)` for Scheme-level unit tests. Tests are automatically discovered by pattern matching.

**File naming and location:**

| Type | Location | Example |
|------|----------|---------|
| Library tests | `pkg/stdlib/lib/<library>/<module>-test.scm` | `pkg/stdlib/lib/wile/algebra/sat-test.scm` |
| Core tests | `test/scheme/<feature>-test.scm` | `test/scheme/numeric-tower-test.scm` |
| Regression tests | `test/regression/issue-<num>-<slug>-test.scm` | `test/regression/issue-123-macro-hygiene-test.scm` (directory not yet populated) |

The `-test.scm` suffix, not the directory, is what makes a file run: `test/run-all.sh`
discovers with `find test pkg/stdlib/lib -name '*-test.scm'`, so a file named without
that suffix is skipped wherever it is placed.

**Test template:**

```scheme
(import (scheme base)
        (chibi test)
        (srfi 1))  ; Module under test

(test-begin "module-name")

(test-group "basic operations"
  (test '(1 2 3) (append '(1) '(2 3)))
  (test '() (append '() '())))

(test-group "edge cases"
  (test-error (car '()))
  (test-assert (null? '())))

(test-end)
```

**Running Scheme tests:**

```bash
make test              # Run all tests (Go + Scheme)
make test-scheme       # Run only Scheme tests
./test/run-all.sh      # Run Scheme tests directly
```

**Testing against different Scheme implementations:**

The Scheme test suite can run against any R7RS-compatible Scheme implementation:

```bash
make test-scheme SCHEME=chez-scheme        # Test with Chez Scheme
make test-scheme SCHEME=chibi-scheme       # Test with Chibi-Scheme
make test-scheme SCHEME=./old-dist/wile  # Compare with old Wile version
```

This is useful for verifying R7RS conformance and compatibility.

See `test/README.md` for full documentation.

### 4. Code Quality

Before committing:

```bash
# Format and lint (REQUIRED)
make lint

# Run all tests (REQUIRED)
make test

# Check coverage (optional)
make cover-go     # Go-side coverage
make cover-scm    # Scheme-side coverage
```

### 5. Commit Your Changes

```bash
# Stage only related files
git add <specific-files>

# Commit with conventional commit message
git commit -m "feat: short description

Longer explanation of what changed and why.
- First change
- Second change
"
```

**Commit prefixes:**
- `feat:` — New feature
- `fix:` — Bug fix
- `refactor:` — Code restructuring without behavior change
- `docs:` — Documentation only
- `test:` — Adding or fixing tests
- `chore:` — Maintenance tasks

### 6. Push and Create a PR

```bash
# Push your branch
git push -u origin feature/descriptive-name

# Create a pull request
gh pr create --title "feat: short description" --body "
## Summary
What this PR does.

## Changes
- Change 1
- Change 2

## Test Plan
- [ ] Tests pass
- [ ] Manual verification
"
```

### 7. Wait for CI

**Do not merge until all CI checks pass.** The PR will automatically run tests on multiple platforms.

## Versioning and Releases

Wile follows [Semantic Versioning](https://semver.org/). The current version lives
in the `VERSION` file at the repo root, formatted `vMAJOR.MINOR.PATCH` (an optional
prerelease suffix like `-alpha` is preserved).

**The version is bumped once per release, not once per commit.** Regular commits and
pull requests must **not** modify `VERSION` — it stays frozen between releases. The
bump happens only during the release ceremony.

**A release bumps the patch component by default.** `bump-minor` and `bump-major` are
deliberate calls, reserved for a release whose contents actually warrant them under
SemVer; they are not the routine choice.

Release flow (maintainers):

```bash
make bump-patch        # the default; also reconciles CHANGELOG link refs
git commit -am "release: $(cat VERSION)"   # version-bump commits land directly on master
make tag               # cuts an annotated git tag matching VERSION
make release           # goreleaser builds/publishes from the tag
```

Published binaries take their version from the **git tag** (`goreleaser` injects
`-X main.BuildVersion={{ .Tag }}`). The `VERSION` file feeds local `make build`
`--version` output and `make tag`.

## Architecture Overview

Wile is an R7RS Scheme interpreter/compiler in Go with hygienic macros.

### Pipeline

```
string → Tokenizer → Parser → SyntaxValue
  → Expander (hygiene) → Compiler → NativeTemplate (bytecode)
  → VM → values.Value (result)
```

### Key Packages

| Package | Purpose |
|---------|---------|
| `wile/` | Public embedding API for Go |
| `values/` | Scheme value types (Number, Pair, Vector, etc.) |
| `environment/` | Lexical scoping and bindings |
| `machine/` | VM, compiler, macro expander |
| `registry/` | Primitive registration |
| `internal/tokenizer` | Lexical analysis |
| `parser/` | Syntax tree construction |
| `syntax/` | Hygienic syntax objects |
| `internal/match` | Macro pattern matching |

### Essential Reading

- **`CODING_STYLE.md`** — Style guide
- **`PRIMITIVES.md`** — Complete primitives reference
- **`BIBLIOGRAPHY.md`** — Papers and specifications the implementation follows
- **`docs/`** — Architecture documentation organized by topic (see `docs/INDEX.md`); design documents for major features live here. Working notes kept during development are not part of the repository.

## Contribution Guidelines

### What We're Looking For

- **Bug fixes** — Especially R7RS conformance issues
- **Test coverage** — See issues labeled `testing`
- **Documentation** — Examples, guides, API docs
- **Standard library** — R7RS-small missing features
- **Performance** — Allocation reduction, targeted optimizations (with benchmarks)
- **Tooling** — REPL improvements, debugging tools, IDE integration

### What to Avoid

- Large refactorings without prior discussion (open an issue first)
- Breaking changes to the public API (`wile/`, `values/`, `registry/`)
- Performance optimizations without measurement (provide benchmarks)
- New dependencies without justification (prefer standard library)

### Code Review Process

1. Maintainer reviews your PR (typically within 2-3 days)
2. Address feedback by pushing new commits (don't force-push)
3. Once approved and CI passes, maintainer merges
4. Your branch is automatically deleted after merge

### R7RS Conformance

**Tests that conform to R7RS must not be removed or reverted.** If a test fails but correctly reflects R7RS behavior, the implementation must be fixed — not the test.

When implementing R7RS features, cite the specification in code comments:

```go
// Add returns the sum of this integer and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2: exact + exact = exact, exact + inexact = inexact.
func (p *Integer) Add(o Number) Number {
```

## Getting Help

- **Questions about an issue?** Comment on the issue
- **Not sure where to start?** Open a GitHub Discussion
- **Found a bug?** Open an issue with a minimal reproduction case
- **Want to propose a feature?** Open an issue for discussion first

## License

By contributing to Wile, you agree that your contributions will be licensed under the same license as the project (see LICENSE file).

## Recognition

Contributors are recognized in release notes and the project README. Thank you for helping make Wile better!
