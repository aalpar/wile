# Contributing to Wile

Thank you for your interest in contributing to Wile! This document provides guidelines for contributing to the project.

## Getting Started

### Prerequisites

- Go 1.23 or later
- Git
- Basic familiarity with Scheme (R7RS) or Lisp
- For Go contributors: Understanding of interpreters, compilers, or virtual machines helpful but not required

### Finding Work

1. Browse [open issues](https://github.com/aalpar/wile/issues)
2. Look for issues labeled:
   - `good-first-issue` — Great for newcomers to the project
   - `help-wanted` — High-priority items needing contributors
   - `documentation` — Writing examples, guides, or improving docs
3. Read the issue description and linked plan files (in `plans/`) for context
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
./dist/darwin/arm64/scheme  # macOS ARM64
./dist/linux/amd64/scheme   # Linux x86-64
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

Follow the coding conventions in `CLAUDE.local.md`:

**Critical Rules:**
- **Run `make lint` before committing** — must pass with 0 issues
- **All tests must pass** (`make test`)
- **Use table-driven tests** for all new test functions
- **Never use single-line function definitions**
- **Use `errors.Is`/`errors.As`** for error comparisons (never `==` or `!=`)

**Key Patterns:**
- Read files before editing them
- Use existing error sentinels (`values/foreign_error.go`) before creating new ones
- Wrap errors with context at boundaries: `values.WrapForeignErrorf(sentinel, "where: what failed")`
- Follow GNU flag conventions (double-dash for long flags: `--verbose`, not `-verbose`)

### 3. Write Tests

- **Table-driven tests are mandatory** — see `CLAUDE.local.md` § "Test Structure"
- Test files follow `foo.go` → `foo_test.go` convention
- Use `qt` (quicktest) for assertions
- Run tests with: `go test -v ./package/...`

### 4. Code Quality

Before committing:

```bash
# Format and lint (REQUIRED)
make lint

# Run all tests (REQUIRED)
make test

# Check coverage (optional)
make cover
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
| `internal/parser` | Syntax tree construction |
| `internal/syntax` | Hygienic syntax objects |
| `internal/match` | Macro pattern matching |

### Essential Reading

- **`CLAUDE.local.md`** — Comprehensive development guide (conventions, architecture, workflow)
- **`CODING_STYLE.md`** — Style guide
- **`PRIMITIVES.md`** — Complete primitives reference
- **`plans/`** — Design documents for major features
- **`docs/dev/`** — Architecture documentation (environment system, numeric tower, etc.)

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
