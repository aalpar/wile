# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Wile is a Scheme interpreter/compiler in Go with hygienic macros. It compiles Scheme to bytecode and executes it on a stack-based virtual machine, implementing R7RS-style `syntax-rules` macros with a "sets of scopes" hygiene model (Flatt 2016).

## Product Vision

Wile is **a Scheme scripting layer that feels native to Go**, not a Scheme that happens to be written in Go.

- **Full R7RS compliance is the baseline.** It earns credibility as a real Scheme, not a toy. Compliance is the floor, not the ceiling.

- **Embedding is the product.** The API surface for Go developers embedding Wile should be the most polished part of the project. Defining Scheme functions from Go, calling Scheme from Go, passing data back and forth — all of this should feel idiomatic to a Go developer who has never written Scheme.

- **Fill Go's expressiveness gaps.** Go is deliberately simple: no macros, no pattern matching, limited metaprogramming. Wile provides what Go intentionally lacks — hygienic macros, first-class continuations, symbolic computation, homoiconicity — without requiring developers to leave the Go ecosystem or link against C libraries. The pitch isn't "learn Scheme" but "solve problems Go makes hard."

- **No FFI tax.** Pure Go means no CGo, no C toolchain, no cross-compilation headaches, no linking surprises. A Go developer adds Wile as a `go get` dependency and it just works. This is a concrete advantage over embedding Chibi-Scheme or S7, which require CGo and bring all of its pain (slow builds, broken cross-compilation, platform-specific toolchain setup).

- **Delegate GC to Go's runtime.** Scheme values are Go heap objects; Go's GC collects them. This is a deliberate architectural choice: less code to maintain, fewer bugs, and the GC improves for free with each Go release. The tradeoff (no generational/compacting GC tuned for Scheme allocation patterns) is accepted — correctness and maintainability over theoretical throughput.

- **Performance is explicitly deprioritized.** Wile will not compete with Chez or Gambit on benchmarks. The target workloads — configuration, policy evaluation, data transformation, scripting — are not bottlenecked on interpreter speed. Correctness, ergonomics, and embedding quality matter more.

- **Modular extension architecture.** The extension system is designed so that extensions can be split into separate GitHub repos. Users import the core runtime plus whatever extensions they need. They can also create custom extensions in their own packages. Currently, extensions live in `internal/extensions/` and are only available in the standalone interpreter binary. Making them publicly importable is a future release if there's adoption.

## Imperatives (Never Deviate)

These are exact patterns. Do not improvise or substitute alternatives.

| Wrong | Correct | Note |
|-------|---------|------|
| Creating plans in random locations | Creating plans in `plans/` | Plans live at repo root |
| `if x := f(); x != nil {` | `x := f()` then `if x != nil {` | No compound if-assignments |
| `func foo() int { return x }` | Multi-line function body | **NEVER** write single-line function definitions |

replace:
```
if <conditional> {
    mc.SetValue(values.TrueValue)
} else {
    mc.SetValue(values.FalseValue)
}
```

with:
```
mc.SetValue( BoolToBoolean(<conditional>) )
```

**ALWAYS** create plan files in `plans/`.
**NEVER** commit changes without asking first. The user structures commits themselves.
**NEVER** commit directly to master. All changes must go through feature branches and pull requests.
**NEVER write single-line function definitions.** This applies to ALL function forms:
named functions, methods, closures (inline, deferred, goroutine, or assigned), and
function arguments. Every function body MUST start on the line after the opening brace
and the closing brace MUST be on its own line. No exceptions.

## Workflow

When working from `TODO.md` or a phased plan, read and update `TODO.md` after completing each phase. Mark items done as you go so progress is visible and no work gets repeated across sessions.

## Session Planning

Finish codebase reading and exploration before the session ends. If a plan is too large to complete in one session, break it into smaller chunks that can each be completed independently. Partial exploration with no code changes is wasted work.

## Wile Architecture

### Pipeline

```
string → Tokenizer(internal/tokenizer) → Parser(internal/parser) → SyntaxValue
  → Expander(machine/expander_*.go) → Compiler(machine/compile_*.go) → NativeTemplate
  → VM(machine/machine_context.go, MachineContext.Run()) → values.Value
```

Entry: `engine.go` → `Engine.Eval()` or `Engine.Compile()` + `Engine.Run()`

### Package Layering

```
values/ → environment/ → internal/{tokenizer,parser,syntax,schemeutil,validate,match,bootstrap,extensions}
  → machine/ → registry/ → wile/ (root)
```

Public API (embedders): `wile/`, `values/`, `registry/`. Internal: `internal/*`. Machine: public but rarely used directly.

### Value Types

`Value` interface: `SchemeString()`, `IsVoid()`, `EqualTo(Value)`. No Kind enum — use type assertions.

- **Numeric** (`Number`): Integer, BigInteger, Float, BigFloat, Rational, Complex, BigComplex
- **Basic**: Boolean, Symbol, String, Character, Byte
- **Collections**: Pair, Vector, ByteVector, ArrayList, Hashtable, EmptyList
- **I/O** (`Port`/`InputPort`/`OutputPort`): BinaryInputPort, CharacterInputPort, TextualWriter
- **Control**: MachineClosure, CaseLambdaClosure, Parameter, ComposableContinuation
- **Advanced**: SyntaxValue, CompileTimeValue, Record, Box, Promise, Channel, Thread, Mutex
- **Errors**: ForeignError, NativeError, StaticError

Interfaces: `Hashable`, `Tuple`, `Indexable`

### VM Operations

`Operation.Apply(context.Context, *MachineContext) (*MachineContext, error)` — stored in `NativeTemplate.Operations`, PC-indexed.

Key ops: Push/Pop (stack), Apply (dispatch), ForeignFunctionCall (Go primitives), MakeClosure, LoadLocal/StoreLocal, LoadGlobal/StoreGlobal, BranchOnFalse/BranchOnNotFalse, SaveContinuation/RestoreContinuation, PushWind/PopWind.

### Extensions

Primitives: `registry/core/prim_*.go`. Signature: `func(context.Context, *MachineContext) (*MachineContext, error)`.

Register: `r.AddPrimitive(PrimitiveSpec{Name, ParamCount (-1=variadic), IsVariadic, Impl}, Phase)`. Phases: `PhaseRuntime | PhaseMacro | PhaseExpand`.

Extension interface: `Name() string` + `AddToRegistry(*Registry) error`.

### Test Helpers

`registry/core/test_helpers_test.go`: `runSchemeCode(t, code)`, `runSchemeCodeExpectError`, `runSchemeCodeExpectTrue`, `runSchemeCodeExpectFalse`, `runSchemeCodeWithTimeout`, `runSchemeCodeWithEnv`.

Root tests (`wile_test.go`): `NewEngine()` directly. Assertions: `qt` (quicktest).

## GNU Flag Conventions

Wile follows standard GNU command-line option conventions. All flags use the `go-flags` library (`github.com/jessevdk/go-flags`).

### Rules

- **Short flags**: Single dash, single letter: `-f`, `-v`, `-V`
- **Long flags**: Double dash, full word: `--file`, `--verbose`, `--version`
- **NEVER** use single-dash long flags (e.g., `-file` is wrong, `--file` is correct)
- Short and long forms are aliases for the same option
- Boolean flags don't take arguments (`--verbose`, not `--verbose=true`)
- Value flags use `--flag VALUE` or `--flag=VALUE` or `-f VALUE`
- `--` terminates flag parsing; everything after is a positional argument

### Current Flags

| Short | Long | Type | Description |
|-------|------|------|-------------|
| `-f` | `--file` | []string | Scheme file(s) to load (repeatable) |
| `-i` | `--interactive` | bool | Enter REPL after loading file(s) |
| `-L` | `--library-path` | string | Library search paths (colon-separated) |
| `-q` | `--quiet` | bool | Suppress informational messages |
| `-V` | `--version` | bool | Print version and exit |

### Reserved Short Flags (GNU Convention)

When adding new flags, prefer these standard GNU short-flag assignments:

| Short | Long | Convention |
|-------|------|------------|
| `-v` | `--verbose` | Increase verbosity |
| `-q` | `--quiet` / `--silent` | Suppress output |
| `-h` | `--help` | Show help (handled automatically by `go-flags`) |
| `-o` | `--output` | Output file |
| `-d` | `--debug` | Enable debug mode |
| `-n` | `--dry-run` | Show what would be done |

## Documentation Notation

| Notation | Meaning | Example |
|----------|---------|---------|
| `<value>` | Required placeholder (user supplies) | `git commit -m "<message>"` |
| `[value]` | Optional element | `go test [-v] ./...` |
| `<value>...` | One or more of this element | `cat <file>...` |
| `[value]...` | Zero or more of this element | `rm [file]...` |
| `{a\|b}` | Required choice between alternatives | `git {push\|pull}` |
| `[a\|b]` | Optional choice between alternatives | `make [build\|test]` |
| `ALLCAPS` | Environment variable or constant | `$GOPATH`, `EOF` |
| `` `literal` `` | Exact text (use as-is) | `` `--verbose` `` |
| `→` | Maps to / becomes / produces | `foo.go → foo_test.go` |

**Escaping**: When angle brackets appear literally in commands (rare), escape as `\<` or quote the whole command.

**Combining**: `[--timeout <ms>]` means the flag is optional, but if provided, requires a value.

## Build Commands

```bash
make build            # Build to ./dist/{os}/{arch}/scheme (e.g., dist/darwin/arm64/scheme)
make build-all        # Build for all platforms (darwin/linux × arm64/amd64)
make test             # Run all tests (go test -v ./...)
make lint             # Run golangci-lint
make fix              # Run golangci-lint with --fix
make cover            # Run tests with coverage
make format           # Format code with golangci-lint
make tidy             # Tidy go.mod
make release-check    # Validate .goreleaser.yml syntax
make release-snapshot # Dry-run release build (no publish, output in dist/)
make release          # Full GoReleaser release (requires tag context)
```

Quick build (convenience binary at dist root):
```bash
go build -o dist/scheme ./cmd
```

Run a single test:
```bash
go test -v -run TestName ./package/...
```

Run the REPL:
```bash
./dist/scheme                            # Using convenience binary
./dist/darwin/arm64/scheme               # Using platform-specific binary
```

Run a Scheme file:
```bash
./dist/scheme --file example.scm
```

### dist/ Directory Structure

```
dist/
├── scheme                    # Convenience binary (from go build -o dist/scheme)
├── darwin/
│   ├── arm64/
│   │   └── scheme            # macOS ARM64 binary (from make build on M1/M2)
│   └── amd64/
│       └── scheme            # macOS Intel binary
└── linux/
    ├── arm64/
    │   └── scheme            # Linux ARM64 binary
    └── amd64/
        └── scheme            # Linux Intel binary
```

## Go Conventions

After any Go code changes, run `make lint` (or at minimum `goimports -w` on changed files) before considering the task complete. Do not report completion with outstanding formatting or import issues.

## Test File Naming Conventions

The standard Go convention is that tests for functions in `foo.go` belong in `foo_test.go`. This project follows that convention with legitimate consolidation patterns for large packages:

| Pattern | When Used | Example |
|---------|-----------|---------|
| **1:1 matching** | Small packages with few files | `environment/binding.go` → `binding_test.go` |
| **Private function consolidation** | Files with only private functions | `internal/validate/validate_if.go` → `validate_test.go` |
| **Thematic consolidation** | Many small related files | `primitives/prim_add.go`, `prim_subtract.go` → `prim_arithmetic_test.go` |
| **Coverage files** | Additional edge case coverage | `internal/tokenizer/*_coverage_test.go` |

**Consolidation suffixes**: `_test.go`, `_internal_test.go`, `_extra_test.go`, `_coverage_test.go`, `_mutual_test.go`

See package-specific CLAUDE.md files for details on each package's test organization.

## Test Structure: Table-Driven Tests Are Mandatory

**ALWAYS use table-driven tests.** Do NOT write a series of individual `t.Run` calls with inline logic. Every test function that exercises multiple inputs must use a `[]struct` test table iterated with a `for` loop. This is non-negotiable.

Table-driven tests are superior in every way that matters:
- **Adding a case is one line**, not a copy-paste of boilerplate
- **The data is separated from the mechanism** — you can read all cases at a glance without wading through repeated `eval`/`assert` calls
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
            result := eval(t, engine, tc.code)
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
            evalExpectError(t, engine, tc.code)
        })
    }
}
```

When a single test function needs both success and error cases, use two separate table loops within the same function, or split into `TestFoo` and `TestFooErrors`.

**Why this matters:** The duplication that remains in table-driven tests (the `for` loop + `t.Run` + assert) is *structural* — it's the test mechanism, written once. The duplication in scattered `t.Run` calls is *accidental* — the same mechanism copy-pasted with different data. Adding a table case is one line; adding a scattered `t.Run` case is three lines of boilerplate where the assertion logic can silently drift from its neighbors.

**The only exception** is a test that requires unique setup/teardown per case (e.g., subprocess execution, `t.Setenv`, file I/O). Even then, prefer a table with a setup callback over hand-unrolled `t.Run` calls.

## Error Handling

**`errors.Is` and `errors.As` are mandatory.** Never compare errors with `==` or `!=`. Error wrapping means `err == ErrFoo` silently fails when the error is wrapped. Use `errors.Is(err, ErrFoo)` for sentinel errors and `errors.As(err, &target)` for typed errors. This applies to all error comparisons including `io.EOF` and any other sentinel values.

**Two-layer error convention: sentinel + wrap.** Every error returned from production code uses two layers:

1. **Static sentinel** (`values.NewStaticError`) defined centrally in `values/foreign_error.go`. The sentinel is a stable identity for programmatic matching via `errors.Is`. Use existing sentinels before adding new ones.

2. **Contextual wrap** (`values.WrapForeignErrorf`) at each return site. The wrap message describes *where* the error occurred and *what operation* failed, so a human reading the error can locate the failure without a debugger.

Never return a bare sentinel — always wrap it with site-specific context. Never use `errors.New` or `fmt.Errorf` in production code; use a sentinel + wrap instead. `fmt.Errorf` is reserved exclusively for internal use within the error type constructors (`StaticError`, `ForeignError`, `NativeError`). In production code, every error must wrap a sentinel so callers can match with `errors.Is`/`errors.As`. Using `fmt.Errorf` creates opaque errors that defeat programmatic error handling.

```go
// WRONG: bare sentinel, no context
return nil, values.ErrNotANumber

// WRONG: errors.New, no programmatic matching
return nil, errors.New("not a number")

// WRONG: fmt.Errorf, opaque — callers cannot use errors.Is/errors.As
return nil, fmt.Errorf("not a number: %s", val)

// CORRECT: sentinel for errors.Is + wrap for human context
return nil, values.WrapForeignErrorf(values.ErrNotANumber, "makeExact: value is not numeric")
```

**Wrap errors with explanatory context.** Every error that crosses a subsystem boundary must include context about what operation failed. Examples:
- `"parameter: converter error"` — tells you a parameter's converter function failed
- `"bootstrap: expansion error"` — tells you macro expansion failed during bootstrap
- `"parse error"` — tells you the parser couldn't read the input

Never wrap with empty messages (`WrapForeignErrorf(err, "")` produces `": underlying error"`). If the error already has sufficient context, return it as-is.

**Use the project's error types consistently:**

| Type | When to use | Where defined |
|------|------------|---------------|
| `*wile.Error` | Engine initialization failures | `error.go` |
| `*wile.CompilationError` | Parse, expand, compile failures | `error.go` |
| `*wile.RuntimeError` | Execution failures, Scheme exceptions | `error.go` |
| `*machine.SchemeError` | Internal VM errors with source location | `machine/scheme_error.go` |
| `*values.ForeignError` | Go primitive failures with stack trace | `values/foreign_error.go` |
| `*values.StaticError` | Sentinel errors for `errors.Is` matching | `values/foreign_error.go` |

## Changelog

Wile maintains a hand-written `CHANGELOG.md` following [Keep a Changelog](https://keepachangelog.com/) format.

### Categories

Use exactly these section headers, in this order:

| Section | Use for |
|---------|---------|
| **Added** | New features, new primitives, new API surface |
| **Changed** | Changes to existing behavior or API |
| **Deprecated** | Features marked for future removal |
| **Removed** | Features or API that were deleted |
| **Fixed** | Bug fixes |
| **Security** | Vulnerability fixes |

Omit empty sections — only include categories that have entries for a given release.

### Writing Entries

- **Write for users, not developers.** "Fixed crash when reading empty port" not "Changed io.EOF handling in prim_read_write.go."
- **One bullet per user-visible change.** Internal refactors don't get entries unless they change observable behavior.
- **Start with a verb.** "Add", "Fix", "Remove", "Change" — not "Added support for..." (the section header already provides tense).
- **No commit hashes or PR numbers in entries.** The version link at the bottom points to the full diff.
- **Group related changes into a single entry** when they form a coherent feature (e.g., one entry for "Full numeric tower with exact/inexact distinction" rather than separate entries for Integer, Float, Rational, Complex).

### When to Update

**Every commit that changes user-visible behavior must include a corresponding `CHANGELOG.md` entry under `[Unreleased]`.** This includes new features, bug fixes, changed behavior, removed functionality, and dependency changes (like minimum Go version). Internal refactors that don't affect users don't need entries.

Update the changelog in a separate follow-up commit to the described change.

### Release Process

1. Accumulate entries under `[Unreleased]` as work lands on master
2. When cutting a release, rename `[Unreleased]` to `[X.Y.Z] - YYYY-MM-DD`
3. Add a fresh empty `[Unreleased]` section above it
4. Add a version link at the bottom: `[X.Y.Z]: https://github.com/aalpar/wile/compare/vPREV...vX.Y.Z`

## GitHub Workflow

### Creating Branches and Pull Requests

**All changes must go through branches and PRs.** Never commit directly to master, even for small fixes. This ensures all changes are reviewed and tracked.

When making changes, follow this workflow:

1. **Create a feature branch** with a descriptive name:
   ```bash
   git checkout -b feature/<descriptive-name>
   # Examples:
   #   feature/schelog-example-and-runtime-api
   #   fix/continuation-escape-handling
   #   refactor/macro-expander
   ```

2. **Run lint before staging** to catch issues early:
   ```bash
   make lint  # Must pass with 0 issues before committing
   ```

3. **Stage only related files** (don't mix unrelated changes):
   ```bash
   git add <specific-files>
   git diff --cached --stat  # Verify what's staged
   ```

4. **Commit with a conventional commit message**:
   ```bash
   git commit -m "$(cat <<'EOF'
   feat: short description of the change

   Longer explanation of what changed and why.
   Use bullet points for multiple items:
   - First change
   - Second change
   EOF
   )"
   ```

   Commit prefixes:
   - `feat:` - New feature
   - `fix:` - Bug fix
   - `refactor:` - Code restructuring without behavior change
   - `docs:` - Documentation only
   - `test:` - Adding or fixing tests
   - `chore:` - Maintenance tasks

5. **Push the branch**:
   ```bash
   git push -u origin feature/<descriptive-name>
   ```

6. **Create a PR with description**:
   ```bash
   gh pr create --title "feat: short description" --body "$(cat <<'EOF'
   ## Summary
   What this PR does.

   ## Changes
   - Change 1
   - Change 2

   ## Test Plan
   - [ ] Tests pass
   - [ ] Manual verification
   EOF
   )"
   ```

7. **Assign and label the PR** (optional):
   ```bash
   gh pr edit <PR-number> --add-assignee <username>
   gh pr edit <PR-number> --add-label "enhancement,documentation"
   ```

8. **Wait for CI to pass** before merging:
   ```bash
   gh pr checks <PR-number> --watch  # Watch CI status
   ```
   **IMPORTANT**: Do not merge until all CI checks are green. This prevents broken builds on master.

### Pull Request Merging

The repository owner is an administrator. When merging PRs to master, use the `--admin` flag to bypass branch protection rules if necessary:

```bash
gh pr merge <PR> --merge --admin --delete-branch
```

**CI Gate**: Even with admin privileges, always wait for CI automation to pass before merging. The `--admin` flag bypasses branch protection (e.g., required approvals), but should not be used to bypass failing CI. A PR with failing CI should be fixed, not force-merged.

Note: GitHub does not allow self-approval of PRs even with admin privileges, but admin merge ensures PRs can be merged without external approval.

### Automated Code Review

**GitHub Copilot Pull Request Reviewer is excellent.** It catches technical errors that matter:
- Factual inaccuracies in documentation (wrong output examples, incorrect claims)
- Mismatches between code behavior and documentation
- Command-line errors (wrong binary names, incorrect flags)
- Pattern mismatches (test discovery rules, file naming conventions)

**Trust Copilot's technical feedback.** When it points out specific issues with suggestions, those are typically accurate and actionable. Fix them before merging.

**Greptile** provides useful PR summaries and numerical verification but less deep technical review. Use it for sanity checks on claims about file counts, scope, and risk assessment.

## Release Process

Releases are automated via GoReleaser v2 (`.goreleaser.yml`) and a GitHub Actions workflow (`.github/workflows/release.yml`). Pushing a `v*` tag to the remote triggers the full release pipeline.

### Artifacts

Each release produces:

| Artifact | Contents |
|----------|----------|
| `wile-v{X.Y.Z}-{os}-{arch}.tar.gz` | `scheme` binary + LICENSE + README.md |
| `checksums.txt` | SHA256 checksums for all archives |

Platforms: darwin/linux × amd64/arm64 (4 archives total).

### How It Works

1. The `release.yml` workflow triggers on `v*` tag pushes
2. GoReleaser builds the `scheme` binary from `./cmd` for all 4 platform combinations with `CGO_ENABLED=0`
3. Binaries are stamped via ldflags: `-X main.BuildSHA={{ .ShortCommit }} -X main.BuildVersion={{ .Tag }}`
4. Archives and checksums are uploaded as a GitHub release
5. GitHub generates the release changelog from commit messages (`changelog.use: github-native`)
6. No manual secrets needed — `GITHUB_TOKEN` is provided automatically by GitHub Actions

### Cutting a Release

1. Ensure all changes are on master and `[Unreleased]` in `CHANGELOG.md` is up to date
2. Rename `[Unreleased]` to `[X.Y.Z] - YYYY-MM-DD` in `CHANGELOG.md`
3. Add a fresh empty `[Unreleased]` section above it
4. Update `VERSION` to `vX.Y.Z`
5. Commit: `chore: set X.Y.Z release date in changelog`
6. Tag and push:
   ```bash
   git tag vX.Y.Z
   git push origin master vX.Y.Z
   ```
7. The release workflow runs automatically — verify at `https://github.com/aalpar/wile/releases`

### Local Verification

Before tagging, validate the GoReleaser config and do a dry run:

```bash
make release-check      # Validates .goreleaser.yml syntax
make release-snapshot   # Builds all archives locally without publishing
ls dist/                # Inspect output: 4 tar.gz files + checksums.txt
```

### Key Files

| File | Purpose |
|------|---------|
| `.goreleaser.yml` | GoReleaser v2 config: builds, archives, checksums, release notes |
| `.github/workflows/release.yml` | GitHub Actions workflow triggered on `v*` tag pushes |
| `VERSION` | Current version string (e.g., `v1.0.4`) |
| `CHANGELOG.md` | Hand-written release notes following Keep a Changelog format |

## Important: Plan File Location

**Plans go in `plans/`.**

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
- `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` - Documented differences between implementation and R7RS specification
- `docs/dev/ENVIRONMENT_SYSTEM.md` - Environment system architecture and usage guide
- `docs/dev/NUMERIC_TOWER.md` - Numeric tower architecture (direct dispatch, no Tower* layer)

### Plan Files

| File | Purpose | Status |
|------|---------|--------|
| `plans/PERFORMANCE_REFACTORING_PLAN.md` | Full-pipeline performance refactoring (8 phases) | Planned |
| `plans/CODE_CONSOLIDATION_ARCHITECTURAL.md` | Operation code generation (optional, HIGH risk) | Reference |
| `plans/ALGEBRAIC_REDUCTIONS.md` | Structural simplifications (numeric tower, ports, ops) | Reference |
| ~~`plans/REFACTORING_OPPORTUNITIES.md`~~ | ~~Smaller refactoring opportunities catalog~~ | Complete (deleted) |
| `plans/EXTERNAL_EXTENSIONS_PLAN.md` | Public extension system | Proposed |
| `plans/PLUGIN_ARCHITECTURE_PROPOSAL.md` | Plugin architecture design | Proposed |
| `plans/TOKENIZER_CONSOLIDATION_PLAN.md` | Tokenizer reader consolidation | Planned |
| ~~`plans/MACHINE_COVERAGE_PLAN.md`~~ | ~~machine/ package test coverage improvement~~ | Complete (deleted) |
| ~~`plans/COMPILE_VALIDATED_COVERAGE.md`~~ | ~~compile_validated.go coverage improvement~~ | Complete (deleted) |
| ~~`plans/QUASISYNTAX_COVERAGE_PLAN.md`~~ | ~~Quasisyntax test coverage~~ | Complete (deleted) |
| `plans/EMPTY_LIST_VOID_REFACTORING.md` | EmptyList/Void type system refactoring | Reference |
| ~~`plans/SYNTAX_VALUE_REFACTORING.md`~~ | ~~Match package SyntaxValue refactoring~~ | Complete (deleted) |
| `plans/TOP_LEVEL_ENVIRONMENT.md` | TopLevelEnvironment introduction | Reference |
| `plans/HYGIENE_DEBUGGING_DESIGN.md` | Hygiene debugging tooling | Planned |
| `plans/MACRO_EXPANSION_TRACING.md` | Macro expansion tracing | Planned |
| `plans/SYSTEMATIC_DEBUG_LOGGING.md` | Debug methodology | Reference |
| `plans/SCHEME_EXAMPLES.md` | Showcase examples and benchmarks | Planned |
| `plans/AUTHORIZATION_FRAMEWORK.md` | K8s-style authorization (verb+resource) for extensions | Planned |

### Design Documents

| File | Purpose |
|------|---------|
| `docs/design/CONTINUATION_ESCAPE_DESIGN.md` | First-class continuation escape mechanism design |
| `docs/design/DELIMITED_CONTINUATIONS.md` | Delimited continuations: prompts, abort, composable capture |
| `docs/dev/ENVIRONMENT_SYSTEM.md` | Environment system architecture |
| `docs/dev/NUMERIC_TOWER.md` | Numeric tower architecture (direct dispatch, lattice model) |
| `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` | Documented R7RS specification deviations |

**Plan file naming**: Use `UPPERCASE_WITH_UNDERSCORES.md` (e.g., `OPTIMIZATION_PLAN.md`, `TESTING_PLAN.md`).

## R7RS Conformance

This project aims to implement R7RS-small. Key resources:

| Source | URL |
|--------|-----|
| R7RS-small PDF | https://small.r7rs.org/attachment/r7rs.pdf |
| R7RS Corrected (HTML) | https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html |
| R7RS-large Wiki (in progress) | https://codeberg.org/scheme/r7rs/wiki |

**Testing policy**: Tests that conform to R7RS must not be removed or reverted. If a test fails but correctly reflects R7RS behavior, the implementation must be fixed—not the test. See `registry/core/CLAUDE.md` for detailed test organization.

### R7RS Specification Comments

Functions implementing R7RS-specified behavior must include comments citing the relevant specification section. This ensures traceability and helps maintain conformance.

### Citing Design and Implementation Influences

When a design choice or implementation technique is drawn from an external source, cite it in code comments. This includes algorithms, data structure choices, and semantic decisions influenced by other work.

**What to cite**:
- Algorithms or techniques adopted from other Scheme implementations (Chez Scheme, Racket, Guile, Chibi-Scheme, Gambit, MIT/GNU Scheme, CHICKEN, etc.)
- Academic papers and their specific contributions (e.g., "optimistic bisimilarity — Chez Scheme, Racket")
- Books (SICP, TSPL, Lisp in Small Pieces, R. Kent Dybvig's writings, etc.)
- SRFIs that inform the design beyond their specification text
- Web resources with substantive technical content (blog posts, implementation notes)

**Format**: Cite inline in doc comments, close to the code the influence applies to. Name the source and what was adopted.

**Examples**:
```go
// Uses optimistic bisimilarity with a visited set to terminate on
// circular structures per R7RS §6.1. This is the same technique
// used by Chez Scheme and Racket.

// Scope sets follow Flatt 2016 ("Binding as Sets of Scopes").

// Floyd's cycle detection (tortoise-and-hare) as used in Chibi-Scheme's
// proper-list check.
```

**Why**: Citing sources makes the codebase self-documenting about *why* things are done a certain way, not just *what* they do. It helps future contributors understand the provenance of design decisions and find the original material for deeper understanding.

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
