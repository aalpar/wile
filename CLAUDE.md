# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Wile is a Scheme interpreter/compiler in Go with hygienic macros. It compiles Scheme to bytecode and executes it on a stack-based virtual machine, implementing R7RS-style `syntax-rules` macros with a "sets of scopes" hygiene model (Flatt 2016).

## Product Vision

Wile is **a Scheme scripting layer that feels native to Go**, not a Scheme that happens to be written in Go.

- **Full R7RS compliance is the baseline.** Compliance is the floor, not the ceiling.
- **Embedding is the product.** Pure Go (no CGo), `go get` dependency, idiomatic API for Go developers.
- **Performance is explicitly deprioritized.** Correctness, ergonomics, and embedding quality matter more. Target workloads (config, policy, scripting) are not bottlenecked on interpreter speed.

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
**NEVER** write code that exclusively accepts `*values.Pair` for read-only operations. Use `values.Tuple` interface instead to support both `*Pair` and `*ArrayList`. Only use `*values.Pair` when mutation (`SetCar`, `SetCdr`) or type-specific predicates (`pair?`) are required.

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

## Code & Style

- Lowercase filenames, no uppercase or underscores in package names
- Avoid generic `util` packages — put helpers where they're used
- Comments explain *why*, not *what* — non-obvious logic gets context, obvious code gets none
- Table-driven tests are the norm for multiple scenarios (see `registry/CLAUDE.md`)
- All new packages require unit tests; significant features need integration tests in `integration/`

## Git Workflow

- `git fetch` + `git rebase`, never `git pull` (merge commits block PRs)
- Never push to upstream master — always branch + PR
- Squash fixup commits after review, not before

## Go Conventions

After any Go code changes, run `make lint` (or at minimum `goimports -w` on changed files) before considering the task complete. Do not report completion with outstanding formatting or import issues.

### Error Handling (summary)

Two-layer convention: **sentinel + wrap**. Use `values.NewStaticError` for sentinels, `values.WrapForeignErrorf` at return sites. Never use bare `errors.New` or `fmt.Errorf` in production code. Always use `errors.Is`/`errors.As`, never `==`/`!=`.

### Type Switches: Interfaces vs Concrete Types

**When debugging type switch issues, READ the actual case types carefully.** Do not assume.

- `case Interface:` matches all types implementing that interface
- `case *ConcreteType:` matches only that specific pointer type

When debugging predicates or type-based dispatch, read the existing cases word-for-word before proposing changes.

### Tuple vs *Pair

Use `values.Tuple` for read-only operations, `*values.Pair` only for mutation or type predicates.

| Use Case | Type | Why |
|----------|------|-----|
| Traversal, pattern matching, assoc lookup | `values.Tuple` | Generic (works with `*Pair`, `*ArrayList`) |
| List copying | Input: `Tuple`, Output: `*Pair` | Read generically, write concretely |
| `list-set!`, `set-car!`, `set-cdr!` | `*values.Pair` | Needs `SetCar`/`SetCdr` |
| `pair?` predicate | `*values.Pair` | Type-specific per R7RS |

Compile-time/macro code uses `*Pair` only (no `ArrayList` at those phases).

## Build Commands

```bash
make build    # Build to ./dist/{os}/{arch}/scheme
make test     # Run all tests (go test -v ./...)
make lint     # Run golangci-lint
go test -v -run TestName ./package/...  # Run a single test
```

See `cmd/CLAUDE.md` for full build commands, dist/ structure, and REPL usage.

## References

- `TODO.md` — Pending tasks, missing R7RS features, future extensions
- `CODING_STYLE.md` — Comprehensive style guide
- `PRIMITIVES.md` — Complete primitives reference
- `BIBLIOGRAPHY.md` — Academic papers, specifications, canonical references
- `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` — Documented R7RS specification deviations
- `docs/dev/ENVIRONMENT_SYSTEM.md` — Environment system architecture
- `docs/dev/NUMERIC_TOWER.md` — Numeric tower architecture
- `docs/EXTENSIONS.md` — Extension system architecture and authoring guide
- `docs/EXTENSION_LIBRARIES.md` — R7RS library integration for extensions
- `plans/CLAUDE.md` — Active plan files and design documents
