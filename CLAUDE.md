# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Wile is a Scheme interpreter/compiler in Go with hygienic macros. It compiles Scheme to bytecode and executes it on a stack-based virtual machine, implementing R7RS-style `syntax-rules` macros with a "sets of scopes" hygiene model (Flatt 2016).

## Product Vision

Wile is **a Scheme scripting layer that feels native to Go**, not a Scheme that happens to be written in Go.

- **Full R7RS compliance is the baseline.** Compliance is the floor, not the ceiling.
- **Embedding is the product.** Pure Go (no CGo), `go get` dependency, idiomatic API for Go developers.

## Imperatives (Never Deviate)

These are exact patterns. Do not improvise or substitute alternatives.

| Wrong | Correct | Note |
|-------|---------|------|
| Creating plans in random locations | Creating plans in `plans/` | Plans live at repo root |
| `if x := f(); x != nil {` | `x := f()` then `if x != nil {` | No compound if-assignments |
| `func foo() int { return x }` | Multi-line function body | **NEVER** write single-line function definitions |
| `panic(werr.ErrFoo)` | `panic(werr.WrapForeignErrorf(werr.ErrFoo, "site: what failed"))` | **NEVER** panic with raw errors — always wrap with location context |

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
**NEVER** commit directly to master. All changes must go through feature branches. Pull requests are optional: open one when the user asks, or when the change is large/risky enough to benefit from Copilot or crosscheck review. Otherwise the standard path is branch → commit → push → merge to master directly once CI is green. Do NOT merge any PR that does get opened without explicit instruction.
**NEVER write single-line function definitions.** This applies to ALL function forms:
named functions, methods, closures (inline, deferred, goroutine, or assigned), and
function arguments. Every function body MUST start on the line after the opening brace
and the closing brace MUST be on its own line. No exceptions.
**NEVER** write code that exclusively accepts `*values.Pair` for read-only operations. Use `values.Tuple` interface instead. Only use `*values.Pair` when mutation (`SetCar`, `SetCdr`) or type-specific predicates (`pair?`) are required.

## Workflow

When working from `TODO.md` or a phased plan, read and update `TODO.md` after completing each phase. Mark items done as you go so progress is visible and no work gets repeated across sessions.

**The build is not clean until `make lint && make covercheck` both pass.** Run both after any code changes and fix all failures before claiming the task complete.

## Session Planning

Finish codebase reading and exploration before the session ends. If a plan is too large to complete in one session, break it into smaller chunks that can each be completed independently. Partial exploration with no code changes is wasted work.

## Wile Architecture

1. **Language**: R7RS Scheme with hygienic macros (sets of scopes per Flatt 2016), first-class continuations, numeric tower
2. **Execution**: Bytecode VM with explicit PC loop (`MachineContext.Run()` in `machine_context.go`), separate eval stack — NOT tree-walking interpreter
3. **Continuations**: Explicit `MachineContinuation` linked list — NOT Go call stack; enables `call/cc`, dynamic-wind, delimited continuations
4. **Pipeline**: `Tokenizer → Parser → Expression → Expander → Compiler → VM` (string → tokens → SyntaxValue → *Expression → bytecode → execution)
5. **Packages (public)**: `wile/` (Engine, embedding API), `values/` (Scheme types), `werr/` (error infrastructure), `registry/` (primitives/extensions), `security/` (authorization), `extensions/` (public extensions), `repl/` (REPL for embedders), `docparse/` (docstring parsing)
6. **Packages (internal)**: `machine/` (VM/compiler/expander), `environment/` (bindings/scopes), `internal/{tokenizer,parser,syntax,match,bootstrap,validate,schemeutil,forms,extensions}`
7. **Values**: Go heap objects managed by Go GC — pure Go, no CGo, no custom allocator
8. **Error handling**: Sentinel + wrap pattern — `werr.NewStaticError` for sentinels, `werr.WrapForeignErrorf` for context; never `fmt.Errorf`
9. **Hygiene**: Identifiers carry scope sets, resolution checks `bindingScopes ⊆ useScopes`; free template identifiers skip intro scope
10. **Concurrency**: Engine not thread-safe (one per goroutine); SRFI-18 threads within Engine safe (VM manages coordination)
11. **Security**: Two-layer sandboxing — extension-level (opt-in via `WithExtension()`) and fine-grained authorization (`security.Authorizer` interface with `security.Check()` gating)
12. **Source loading**: All include/load/import goes through `FileResolver` interface (`machine/file_resolver.go`). Four implementations: `OSFileResolver` (OS filesystem), `FSFileResolver` (virtual `fs.FS` via `WithSourceFS`), `EmbedFileResolver` (bootstrap), `ChainFileResolver` (searches multiple resolvers in order). Multiple `WithSourceFS` calls build a chain; `WithSourceOS()` appends OS as fallback.

### Pipeline

```
string → Tokenizer(internal/tokenizer) → Parser(internal/parser) → SyntaxValue
  → *Expression(expression.go, Engine.Parse())
  → Expander(machine/expander_*.go) → Compiler(machine/compile_*.go) → NativeTemplate
  → VM(machine/machine_context.go, MachineContext.Run()) → values.Value
```

Single-expression entry: `Engine.Parse()` → `*Expression` → `Engine.Eval()` or `Engine.Compile()` + `Engine.Run()`
Multi-expression entry: `Engine.EvalMultiple()` (string → parse/expand/compile/run loop internally)

`*Expression` wraps a single `SyntaxValue` — the "exactly one expression" constraint
is enforced at parse time by `Parse`, not by `Eval`/`Compile`. This prevents silent
partial consumption of multi-expression input. See `expression.go`.

### Package Layering

```
werr/ → values/ → docparse/ → environment/ → internal/{tokenizer,parser,syntax,schemeutil,validate,match,bootstrap,extensions,forms}
  → machine/ + security/ → registry/ → extensions/ → wile/ (root) → repl/
```

Note: `machine/` and `security/` are peers — `machine/` imports `security/` for authorization gate sites, but `security/` has no dependency on `machine/`.

Public API (embedders): `wile/`, `values/`, `werr/`, `registry/`, `security/`, `extensions/*`, `repl/`, `docparse/`. Internal: `internal/*`. Machine: public but rarely used directly.

### Security Model

Two-layer sandboxing for embedded use:

1. **Profile-based** (primary API): `WithProfile(p)` selects a named bundle of extensions + authorizer. Profiles: `Tiny` (core only), `Console` (I/O + /tmp files + sandboxed env), `ConsoleWithLoad` (Console + eval/load under /tmp), `Small` (R7RS-small baseline), `KitchenSink` (everything). Orthogonal modifier `WithSandbox()` wraps the authorizer with an env-map restriction. Ad-hoc `WithExtension()` still available for callers who need a custom mix.
2. **Fine-grained authorization**: `security.Authorizer` interface gates privileged operations at runtime. K8s-style vocabulary: Resource (`file`, `code`, `env`, `process`) + Action (`read`, `write`, `delete`, `load`, `exit`). Built-in authorizers: `ConsoleAuthorizer` (/tmp file access, deny code/process), `ConsoleWithLoadAuthorizer` (Console + /tmp code:load). Set via `WithAuthorizer()` or via a profile. Gate sites: files, system, eval extensions; `include`; library import. Scheme-level `(environment '(wile <profile>))` constructs a fresh namespace for a named profile.

## Code & Style

- Lowercase filenames, no uppercase or underscores in package names
- Avoid generic `util` packages — put helpers where they're used
- Comments explain *why*, not *what* — non-obvious logic gets context, obvious code gets none
- Table-driven tests are the norm for multiple scenarios (see `registry/CLAUDE.md`)
- All new packages require unit tests; significant features need integration tests in `integration/`
- **Early return from functions.** Check preconditions and known failure modes first, return early on error/edge cases, keep the happy path flat and unindented. No nested if/else chains when guard clauses suffice. See `CODING_STYLE.md` for examples.

## Git Workflow

- `git fetch` + `git rebase`, never `git pull` (merge commits block PRs)
- Never push to upstream master — always branch + PR
- Squash fixup commits after review, not before

## Go Conventions

After any Go code changes, run `make lint` (or at minimum `goimports -w` on changed files) before considering the task complete. Do not report completion with outstanding formatting or import issues.

### Variable Naming Convention

| Name | Role | Rationale |
|------|------|-----------|
| `p` | Method receiver (when the body uses it) | Type is in the signature; role is clear from being a receiver. No need to name it after the type. Exception: compiler uses `c`. **When the receiver is unused** (e.g., empty marker methods, dispatch methods that consult only parameters), omit the name entirely: `func (*T) Method(...)`. Go's "unused receiver" idiom takes precedence over the `p` convention — naming a receiver you never reference is noise. |
| `q` | Primary return value | Assign `q` as early as possible so the reader can track "this is what gets returned" through the code flow. |
| `err` | Error return value | Standard Go convention. |

These names save mental space: `p` is always the receiver, `q` is always the result being built, `err` is always the error. No need to invent descriptive names for roles that are already clear from position.

### Error Handling (summary)

Two-layer convention: **sentinel + wrap**. Use `werr.NewStaticError` for sentinels, `werr.WrapForeignErrorf` at return sites. Never use bare `errors.New` or `fmt.Errorf` in production code. Always use `errors.Is`/`errors.As`, never `==`/`!=`.

### Type Switches: Interfaces vs Concrete Types

**When debugging type switch issues, READ the actual case types carefully.** Do not assume.

- `case Interface:` matches all types implementing that interface
- `case *ConcreteType:` matches only that specific pointer type

When debugging predicates or type-based dispatch, read the existing cases word-for-word before proposing changes.

### Tuple vs *Pair

Use `values.Tuple` for read-only operations, `*values.Pair` only for mutation or type predicates.

| Use Case | Type | Why |
|----------|------|-----|
| Traversal, pattern matching, assoc lookup | `values.Tuple` | Generic (works with `*Pair`, `emptyListType`) |
| List copying | Input: `Tuple`, Output: `*Pair` | Read generically, write concretely |
| `list-set!`, `set-car!`, `set-cdr!` | `*values.Pair` | Needs `SetCar`/`SetCdr` |
| `pair?` predicate | `*values.Pair` | Type-specific per R7RS |

## Build Commands

```bash
make build    # Build to ./dist/{os}/{arch}/wile
make test     # Run all tests (go test -v ./...)
make lint     # Run golangci-lint
go test -v -run TestName ./package/...  # Run a single test
```

See `cmd/CLAUDE.md` for full build commands, dist/ structure, and REPL usage.

## References

- `memory/` — Historical design decisions, failed experiments, and lessons learned (Claude Code auto-memory). Exists to prevent re-treading old territory or repeating past mistakes. **Check before proposing optimizations or architectural changes.**
- `TODO.md` — Pending tasks, missing R7RS features, future extensions
- `CODING_STYLE.md` — Comprehensive style guide
- `PRIMITIVES.md` — Complete primitives reference
- `BIBLIOGRAPHY.md` — Academic papers, specifications, canonical references
- `docs/reference/r7rs-differences.md` — Documented R7RS specification deviations
- `docs/environment/system.md` — Environment system architecture
- `docs/numeric/tower.md` — Numeric tower architecture
- `docs/dev/debug-methodology.md` — Systematic debug logging methodology and Go gotchas
- `docs/dev/iteration-idioms.md` — Four iteration shapes (`ForEach` / `iter.Seq` / mutex-snapshot / error-callback) and when to use each
- `docs/extensions/architecture.md` — Extension system architecture and authoring guide
- `docs/extensions/libraries.md` — R7RS library integration for extensions
- `docs/embedding/source-loading.md` — FileResolver chain, embedded stdlib, library import resolution
- `docs/compiler/peephole-optimizer.md` — Superinstruction formation, 3-pass pipeline, promoted opcodes
- `docs/coverage/scheme-coverage.md` — Scheme-level line coverage (`--cover`, `coverage` package)
- `plans/CLAUDE.md` — Active plan files and design documents
