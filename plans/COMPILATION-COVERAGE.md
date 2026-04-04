# Plan: Bring machine/compilation Coverage to 80%

**Status:** Draft
**Created:** 2026-04-04
**Current coverage:** 68.6% (2550/3718 statements covered)
**Target:** 80% (2974 statements) → need 424 more covered statements

## Problem

`machine/compilation` was extracted from `machine/` in PR #593 (2026-03-30).
It's excluded from `covercheck.sh` with the note "newly-extracted packages
pending test reorganization." The gap is genuine — even cross-package Engine
tests only reach 5–9% of the worst files.

## Approach

Mixed strategy:
- **Scheme-level tests** via a new Engine-like test helper for happy paths
  (each `(import ...)` or `(syntax-case ...)` exercises many internal functions)
- **White-box unit tests** for error branches and edge cases unreachable from Scheme

## Phase 0: Test Infrastructure

**New helper:** `SetupEngineTest` in `machine/testutil/testutil.go`

Creates a test environment that mirrors what `wile.NewEngine()` does, without
importing the root `wile` package (which would create a circular dependency):

1. `bootstrap.NewNamespaceFrameTiny()` — full runtime env
2. `compilation.NewLibraryRegistry()` — library registry
3. `compilation.NewFSFileResolver(fsys, env)` — virtual FS for test `.sld` files
4. `env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)`
5. Wire `LibraryRegistry` and `FileResolver` onto the namespace

Accepts an `fs.FS` parameter so callers can use `testing/fstest.MapFS` with
inline `.sld` content — no temp directories needed.

Returns the env, usable with `testutil.EvalSchemeInEnv`.

## Phase 1: Library System (~393 uncovered stmts)

Target files and current coverage:

| File | Stmts | Covered | % | Approach |
|------|-------|---------|---|----------|
| `library_loader.go` | 89 | 0 | 0% | Scheme-level: `(import ...)` with test `.sld` |
| `library_bindings.go` | 104 | 1 | 1% | Scheme-level + white-box for modifiers |
| `compile_import.go` | 24 | 0 | 0% | Scheme-level: `(import ...)` |
| `compile_time_continuation_library.go` | 26 | 0 | 0% | Scheme-level: library with `(begin ...)` body |
| `compile_library_forms.go` | 146 | 43 | 29.5% | Scheme-level: `define-library` with exports |
| `library_registry.go` | 84 | 36 | 42.9% | White-box: registry methods, loading state |

### Test cases needed

**Scheme-level** (in `machine/compilation/library_loader_test.go`, external package):
- `(import (test lib))` with a minimal `.sld` that exports one binding
- `(import (only (test lib) foo))` — only modifier
- `(import (except (test lib) bar))` — except modifier
- `(import (prefix (test lib) t:))` — prefix modifier
- `(import (rename (test lib) (foo my-foo)))` — rename modifier
- Library with `(begin (define x 42))` body — exercises `compileLibraryBegin`
- Library with `(include "helper.scm")` — exercises include in library context
- Library with `(export (rename internal-name external-name))` — rename exports
- Library with `(cond-expand ...)` — exercises `processCondExpand` in library context
- Error: circular dependency (lib A imports lib B imports lib A)
- Error: library name mismatch
- Error: missing library file
- Error: no file resolver configured

**White-box** (in existing `library_internal_test.go`, internal package):
- `ApplyToExports` with only/except/prefix/rename modifiers
- `LoadLibrary` with nil registry
- `loadLibraryFromReader` with malformed content
- `findLibraryBinding` for missing bindings
- `LibraryRegistry.IsLoading` / `StartLoading` / `FinishLoading`
- `fireImportObserver` with/without observer

## Phase 2: Syntax-Case & With-Syntax (~189 uncovered stmts)

| File | Stmts | Covered | % | Approach |
|------|-------|---------|---|----------|
| `compile_syntax_case.go` | 118 | 14 | 11.9% | Scheme-level + white-box |
| `compile_with_syntax.go` | 83 | 29 | 34.9% | Scheme-level + white-box |
| `compile_define_for_syntax.go` | 43 | 12 | 27.9% | Scheme-level |

### Test cases needed

**Scheme-level** (in respective `_test.go` files, external package):
- Basic `syntax-case` with single clause, pattern vars → `compileSyntaxCaseClause`
- `syntax-case` with fender (guard) → fender branch code
- `syntax-case` with multiple clauses → clause iteration + patching
- `syntax-case` with literals → `extractLiteralsWithSyntax` path
- `with-syntax` with bindings → full transformation to `syntax-case`
- `with-syntax` with multiple bindings → pattern/expr collection loop
- `with-syntax` with empty bindings → `compileWithSyntaxBody` path
- `with-syntax` body with multiple expressions → begin wrapping
- `define-for-syntax` basic usage → compile-time definition

**White-box error cases:**
- `compileSyntaxCaseClause` with bad pattern
- `createPatternVarEnvironment` (already 0% — called only from `compileSyntaxCaseClause`)
- `CompileWithSyntax` with non-list bindings
- `buildWithSyntaxBegin` edge cases

## Phase 3: Expander + Near-Threshold Files (~277 uncovered stmts)

These files are 61–78% and need targeted tests for specific uncovered branches.

| File | Stmts | Covered | % | Gap |
|------|-------|---------|---|-----|
| `expander_primitive_forms.go` | 146 | 89 | 61.0% | 57 |
| `expander_lambda.go` | 131 | 91 | 69.5% | 40 |
| `expander_time_continuation.go` | 172 | 122 | 70.9% | 50 |
| `compile_cond_expand.go` | 116 | 79 | 68.1% | 37 |
| `compile_syntax_rules.go` | 181 | 132 | 72.9% | 49 |
| `compile_eval_when.go` | 80 | 61 | 76.2% | 19 |
| `compile_transformer.go` | 37 | 29 | 78.4% | 8 |
| Others | ~100 | ~70 | ~70% | ~30 |

### Approach

Investigate each file's uncovered blocks (via `go tool cover -html`) and write
targeted Scheme-level tests that exercise specific branches:

- `expandImportForm` (0%) — covered by Phase 1 library tests
- `expandWithBindingScope` (0%) — `(let ((=> #f)) (cond ...))` pattern
- `extractIdentifierList` (0%) in `expander_lambda.go` — case-lambda with rest args
- `collectPatternVariables` (0%) — syntax-rules with nested patterns
- `compile_cond_expand.go` — `(cond-expand (library ...) ...)` and nested requirements
- `compile_eval_when.go` — `(eval-when (expand) ...)` compile-time eval
- `compile_syntax_rules.go` — ellipsis-in-literals, custom ellipsis identifier

## Phase 4: Remove Exclusion

After coverage reaches 80%:
1. Remove `"machine/compilation"` from `EXCLUDED_PKGS` in `covercheck.sh`
2. Verify `make covercheck` passes
3. PR

## Budget Estimate

We need 424 statements. Expected yield per phase:

| Phase | Expected New Covered | Running Total |
|-------|---------------------|---------------|
| 1: Library system | ~250 | 2800 |
| 2: Syntax-case/with-syntax | ~120 | 2920 |
| 3: Expander + near-threshold | ~80 | 3000+ |

Target: 2974 (80%). This gives ~26 statements of buffer.

## Implementation Order

1. Phase 0 (test helper) — unblocks everything else
2. Phase 1 (library system) — biggest bang per test
3. Phase 2 (syntax-case) — user-requested priority
4. Phase 3 (expander) — targeted gap-filling
5. Phase 4 (remove exclusion) — verify and ship
