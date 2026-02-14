# Staff Engineer Technical Debt Assessment

**Assessment Date**: 2026-02-13 (updated)
**Scope**: Recently changed files (last 10 commits)
**Files Analyzed**: 35 files across `values/`, `registry/core/`, `machine/`, `internal/extensions/io/`, `internal/parser/`

---

## Executive Summary

The recently changed files reflect a mature codebase with strong architectural discipline (overflow-safe arithmetic, hygiene-aware macros, R7RS compliance). The dominant debt is **structural duplication** in the numeric tower, where a well-designed abstraction (the Number interface) is undermined by 28 hand-rolled type switches. This is the classic "missing abstraction" smell — the lattice model exists conceptually but isn't reified into data.

Secondary issues are **extension friction** (EqualTo symmetry is N×N) and **helper misplacement** (numeric comparison helpers far from type definitions).

The codebase is **ready for the next phase of growth** (new numeric types, async I/O, performance tuning) but will benefit significantly from addressing the numeric tower duplication before adding complexity.

---

## Prioritized Findings

### [Priority: High] — Numeric Tower Type-Switch Explosion (Deferred)

**Where**: `values/{integer,rational,complex,big_integer,big_float,big_complex}.go` lines 169-300 (Add/Subtract/Multiply/Divide methods)

**What**: Each of the 7 numeric types implements 4 arithmetic operations with nearly identical type-switch dispatch over all 7 types. This is ~28 methods containing ~200 lines each of duplicated logic. The pattern is:
```go
switch v := o.(type) {
case *Integer: /* operation */
case *BigInteger: /* operation */
case *Float: /* operation */
// ... 4 more cases
}
```

**Why it matters**:
- **Maintenance burden scales quadratically**: Adding a new numeric type (e.g., `Decimal`) requires touching 7 files × 4 operations = 28 methods
- **Bug fix propagation**: Fixing a promotion rule (e.g., exact zero domination in multiplication) requires identical edits in 7 places
- **Test coverage explosion**: Numeric tower coverage tests already enumerate 49 type combinations (`values/numeric_tower_coverage_test.go`)

**Suggested fix**: Extract a lattice-based dispatch table mapping `(TypeA, TypeB) → promoter func`. Each type's arithmetic methods become lookups + delegation.

**Effort**: L (3-5 days for design + implementation + comprehensive testing)

---

### [Priority: Medium] — EqualTo Symmetry Requires N×N Awareness (Deferred)

**Where**: `values/integer.go:470-480`, `rational.go:402-412`, `big_integer.go` (similar)

**What**: Each numeric type's `EqualTo` method must handle comparisons with related types:
```go
func (p *Integer) EqualTo(v Value) bool {
    switch other := v.(type) {
    case *Integer: /* ... */
    case *BigInteger: /* symmetry for BigInteger.EqualTo(Integer) */
    case *Rational: /* symmetry for Rational.EqualTo(Integer) */
    }
}
```

**Why it matters**:
- **Symmetry breakage risk**: If `A.EqualTo(B)` is implemented but `B.EqualTo(A)` is not, equality is asymmetric
- **Extension friction**: Adding `Decimal` requires updating Integer, Rational, and Float to recognize it
- **No compile-time enforcement**: Go doesn't enforce symmetry; relies on manual cross-referencing

**Suggested fix**: Centralize equality in a type-agnostic dispatcher using `Number.Compare()`. Each type only implements `Compare` (already exists); equality becomes `a.Compare(b) == 0`.

**Effort**: M (1-2 days for refactor + test verification)

---

---

## Assessment Methodology

**Files Analyzed**: 35 files from `git diff --name-only HEAD~10`
**Focus Areas**:
- Structural debt (missing abstractions, divergent patterns)
- Consistency debt (naming drift, convention violations)
- Testing debt (missing coverage, brittle tests)
- Evolution risk (hardcoded assumptions, scaling walls, extension friction)

**Priority Criteria**:
- **High**: Actively blocking features, spreading, or causing bugs
- **Medium**: Slowing development but not breaking anything today
- **Low**: Aesthetic or minor — fix opportunistically

---

# Full Codebase Technical Debt Assessment

**Assessment Date**: 2026-02-13 (updated)
**Scope**: Entire codebase (605 Go source files, ~57K lines)
**Methodology**: Systematic review across all packages for structural debt, consistency issues, testing gaps, and evolution risks

---

## Executive Summary

The Wile codebase is **architecturally sound with recent correctness improvements** (10 HIGH, 2 MEDIUM bugs fixed in last architectural review). The core packages (values, machine, environment, syntax) have 85-93% test coverage and follow consistent error handling conventions (~772 wrapped errors, ~2 bare errors remaining as panic-recovery wrappers). The macro system's three-layer architecture (pattern matching VM + syntax adapter + hygienic layer) is well-factored and documented.

**Primary debt**: Testing gaps in user-facing boundaries (`cmd/scheme` at 9.9%, I/O at 55.2%).

**Secondary debt**: Duplication in scope-aware lookup (compiler vs expander), operation boilerplate (~32 hand-written files), and tokenizer readers (3 similar functions). These make adding new features (operations, syntax types, token categories) more expensive than necessary.

---

## HIGH PRIORITY

### [Priority: High] — CLI Package Test Coverage: 9.9% for User-Facing Entry Point

**Where**: `cmd/scheme/main.go` (~280 lines including flags, REPL, file loading, error handling)

**What**: The main CLI entry point has only 9.9% test coverage despite containing critical logic for flag parsing, library path resolution, REPL interaction, and error formatting. Functions like `main()`, `runREPL()`, and `loadFiles()` are untested. The `--file` flag (repeatable), `--interactive` mode, and `--library-path` parsing have no regression tests.

**Why it matters**: The CLI is the primary user interface. Bugs here (incorrect flag handling, REPL crashes, library loading failures) directly impact user experience. The recently added GNU-style long flags (`--file`, `--interactive`, `--library-path`) have no tests verifying they work. Adding new flags or changing behavior is risky without coverage.

**Suggested fix**: Add `cmd/scheme/main_test.go` with integration tests: spawn `scheme` subprocess, pass flags, check output. Test: help text, version, multiple `--file` args, `--interactive` after file load, `SCHEME_INCLUDE_PATH` resolution, error messages for missing files, REPL exit conditions.

**Effort**: M (requires subprocess spawning pattern, mock stdin/stdout, ~3-5 days)

---

### [Priority: High] — I/O Extension Test Coverage: 55.2% for 912-Line File I/O Package

**Where**: `internal/extensions/io/prim_read_write.go` (~824 lines, 55.2% coverage)

**What**: The I/O primitives (`read`, `read-line`, `read-bytevector`, `write`, `write-shared`, `write-simple`) have 55.2% test coverage. Error paths for malformed input, EOF handling, circular structure detection, and bytevector partial reads are undertested. The recently fixed M10/M11 issues (partial bytevector read, unbounded allocation) indicate this area is bug-prone.

**Why it matters**: I/O is a boundary with the outside world—untested error paths become production crashes. The `read` primitive interacts with tokenizer/parser, and bugs here can expose parser edge cases. `write-shared` implements datum label graph serialization (`#0=` / `#0#`), which is complex and undertested.

**Suggested fix**: Add dedicated error path tests: `read` on truncated input, `read-bytevector` with exact buffer size vs. larger, `write-shared` with deeply nested circular structures, `read-line` at EOF, binary vs textual port type mismatches.

**Effort**: M (3-4 days for comprehensive error path coverage)

---

## MEDIUM PRIORITY

### [Priority: Medium] — Pattern Matching Adapter Layer: Two Separate Conversions for Syntax ↔ Raw Values

**Where**: `internal/match/syntax_adapter.go:158-263` (two distinct conversion functions)

**What**: The pattern matching VM operates on raw `values.Value` trees. The macro system uses `syntax.SyntaxValue` with scopes. The adapter layer converts between them:
- `ConstructPatternTree(syntax.SyntaxValue) (values.Value, error)` — strips scopes for pattern matching
- `fromPatternValue(values.Value, []ScopeID) (syntax.SyntaxValue, error)` — adds scopes back after template expansion

These are inverses, but the relationship is implicit. Adding a new `SyntaxValue` type (e.g., `SyntaxBox`, `SyntaxRecord`) requires updating both functions symmetrically or pattern matching breaks.

**Why it matters**: Extension friction. Adding `syntax-box` or `syntax-record` special forms requires touching two separate functions in the adapter layer. Asymmetric updates cause silent failures—patterns match but templates don't construct, or vice versa.

**Suggested fix**: Add a dispatch table mapping `syntax.SyntaxValue` types to constructor/destructor pairs. New syntax types register in one place. Alternatively, add `ToPatternValue() values.Value` and `FromPatternValue(v values.Value, scopes) SyntaxValue` methods to the `SyntaxValue` interface and dispatch via interface calls.

**Effort**: M (requires refactoring the adapter layer, ~3-4 days with full testing)

---

### [Priority: Medium] — Operation Code Generation: Hand-Written Boilerplate for 36+ Operation Types

**Where**: `machine/operation_*.go` (~36 files, ~30-80 lines each)

**What**: Each VM operation (`OperationPush`, `OperationPop`, `OperationLoadLiteral`, etc.) has a dedicated file with hand-written `Apply`, `String`, `EqualTo`, and `MarshalJSON` methods. The boilerplate is 90% identical across operations—only the core `Apply` logic differs. Adding a new operation requires creating a new file and hand-writing 4 methods.

**Why it matters**: Extension friction. Adding a new bytecode instruction (e.g., `OperationTailCallOptimization`, `OperationInlineCache`) requires ~60 lines of boilerplate. The `EqualTo` and `MarshalJSON` methods use reflection helpers (`sameType`, `fieldMatches`) that could be generated. See also `plans/ARCHITECTURAL_REVIEW_REFACTORING.md` §2.2.

**Suggested fix**: Do NOT pursue code generation (too risky per existing plan). Instead, extract the boilerplate into an `OperationBase` struct with default `String`/`EqualTo`/`MarshalJSON` implementations. Each operation embeds `OperationBase` and only overrides `Apply`. This reduces new operation code from 60 lines to ~15.

**Effort**: M (requires refactoring all ~36 operation files, ~1 week, but lower risk than codegen)

---

### [Priority: Medium] — Tokenizer Reader Methods: Three Nearly-Identical Functions for Different Token Types — RESOLVED

**Where**: `internal/tokenizer/tokenizer.go`

**Resolution**: Addressed in multiple PRs:
- PR #230 (`refactor/tokenizer-reader-consolidation`): Extracted `readDelimited` to unify string and extended symbol scanning (~60 lines saved).
- `refactor/tokenizer-predicate-cleanup`: Consolidated duplicate predicates (`isSymbolInitial`/`isIdentifierInitial` → `isInitial`), fixed `for`→`if` bug (~17 lines saved).
- `refactor/tokenizer-signed-state-helper`: Extracted `signedState` helper for signed/unsigned state dispatch (~12 lines saved).
- `refactor/tokenizer-number-parsing-consolidation`: Deleted `scanForImaginaryNumberSpecials`, extracted `readOptionalDecimalPart` (~85 lines saved).

**Total savings**: ~174 lines. See `plans/TOKENIZER_CONSOLIDATION_PLAN.md` for remaining (deferred) opportunities.

**Effort**: Completed

---

## LOW PRIORITY

### [Priority: Low] — Numeric Type Switch Incompleteness: Interfaces vs Concrete Types (Deferred)

**Where**: Multiple files with type switches on numbers (e.g., `registry/core/prim_arithmetic.go`, `values/numeric_tower.go`)

**What**: Some type switches use `case values.ComplexNumber:` (interface, matches both `*Complex` and `*BigComplex`) while others use `case *values.Complex:` / `case *values.BigComplex:` (concrete types, must list both). The recent H6 fix (`real?` predicate) showed how using concrete types missed `*Complex`. The inconsistency is a latent bug pattern.

**Why it matters**: When adding a new numeric type (e.g., `*Decimal` for R7RS-large), every type switch that uses concrete types must be updated. Type switches using interfaces automatically handle the new type if it implements the interface.

**Suggested fix**: Lint rule: type switches on numeric values must use interface types (`values.Number`, `values.RealNumber`, `values.ComplexNumber`) unless concrete type dispatch is semantically required. Fix existing switches to use interfaces where possible.

**Effort**: S (1-2 days to audit and fix)

---

### [Priority: Low] — 507 TODO/FIXME Comments Across 74 Files

**Where**: Concentrated in tests (`internal/parser/parser_coverage_test.go`: 104 TODOs, `internal/validate/validate_test.go`: 29 TODOs, `internal/bootstrap/multithreading_test.go`: 30 TODOs)

**What**: 507 TODO comments, mostly in test files marking missing coverage or known edge cases. Examples: `"TODO: test overflow"`, `"FIXME: test circular lists"`, `"XXX: this should fail but doesn't"`. The TODOs are not tracked in any issue tracker.

**Why it matters**: Technical debt visibility. TODOs in test files indicate known gaps in coverage. TODOs in production code indicate incomplete implementations. Without centralized tracking, TODOs accumulate and are forgotten.

**Suggested fix**: Create GitHub issues for all production code TODOs (non-test files). Test TODOs can stay inline but should be audited to verify they're not hiding real bugs.

**Effort**: S (2-3 days to triage and create issues)

---

### [Priority: Low] — VM Operation Equality: Reflection-Based Comparison Instead of Code Generation

**Where**: `machine/operation_helpers.go:10-40` (`sameType`, `fieldMatches`)

**What**: All operation `EqualTo` methods use reflection helpers to compare struct fields generically. This is slower than direct field comparison and harder to debug (reflection panics are opaque). The helpers were added to avoid hand-writing equality checks for ~32 operation types, but the tradeoff is performance and debuggability.

**Why it matters**: Operation equality is used in test assertions and potentially in optimization passes (detecting redundant operations). The reflection overhead is small but unnecessary—operations are compared frequently during compilation.

**Suggested fix**: Either accept the reflection cost (it's negligible for compilation workloads) or generate `EqualTo` methods via `go generate` using AST inspection. Do NOT hand-write ~32 equality methods.

**Effort**: S (if accepting status quo) / M (if pursuing code generation)

---

## Top Priority (Recommended Action)

1. **I/O extension test coverage** — I/O is a system boundary where bugs leak into production. Recent M10/M11 fixes show this area is actively problematic.

---

## Codebase Metrics

| Metric | Value |
|--------|-------|
| Total Go source files | ~615 |
| Total test files | ~302 |
| struct types | 309 |
| Foreign functions (primitives) | 505 |
| Error wrapping sites | ~772 |
| Bare errors (fmt.Errorf/errors.New) | ~2 (panic-recovery wrappers only) |
| context.TODO() uses | 4 production, ~404 tests |
| TODO/FIXME comments | 507 |
| Overall test coverage | 85.3% (root), 85-93% (core packages) |

| Package | Coverage | Notes |
|---------|----------|-------|
| `registry/helpers` | 86.7% | Fixed (commit `c20df47`) |
| `cmd/scheme` | 9.9% | User-facing CLI |
| `internal/extensions/io` | 55.2% | Bug-prone I/O boundary |
| `internal/extensions/math` | 68.0% | Complex arithmetic |
| `internal/extensions/eval` | 73.4% | Meta-evaluation |
| `values` | ~85% | Core numeric tower |
| `machine` | 85.0% | VM and compiler |
| `environment` | 90.4% | Symbol resolution |
| `registry/core` | 88.5% | Standard library |
| `internal/extensions/gointerop` | 96.4% | Go FFI |
| `internal/extensions/system` | 100.0% | System interface |

