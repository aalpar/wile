# Tech Debt Review — Wile Codebase

Date: 2026-02-27
Verified: 2026-02-28 (13/14 claims verified accurate; F13 retracted — no duplication exists)
Updated: 2026-03-01 (completion status verified against sources; F6/F7 resolved — coverage was never low)
Scope: Full codebase (662 Go files, 177k lines)
Method: Staff-engineer assessment — structural debt, consistency debt, testing debt, evolution risk
Prior reviews: Structural (complete, remediated), Signals (complete, remediated)

## Executive Summary

The codebase is architecturally sound — bytecode VM, correct continuation chain, proper hygiene model, clean package layering. The debt is organizational, not foundational. Of the 14 original findings: 10 are complete or resolved (F1, F2, F3, F4, F5, F6, F7, F8, F9, F14), 1 retracted (F13), 2 deferred by design (F10, F11), leaving 1 actionable item: F12 (subsystem benchmarks).

---

## Findings

### F1 [Priority: High] [COMPLETE] — Numeric dispatch tables: 41 hand-unrolled arrays

**Where**: `values/{integer,big_integer,float,big_float,complex,big_complex,rational}.go`
**What**: Each of 7 numeric types declares `[numKinds]func` dispatch arrays for Add, Subtract, Multiply, Divide, LessThan, Compare — totaling 41 arrays (BigComplex has 5, all others have 6). Each array is populated in `init()` with 7–9 entries of structurally identical code: extract operand, promote, compute, wrap result.

Example — adding two types to BigFloat:
```go
// integer.go init()
integerAdd[KindBigFloat] = func(p *Integer, o Number) Number {
    return &BigFloat{value: new(big.Float).Add(p.bigFloat(), o.(*BigFloat).value)}
}

// big_integer.go init()
bigIntegerAdd[KindBigFloat] = func(p *BigInteger, o Number) Number {
    self := p.bigFloat()
    return &BigFloat{value: new(big.Float).Add(self, o.(*BigFloat).value)}
}
```

Same shape, same semantics, different only in the receiver's conversion method name.

**Why it matters**: Adding a new numeric type (e.g., `Decimal`) requires adding dispatch entries to all 41 arrays across 7 files — each written by hand. The pattern is a loop over `(srcType, dstType, operation)` triples waiting to be expressed as data. Every entry is also an independent bug surface (see: exactness contagion inconsistency in `Complex.Multiply()` vs `BigComplex.Multiply()` which handle zero differently).

**Suggested fix**: Extract a promotion lattice as data — `promotionTable[srcKind][dstKind] → promoter func` — and generate dispatch entries from it. The arithmetic operations themselves are uniform: promote both operands to the LUB type, apply the operation, simplify.

**Effort**: L (touches 7 files, needs careful correctness verification)

**Status**: COMPLETE — All 3 phases implemented. `values/promotion.go` contains the promotion lattice as data (`promotionTable`, `promoter` arrays) and generic dispatch generators (`makeArithmeticDispatch[T]`, `makeLessThanDispatch[T]`, `makeCompareDispatch[T]`). All 41 hand-written dispatch arrays across 7 type files replaced with generated dispatch. Tests in `values/promotion_test.go`.

**Remediation Instructions** (retained for reference):

This is the largest structural debt item. Do NOT attempt a full rewrite in one pass. Break into phases:

**Phase 1: Extract the promotion lattice as data (prerequisite for everything else)**

**CRITICAL DESIGN CONSTRAINT — NO LOSSY PROMOTIONS:**

The numeric tower is a LATTICE, not a linear chain. The previous TowerAdd attempt failed
because it used a linear rank (Integer → BigInteger → Rational → Float → BigFloat →
Complex → BigComplex), which forced exact types through float64 on the way to Complex,
destroying exactness.

The promotion table encodes a SYMMETRIC, no-loss LUB for every (srcKind, dstKind) pair.
"No loss" means: no intermediate conversion discards information. Specifically:
- Exact types (Integer, BigInteger, Rational) NEVER promote to float64 or complex128.
  They go to BigFloat (when meeting inexact reals) or BigComplex (when meeting complex).
- BigFloat NEVER promotes to complex128 (would lose precision). Goes to BigComplex.
- Float → complex128 is safe (just adds 0i).

**No-Loss Result Type Matrix (symmetric):**

```
             │ Int    BigInt  Rat     Float    BigFloat  Complex    BigComplex
─────────────┼──────────────────────────────────────────────────────────────────
Integer      │ Int¹   BigInt  Rat     BigFloat BigFloat  BigComplex BigComplex
BigInteger   │ BigInt BigInt  Rat     BigFloat BigFloat  BigComplex BigComplex
Rational     │ Rat    Rat     Rat     BigFloat BigFloat  BigComplex BigComplex
Float        │ BigFloat BigFloat BigFloat Float BigFloat Complex    BigComplex
BigFloat     │ BigFloat BigFloat BigFloat BigFloat BigFloat BigComplex BigComplex
Complex      │ BigComplex BigComplex BigComplex Complex BigComplex Complex BigComplex
BigComplex   │ BigComplex BigComplex BigComplex BigComplex BigComplex BigComplex BigComplex
```

¹ Integer + Integer may overflow to BigInteger via addInt64/subInt64/mulInt64.

**Three zones:**
1. Exact×Exact (top-left 3×3): stays exact — Integer, BigInteger, Rational
2. Exact×InexactReal: always BigFloat — exact value gets 256-bit precision
3. Anything×Complex: exact→BigComplex; Float+Complex stays Complex; BigFloat+Complex→BigComplex

**Lossless promotion paths (all edges):**
```
Exact chain:    Integer → BigInteger → Rational
                    ↓          ↓           ↓
Inexact real:   BigFloat ← BigFloat ← BigFloat
                    ↑
Float native:   Float → BigFloat
                  ↓
Complex:        Complex → BigComplex ← BigFloat
                              ↑
                    Integer/BigInt/Rational (exact parts)
```

**IEEE 754 fast-path optimization (Phase 2/3, NOT in the promotion table):**

The promotion table says Integer + Float → BigFloat (correct, no-loss). But at runtime,
if |int64 value| ≤ 2^53 (IEEE 754 double mantissa: 52 explicit + 1 implicit = 53 bits),
float64 arithmetic is exact. The generated dispatch can shortcut:
- Integer + Float: if |p.Value| ≤ 1<<53, use float64 (fast). Else BigFloat.
- Integer + Complex: if |p.Value| ≤ 1<<53, use complex128 (fast). Else BigComplex.
BigInteger always takes the BigFloat/BigComplex path (exists because value exceeded int64).

**Implementation steps:**

1. Read `docs/dev/NUMERIC_TOWER.md` for the existing lattice documentation.
2. Create `values/promotion.go` with:
   - A `promotionTable [numKinds][numKinds]NumericKind` array that maps `(kindA, kindB) → resultKind` (the LUB type). Symmetric: `promotionTable[a][b] == promotionTable[b][a]`.
   - A `promoter [numKinds][numKinds]func(Number) Number` array that promotes a value of `srcKind` to `resultKind`. Each type already has conversion methods (e.g., `(*Integer).bigFloat()`, `(*BigInteger).bigFloat()`). The promoter table codifies these into a single lookup. Only lossless conversions are used.
3. Populate both tables in `init()`. Source the conversion methods from each type.
4. Write tests in `values/promotion_test.go`: verify every `(srcKind, dstKind)` pair returns the expected result type. Test symmetry: `promote(a, b)` and `promote(b, a)` should yield the same result type.

**Phase 2: Generate Add dispatch from the promotion table (proof of concept)**

1. Create a generic `makeAddDispatch` function that, given a type's Kind and its promotion functions, generates all `[numKinds]func` entries for Add. The pattern is: promote both operands to LUB, call the LUB type's Add method, return.
2. Replace `integerAdd` array population in `integer.go init()` with a call to `makeAddDispatch`.
3. Benchmark result in numeric benchmarks.  No more than +2% performance regression over current `master` branch - performance can degrade by NO MORE than 2%!
4. Run `make test` to verify correctness.
5. If successful, repeat for all 7 types' Add arrays.

**Phase 3: Generalize to all operations**

Once Phase 2 proves the pattern, extend to Subtract, Multiply, Divide, LessThan, Compare. Each operation has the same structure: promote both to LUB, apply operation, return. The differences are only in which method to call on the promoted type.

**Verification**: After each phase, run `make test && make lint`. The numeric tower has extensive test coverage — any regression will be caught.

**Key files to read first**:
- `values/integer.go` init() (lines ~170-230) — canonical dispatch entry pattern
- `values/big_complex.go` init() — the type with 5 arrays (no LessThan)
- `values/values.go` — `Number` interface, `Kind` enum, `numKinds` constant
- `docs/dev/NUMERIC_TOWER.md` — lattice documentation

---

### F2 [Priority: High] [PARTIAL] — Extension friction: adding a callable type = 3+ files, adding an opcode = 7+ files

**Where**: `machine/machine_context.go` (ApplyCallable, Run), `machine/expander_time_continuation.go` (transformer dispatch), `machine/native_template.go` (operationToInstruction/instructionToOperation), `machine/compile_validated.go`, `machine/opcode.go`

**What**: The callable type dispatch appears in at least 3 locations:
1. `machine_context.go` ApplyCallable switch
2. `expander_time_continuation.go` transformer invocation (~line 1337)
3. `expander_time_continuation.go` second transformer dispatch (~line 1447)

Adding a new opcode requires:
1. `opcode.go`: Add constant + name
2. `native_template.go`: Two switch statements (operationToInstruction, instructionToOperation)
3. `machine_context.go` Run(): Add dispatch case
4. New `operation_*.go` file
5. Compiler method to emit it
6. Tests

No registry, no visitor, no dispatch table — all hand-wired switches.

**Why it matters**: The file-touch count is the tax on every new feature. It's not just effort — it's the probability of forgetting a site. The `REVIEW.local.md` already documents this exact failure mode (PR #147: "When a validation switch intentionally excludes a callable type...").

**Suggested fix**: For callables: introduce a `CallableHandler` interface registered once, dispatched everywhere. For opcodes: this is harder (performance-sensitive hot loop), but the operation↔instruction mapping could be code-generated from a single definition table.

**Effort**: M (callables) / L (opcodes, due to performance constraints)

**Status**: Part A COMPLETE — opcode addition checklist added to `machine/opcode.go:22`. Part B COMPLETE — `invokeTransformerClosure()` helper in `expander_time_continuation.go` unifies both expander dispatch sites.

**Remediation Instructions**:

**Part A: Document the opcode addition checklist (do first, trivial)** [COMPLETE]

1. Create a comment block at the top of `machine/opcode.go` (after the package doc) documenting the exact files that must be touched when adding a new opcode:
   ```
   // Adding a new opcode requires changes in:
   // 1. opcode.go — add OpXxx constant and entry in opcodeNames
   // 2. machine_context.go Run() — add dispatch case in the main switch
   // 3. native_template.go — add cases in both operationToInstruction() and instructionToOperation()
   // 4. operation_xxx.go — create new operation type (or add to existing file)
   // 5. compile_*.go — add compiler method to emit the new opcode
   // 6. Relevant _test.go files
   // 7. peephole.go — if the new op participates in fusion/chaining
   ```

**Part B: Unify callable dispatch (Medium effort)**

The callable dispatch is duplicated in 3 locations:
- `machine_context.go:572` — `ApplyCallable()` with 5 cases: MachineClosure, ForeignClosure, CaseLambdaClosure, Parameter, ComposableContinuation
- `expander_time_continuation.go:1336` — `invokeMacroTransformer()` with 2 cases: MachineClosure, ForeignClosure
- `expander_time_continuation.go:1446` — `IsSyntaxBindingWithScopes()` with 2 cases: MachineClosure, ForeignClosure

The two expander dispatches only handle MachineClosure and ForeignClosure because those are the only types used as macro transformers. The fix:

1. Read `machine/machine_context.go` — find `ApplyCallable()` (around line 572). Study the 5-case switch.
2. Read `machine/expander_time_continuation.go` — find both transformer dispatch switches at lines ~1336 and ~1446. Note: these only dispatch on closure types (not Parameter or ComposableContinuation), which is correct — macro transformers are always closures.
3. The transformer dispatch sites could use a shared `invokeClosureAsTransformer(cls, args)` helper that handles the MachineClosure/ForeignClosure split. Create this in `machine/operation_helpers.go` or a new `machine/callable_dispatch.go`.
4. Replace both dispatch sites in `expander_time_continuation.go` with calls to the shared helper.
5. `ApplyCallable()` has 5 cases — leave it alone for now (it handles more types and is the hot path).
6. Run `make test && make lint`.

**Key files to read**:
- `machine/machine_context.go:572-592` — ApplyCallable
- `machine/expander_time_continuation.go:1336-1362` — invokeMacroTransformer
- `machine/expander_time_continuation.go:1446-1470` — IsSyntaxBindingWithScopes

---

### F3 [Priority: High] [COMPLETE] — Complex and BigComplex missing HashCode

**Where**: `values/complex.go`, `values/big_complex.go`
**What**: Neither type implements `Hashable` (confirmed: 0 `HashCode` occurrences in both files). All other numeric types that can serve as hashtable keys implement it: Integer, BigInteger, Float, BigFloat, Rational. The `values/CLAUDE.local.md` explicitly lists the Hashable implementors and Complex/BigComplex are absent.

**Why it matters**: R7RS §6.10 allows any value satisfying `equal?` to be a hashtable key. A complex number used as a hashtable key will fail at runtime. This is a correctness gap, not just cosmetic debt — it violates the contract that `if a.EqualTo(b) then a.HashCode() == b.HashCode()` by making the predicate undefined.

**Suggested fix**: Add `HashCode()` to both types. For Complex: hash real and imaginary parts as float64 pair. For BigComplex: delegate to parts' HashCode methods.

**Effort**: S

**Status**: COMPLETE — `Complex.HashCode()` added at `values/complex.go:321`, `BigComplex.HashCode()` added at `values/big_complex.go:637`.

**Remediation Instructions** (retained for reference):

1. Read `values/values.go:125-128` for the `Hashable` interface definition (`HashCode() uint64`).
2. Read existing implementations for the pattern:
   - `values/integer.go:70-72` — uses `hashExactNumeric(new(big.Rat).SetInt64(p.Value))`
   - `values/float.go:46-51` — uses `hashInexactNumeric(p.bigFloat())`
   - `values/big_float.go:71-73` — uses `hashInexactNumeric(p.value)`
3. Find the helper functions `hashExactNumeric` and `hashInexactNumeric` — they're in `values/` (search for `func hashExactNumeric` and `func hashInexactNumeric`). Understand what they do.

**Add HashCode to Complex** (`values/complex.go`):
```go
func (p *Complex) HashCode() uint64 {
	return hashInexactNumeric(p.bigComplexFloat())
}
```
Complex numbers are inexact. The simplest correct approach: promote to BigComplex (or its *big.Float components) and hash as the pair. But check how `BigFloat.HashCode()` works — you may need to hash real and imaginary parts separately and combine them. The key invariant: if `a.EqualTo(b)` then `a.HashCode() == b.HashCode()`. Check `Complex.EqualTo()` to understand equality semantics.

A safer approach: hash the real and imaginary parts independently and combine:
```go
func (p *Complex) HashCode() uint64 {
	r := hashInexactNumeric(new(big.Float).SetFloat64(real(p.value)))
	i := hashInexactNumeric(new(big.Float).SetFloat64(imag(p.value)))
	return r ^ (i * 0x9e3779b97f4a7c15)  // golden ratio hash combine
}
```

**Add HashCode to BigComplex** (`values/big_complex.go`):
```go
func (p *BigComplex) HashCode() uint64 {
	r := hashInexactNumeric(p.value.Real)
	i := hashInexactNumeric(p.value.Imag)
	return r ^ (i * 0x9e3779b97f4a7c15)
}
```
Read the BigComplex struct to find the field names for real/imaginary parts (likely `p.value.Real` and `p.value.Imag` based on the big.Complex type, but verify).

**After implementation**:
4. Add the interface compliance assertions at the top of each file:
   ```go
   var _ Hashable = (*Complex)(nil)
   var _ Hashable = (*BigComplex)(nil)
   ```
5. Update `values/CLAUDE.local.md` to add Complex and BigComplex to the Hashable implementor list.
6. Write tests in `values/complex_test.go` and `values/big_complex_test.go`:
   - Equal values produce equal hashes: `Complex(1+2i).HashCode() == Complex(1+2i).HashCode()`
   - Different values likely produce different hashes (probabilistic, not guaranteed)
   - Complex and BigComplex with the same numeric value produce the same hash (cross-type consistency)
7. Run `make test && make lint`.

---

### F4 [Priority: Medium] [COMPLETE] — Test helper duplication: registry/core vs registry/testhelpers

**Where**: `registry/core/test_helpers_test.go` (212 lines), `registry/testhelpers/helpers.go` (131 lines)
**What**: The core package maintains a local copy of the test helper API (`runSchemeCode`, `runSchemeCodeExpectTrue/False/Error`, `schemeCodeTestCase`) that duplicates the exported `registry/testhelpers` package. The local version adds `runSchemeCodeWithEnv` and `runSchemeCodeWithEnvAndContext` which don't exist in the exported package. 130+ test files in `registry/core/` import the local version.

**Why it matters**: Changes to test infrastructure must be made in two places. The exported package exists but is bypassed by the largest consumer. New contributors will find two APIs and won't know which to use.

**Suggested fix**: Promote `WithEnv` and `WithContext` variants to `registry/testhelpers`, then delete `registry/core/test_helpers_test.go` and update imports.

**Effort**: S

**TODO — Remediation Instructions**:

The goal: merge the two test helper packages into one. The local `registry/core/test_helpers_test.go` has 10 functions; the exported `registry/testhelpers/helpers.go` has 8 exported functions. 6 overlap; 4 are unique to the local version.

**Step 1: Add missing functions to `registry/testhelpers/helpers.go`**

Read `registry/core/test_helpers_test.go` and add these 4 functions (exported) to `registry/testhelpers/helpers.go`:

- `RunSchemeCodeWithEnv(t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error)` — currently `runSchemeCodeWithEnv` at line ~74
- `RunSchemeCodeWithEnvAndContext(ctx context.Context, t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error)` — currently `runSchemeCodeWithEnvAndContext` at line ~185
- `RunProgramAST(t *testing.T, prog values.Value) (values.Value, error)` — currently `runProgramAST` at line ~34
- `RunProgramASTWithEnv(t *testing.T, env *environment.EnvironmentFrame, prog values.Value) (values.Value, error)` — currently `runProgramASTWithEnv` at line ~44

Copy the implementations from the local file, adjusting to exported names. Check that `registry/testhelpers/helpers.go` already imports the necessary packages (it probably imports `wile` and `values`; you may need to add `environment` and `context`).

**Step 2: Update all 112 test files in registry/core/**

The test files currently call `runSchemeCode(t, ...)` (lowercase, local). They need to call `testhelpers.RunSchemeCode(t, ...)` (exported, imported).

For each test file in `registry/core/`:
1. Add import: `"github.com/aalpar/wile/registry/testhelpers"`
2. Replace all lowercase calls with `testhelpers.Xxx` calls:
   - `runSchemeCode(` → `testhelpers.RunSchemeCode(`
   - `runSchemeCodeExpectTrue(` → `testhelpers.RunSchemeCodeExpectTrue(`
   - `runSchemeCodeExpectFalse(` → `testhelpers.RunSchemeCodeExpectFalse(`
   - `runSchemeCodeExpectError(` → `testhelpers.RunSchemeCodeExpectError(`
   - `runSchemeCodeWithEnv(` → `testhelpers.RunSchemeCodeWithEnv(`
   - `runSchemeCodeWithEnvAndContext(` → `testhelpers.RunSchemeCodeWithEnvAndContext(`
   - `runSchemeCodeWithTimeout(` → `testhelpers.RunSchemeCodeWithTimeout(`
   - `runSchemeCodeWithContext(` → `testhelpers.RunSchemeCodeWithContext(`
   - `runProgramAST(` → `testhelpers.RunProgramAST(`
   - `runProgramASTWithEnv(` → `testhelpers.RunProgramASTWithEnv(`
   - `schemeCodeTestCase` → `testhelpers.SchemeCodeTestCase`
   - `schemeCodeErrorTestCase` → `testhelpers.SchemeCodeErrorTestCase` (if this struct exists)

Note: 3 files already import `testhelpers` — `core_test.go`, `prim_auxiliary_syntax_test.go`, `prim_byte_vector_utf8_test.go`. These may need import deduplication.

This is a mechanical sed/replace operation. Use `grep -rl 'runSchemeCode\|runProgramAST\|schemeCodeTestCase' registry/core/*_test.go` to find all affected files.

**Step 3: Delete `registry/core/test_helpers_test.go`**

After all imports are updated and tests pass, delete the local file.

**Step 4: Verify**

Run `make test && make lint`. All 115 test files in registry/core/ should pass using the exported helpers.

---

### F5 [Priority: Medium] [COMPLETE] — Duplicate test-scheme invocation in Makefile

**Where**: `Makefile:133-134`
**What**: The `test` target calls `test-scheme` twice:
```makefile
@$(MAKE) test-scheme
@$(MAKE) test-scheme
```

**Why it matters**: Every `make test` run executes the Scheme test suite twice, wasting CI time. Minor, but the kind of thing that erodes trust in the build system.

**Suggested fix**: Delete line 134.

**Effort**: Trivial

**Status**: COMPLETE — duplicate `test-scheme` line removed. Only one invocation remains at `Makefile:133`.

**Remediation Instructions** (retained for reference):

1. Open `Makefile`, find lines 133-134 (the `test` target).
2. Delete the second `@$(MAKE) test-scheme` line (line 134).
3. Run `make test` to verify the Scheme tests still run once.

---

### F6 [Priority: Medium] [RESOLVED] — internal/validate at 13% test coverage, not excluded from covercheck

**Where**: `internal/validate/` (15 src files, 2 test files), `tools/sh/covercheck.sh:25-34`
**What**: The validation package — responsible for compile-time form validation before codegen — has approximately 13% test coverage. It is not in the `covercheck.sh` exclusion list, so either: (a) it's passing the 80% threshold somehow (unlikely at 13%), or (b) `covercheck` is computing coverage differently than a naive file ratio. Either way, the validation logic that gates correctness of compiled forms has minimal direct test coverage.

**Why it matters**: Validation is the gatekeeper between expansion and compilation. A bug here means malformed ASTs reach the compiler silently. The package likely gets exercised transitively through integration tests, but that's implicit — no regression tests guard individual validation rules.

**Suggested fix**: Either add targeted tests for validation rules (preferred) or explicitly exclude with a documented rationale.

**Effort**: M

**Status**: RESOLVED — The original 13% figure was based on file count ratio (2 test files / 15 source files), not Go statement coverage. Actual statement coverage measured 2026-03-01: **86.4%** (`go test -cover ./internal/validate/`), well above the covercheck threshold. The package gets substantial coverage transitively through compiler and integration tests. No action needed.

**TODO — Remediation Instructions** (retained for reference):

First, understand the actual coverage situation. The file count ratio (2/15) is misleading — Go coverage is measured by statement, not file count. The package may get substantial coverage transitively through integration tests.

**Step 1: Measure actual coverage**

```bash
cd /Users/aalpar/projects/wile-workspace/wile
go test -coverprofile=cover.out ./internal/validate/
go tool cover -func=cover.out | tail -1
```

This gives the actual statement coverage percentage. If it's already above the `covercheck.sh` threshold (read `tools/sh/covercheck.sh` to find the threshold — likely 80%), then the transitive coverage is sufficient and the file count ratio was misleading.

**Step 2: If coverage is genuinely low, add targeted tests**

The 15 source files in `internal/validate/` are:
- `validate.go` — main `Validate()` entry point
- `validate_if.go`, `validate_lambda.go`, `validate_define.go`, `validate_set.go`, `validate_quote.go`, `validate_begin.go`, `validate_case_lambda.go`, `validate_dynamic_wind.go`, `validate_call.go`, `validate_macro.go` — per-form validators
- `validated_forms.go` — ValidatedExpr types
- `errors.go` — validation error sentinels
- `register.go` — validator registration
- `doc.go` — package doc

For each `validate_*.go` file, add tests in a new `validate_*_test.go` or extend the existing test files:

1. **Happy path**: Valid form produces correct `ValidatedExpr` subtype.
2. **Error paths**: Each validator rejects malformed input with the correct sentinel error:
   - Wrong argument count (e.g., `(if)`, `(if a)`, `(if a b c d)`)
   - Wrong argument types where applicable
   - Missing required subforms
3. **Edge cases**: Empty body for `begin`, single-arm `if`, `define` in expression context vs definition context.

Use table-driven tests following the project convention. For each form, create test cases like:
```go
func TestValidateIf(t *testing.T) {
    tests := []struct {
        name    string
        input   string  // Scheme code
        wantErr error   // nil for valid, sentinel for invalid
    }{
        {"valid 2-arm", "(if #t 1 2)", nil},
        {"valid 1-arm", "(if #t 1)", nil},
        {"too few args", "(if)", ErrTooFewArguments},
        {"too many args", "(if a b c d)", ErrTooManyArguments},
    }
    // ...
}
```

Look at the existing `validate_test.go` and `validated_forms_test.go` for the test setup pattern — they'll show how to parse Scheme code into syntax values and call the validator.

**Step 3: Verify**

Run `make test && make covercheck` to ensure the package meets the coverage threshold.

---

### F7 [Priority: Medium] [RESOLVED] — security/ package at 29% test coverage

**Where**: `security/` (7 src files, 2 test files)
**What**: The authorization framework — the runtime security boundary for embedded use — has sparse direct test coverage. Two test files for seven source files.

**Why it matters**: Security code that isn't tested is security code you can't trust. The two-layer sandboxing model (extension-level + authorizer) is a selling point for embedding. Under-testing here creates risk exactly where confidence matters most.

**Suggested fix**: Add tests for authorization denial paths, resource/action combinations, and edge cases (nil authorizer, nil context).

**Effort**: M

**Status**: RESOLVED — The original 29% figure was based on file count ratio (2 test files / 7 source files), not Go statement coverage. Actual statement coverage measured 2026-03-01: **95.2%** (`go test -cover ./security/`). The security package is well-tested. No action needed.

**TODO — Remediation Instructions** (retained for reference):

Same as F6 — first measure actual coverage before writing tests.

**Step 1: Measure actual coverage**

```bash
cd /Users/aalpar/projects/wile-workspace/wile
go test -coverprofile=cover.out ./security/
go tool cover -func=cover.out | tail -1
```

**Step 2: If low, add tests**

The 7 source files are:
- `access.go` — Resource/Action types and K8s-style vocabulary
- `authorizer.go` — `Authorizer` interface definition
- `composite.go` — `CompositeAuthorizer` (chains multiple authorizers)
- `context.go` — Security context attached to Go context
- `deny_all.go` — `DenyAllAuthorizer` implementation
- `filesystem_root.go` — Filesystem root restriction authorizer
- `read_only.go` — Read-only authorizer implementation

For each authorizer implementation, test:

1. **`deny_all.go`**: All (Resource, Action) combinations return denial. Test with every Resource/Action pair from `access.go`.
2. **`read_only.go`**: Read actions allowed, write/delete/exit/load actions denied. Test boundary: what about `load`? What about unknown actions?
3. **`filesystem_root.go`**: Paths within root allowed, paths outside root denied. Edge cases: relative paths, symlink traversal, root itself, empty path.
4. **`composite.go`**: Chain of [allow-read, deny-all] — read allowed, write denied. Chain of [deny-all, allow-all] — should deny win? Test ordering semantics.
5. **`context.go`**: `security.Check()` with nil context (should not panic), context without authorizer, context with authorizer. This is the main gateway function — test it thoroughly.

The existing test files (`authorizers_test.go`, `security_test.go`) show the testing pattern. Read them first to understand the setup.

**Step 3: Verify**

Run `make test && make covercheck`.

---

### F8 [Priority: Medium] [COMPLETE] — Two compilation dispatch paths with no unifying abstraction

**Where**: `machine/compile_validated.go` (validated forms), `machine/compile_time_continuation.go` (registry-based forms)
**What**: Core forms (if, lambda, define, set!, quote, begin) go through the validated path: `validate.Validate()` → `ValidatedExpr` subtypes → `compileValidated()` switch. Extension forms (define-syntax, import, include, syntax-case) go through the registry path: `CompileSyntaxPrimitive()` → `LookupSyntaxCompiler()`. Two different mechanisms for one concept: "compile a special form."

**Why it matters**: A new contributor adding a form must choose the right path with no guidance beyond convention. The two paths have different error handling, different tracing, and different extension points. This is consistency debt that grows with every new form.

**Suggested fix**: Document the decision criteria explicitly. Long-term: consider whether all forms could use the registry path, with validated forms being pre-registered entries that happen to validate first.

**Effort**: S (document) / L (unify)

**Status**: COMPLETE (document phase) — doc comments added at `compile_validated.go:25-35` and cross-reference at `compile_time_continuation.go:178-179`. Unification deferred.

**Remediation Instructions (document only — S effort)** (retained for reference):

The two paths are:
- **Validated path** (`machine/compile_validated.go:60-114`): Core forms — `if`, `lambda`, `define`, `quote`, `quasiquote`, `begin`, `case-lambda`, `dynamic-wind`, `set!`. Uses `validate.Validate()` → `ValidatedExpr` subtypes → `compileValidated()` two-tier dispatch (form-name registry lookup at lines 74-82, then type-based at lines 90-106).
- **Registry path** (`machine/compile_time_continuation.go:177-198`): Extension forms — `syntax-case`, `import`, `define-syntax`, `define-library`, `include`, `quasisyntax`, `with-syntax`, `cond-expand`. Uses `CompileSyntaxPrimitive()` → `LookupSyntaxCompiler()` → registered `SyntaxCompiler` bindings.

Add a doc comment at the top of `machine/compile_validated.go` explaining:
```go
// compile_validated.go handles compilation of core Scheme forms that go through
// upfront validation (validate.Validate) before codegen. These are the fixed
// R7RS core forms: if, lambda, define, set!, quote, quasiquote, begin,
// case-lambda, dynamic-wind.
//
// Extension forms (define-syntax, import, include, syntax-case, etc.) use a
// separate registry-based compilation path in compile_time_continuation.go via
// CompileSyntaxPrimitive() → LookupSyntaxCompiler().
//
// Decision criteria: if a form has fixed syntax that can be validated once before
// compilation, it goes through the validated path. If a form's syntax is
// extensible or defined by the extension system itself, it uses the registry path.
```

Also add a cross-reference comment at the top of `machine/compile_time_continuation.go` near `CompileSyntaxPrimitive()`:
```go
// CompileSyntaxPrimitive compiles extension forms via the syntax compiler registry.
// Core forms (if, lambda, define, etc.) use a separate validated compilation path —
// see compile_validated.go.
```

---

### F9 [Priority: Medium] [COMPLETE] — BigComplex missing 1 dispatch table (5 vs 6 for all other types)

**Where**: `values/big_complex.go`
**What**: BigComplex has 5 `[numKinds]` dispatch arrays while all other numeric types have 6. The missing table is likely `LessThan` (complex ordering is undefined in R7RS, so this may be intentional).

**Why it matters**: If intentional, it should be documented. If accidental, it's a latent panic when `BigComplex.LessThan()` is called with certain operand types.

**Suggested fix**: Verify which table is missing. If `LessThan`, add a comment explaining the R7RS rationale. If something else, add it.

**Effort**: Trivial

**Status**: COMPLETE — comment documenting intentional omission added at `values/big_complex.go:261-264`.

**Remediation Instructions** (retained for reference):

Verified: BigComplex has 5 dispatch arrays. The missing one is `bigComplexLessThan`. The `LessThan` method (line 584-586) delegates to `Compare()`:
```go
func (p *BigComplex) LessThan(o Number) bool {
    return p.Compare(o) < 0
}
```

This is intentional — complex numbers have no natural ordering in R7RS. Complex.go also has `complexLessThan` (6 arrays total) but BigComplex omits the table.

**Action**: Add a comment in `values/big_complex.go` near the dispatch array declarations (around line 260-264):
```go
// BigComplex has 5 dispatch tables (no bigComplexLessThan).
// Complex ordering is undefined in R7RS §6.2.6 — LessThan delegates to Compare.
// Compare itself uses magnitude comparison (|a| vs |b|) as a total order for
// internal use (sorting, etc.) but this is NOT the mathematical ordering.
```

Also check: does Complex.go's `complexLessThan` table do the same delegation? If so, note consistency. If it has a real table but BigComplex doesn't, that's an inconsistency worth documenting.

Run `make lint` after adding the comment.

---

### F10 [Priority: Medium] — MachineContext: 1586 lines, 71 methods, 10+ responsibilities

**Where**: `machine/machine_context.go`
**What**: MachineContext owns: VM execution loop, continuation management, dynamic-wind stack, call dispatch, sub-context creation, exception handling, debugger integration, stack traces, context cancellation, thread identity. Every new VM feature adds methods here. (Updated 2026-03-01: now 1586 lines, 71 functions — slight growth from 1562/66 at original review.)

**Why it matters**: The blast radius of any change is the entire file. Testing individual subsystems (e.g., continuation management alone) requires constructing the full MachineContext. This is the single most common file touched in PRs.

**Suggested fix**: Extract continuation chain management and dynamic-wind into separate types that MachineContext delegates to. Start with `WindingStack` — it has clear boundaries (unwind/rewind/restore) and is already a distinct concept.

**Effort**: L

**TODO — Remediation Instructions**:

This is a long-term refactoring that should NOT be done incrementally. The payoff is too low until the embedding API has consumers. Document the intent and defer.

If/when this is prioritized, the approach would be:

1. **Extract WindingStack**: The winding/dynamic-wind methods form a cohesive group:
   - `PushWindingFrame()`, `PopWindingFrame()`, `UnwindTo()`, `RewindTo()`, `unwindStackTo()`, `WindingStack()`, `SetWindingStack()`
   - Create `machine/winding_stack.go` with a `WindingStack` type.
   - MachineContext gets a `windingStack WindingStack` field and delegates.

2. **Extract ContinuationChain**: Continuation management methods:
   - `SaveContinuation()`, `PopContinuation()`, `Restore()`, `RestoreAndRelease()`, `RestoreWithWinding()`, `RestoreWithWindingFrom()`, `CurrentContinuation()`, `SliceContinuationAt()`, `FindPrompt()`
   - This is harder because continuations interact with winding, exceptions, and the VM loop. Extract only if WindingStack extraction succeeds cleanly.

3. **Extract ExceptionHandler**: Small group:
   - `PushExceptionHandler()`, `PopExceptionHandler()`, `ExceptionHandler()`, `SetExceptionHandler()`

The key constraint: MachineContext is the hot path. Any extraction that adds indirection in `Run()` must not degrade benchmark performance. Test with `wile_bench_test.go` before and after.

---

### F11 [Priority: Low] — extensions/io and extensions/eval locked in internal/

**Where**: `internal/extensions/io/`, `internal/extensions/eval/`, `internal/extensions/all/`
**What**: I/O and eval extensions live in `internal/`, making them invisible to embedders who import the public API. The `all` convenience aggregator is also internal.

**Why it matters**: Embedders who want I/O or eval must use the engine's built-in configuration rather than composing extensions themselves. This limits the embedding API's composability — the stated product vision.

**Suggested fix**: Promote to `extensions/{io,eval}/` when the extension API stabilizes. Low priority because no external consumers exist yet.

**Effort**: M

**TODO — Remediation Instructions**:

Defer until the extension API stabilizes and external consumers exist. No action needed now.

When ready, the migration steps:
1. Copy `internal/extensions/io/` → `extensions/io/`. Update package name and imports.
2. Copy `internal/extensions/eval/` → `extensions/eval/`. Update package name and imports.
3. Update `internal/extensions/all/register.go` to import from the new public locations.
4. Update `engine.go` (or wherever `internal/extensions/io` and `internal/extensions/eval` are imported) to use the public paths.
5. Note: `extensions/files/` test files import `internal/extensions/io` as `extio` (found in `prim_files_test.go:17` and `with_file_continuation_test.go:17`). Update these imports.
6. Delete the `internal/extensions/io/` and `internal/extensions/eval/` directories.
7. Run `make test && make lint`.

---

### F12 [Priority: Low] — No benchmarks for machine/ (VM execution), registry/core/ (primitives), or internal/tokenizer/

**Where**: Benchmark files exist only in `values/`, `internal/parser/`, `environment/`, and root `wile_bench_test.go`
**What**: The bytecode VM hot loop, primitive dispatch, and tokenizer — the three performance-critical subsystems — have no dedicated benchmarks. High-level benchmarks (Eval, ZebraPuzzle, Gabriel suite) exist but can't isolate regressions to specific subsystems.

**Why it matters**: Performance regressions in the VM loop or tokenizer will show up only in end-to-end benchmarks, making root-cause analysis slow. The Gabriel suite catches macro-level regressions but not micro-level ones.

**Suggested fix**: Add `BenchmarkVMDispatch` (tight loop of arithmetic ops), `BenchmarkTokenize` (tokenize a representative program), and `BenchmarkPrimitiveCall` (call a primitive N times).

**Effort**: S

**TODO — Remediation Instructions**:

Create three benchmark files:

**1. `machine/vm_bench_test.go` — VM dispatch loop benchmark**

```go
func BenchmarkVMDispatch(b *testing.B) {
    // Compile a tight arithmetic loop: (let loop ((i 0)) (if (< i N) (loop (+ i 1)) i))
    // This exercises OpPush, OpLoadLocal, OpApply, OpBranch in a tight loop.
    // Use engine.Compile() + engine.Run() to isolate VM execution from parsing.
}
```

Read `wile_bench_test.go` at the project root for the pattern — it uses `wile.NewEngine()` and `engine.Eval()`. The VM benchmark should use `engine.Compile()` + `engine.Run()` to exclude parse/expand time.

**2. `internal/tokenizer/tokenizer_bench_test.go` — Tokenizer throughput benchmark**

```go
func BenchmarkTokenize(b *testing.B) {
    // Tokenize a representative Scheme program (e.g., from scm/ or test/ directory).
    // Read a .scm file into a string, then tokenize it N times.
    // Report bytes/sec: b.SetBytes(int64(len(input)))
}
```

Read `internal/tokenizer/tokenizer.go` to find the public tokenizer API. Look for a `Tokenize()` or `NewTokenizer()` function.

**3. `registry/core/prim_bench_test.go` — Primitive call overhead benchmark**

```go
func BenchmarkPrimitiveCall(b *testing.B) {
    // Call a simple primitive (e.g., +, car, cons) N times via engine.Eval().
    // This measures the overhead of primitive dispatch, argument extraction, and result boxing.
}
```

Each benchmark should be self-contained. Read the existing benchmark files (`values/integer_bench_test.go`, `internal/parser/parser_bench_test.go`, `environment/environment_bench_test.go`) for import patterns and setup conventions.

Run `go test -bench=. -benchmem ./machine/ ./internal/tokenizer/ ./registry/core/` to verify.

---

### ~~F13 [Priority: Low] — Integration test helper duplication~~ **RETRACTED**

**Where**: `integration/r7rs_test.go`, `integration/quasisyntax_test.go`
**Original claim**: Both files independently define `getProjectRoot()`, `getSchemeBinary()`, `getLibPath()`, `getTestDataPath()` — four identical functions.

**Verification result**: **FALSE**. These functions are defined ONLY in `integration/r7rs_test.go`. The file `integration/quasisyntax_test.go` does NOT define or use them — it uses `wile.NewEngine()` and `engine.Eval()` directly without subprocess invocation. No duplication exists.

**No action required.**

---

### F14 [Priority: Low] [COMPLETE] — ByteVectorBufferdOutputPort typo in type name

**Where**: `values/byte_vector_buffered_output_port.go`
**What**: The type is named `ByteVectorBufferdOutputPort` — missing 'e' in "Buffered". Documented in `values/CLAUDE.local.md` as known.

**Why it matters**: Cosmetic, but it's a public type. Fixing it later with consumers is harder than fixing it now with zero consumers.

**Suggested fix**: Rename via `gorename` or IDE refactor. Update all references.

**Effort**: S

**Status**: COMPLETE — type renamed to `ByteVectorBufferedOutputPort` throughout codebase. Zero `Bufferd` occurrences remain in Go or markdown files.

**Remediation Instructions** (retained for reference):

The typo affects 31 Go references across 3 files and 9 markdown references across 4 docs.

**Step 1: Rename the type and constructor**

Use Serena's `rename_symbol` or `gorename` to rename:
- `ByteVectorBufferdOutputPort` → `ByteVectorBufferedOutputPort` (type)
- `NewByteVectorBufferdOutputPort` → `NewByteVectorBufferedOutputPort` (constructor)

The affected Go files:
1. `values/byte_vector_buffered_output_port.go` — 24 references (type def, interface assertions, all methods)
2. `values/port_coverage_test.go` — 6 references (test comments and function names)
3. `internal/extensions/io/prim_ports.go` — 1 reference (`values.NewByteVectorBufferdOutputPort()`)

If using manual rename: search for `BufferdOutput` (not `BufferedOutput`) to catch all occurrences. The filename `byte_vector_buffered_output_port.go` is already spelled correctly — only the Go identifier is wrong.

**Step 2: Update documentation**

Update these markdown files (search for `Bufferd`):
1. `values/CLAUDE.local.md` — lines ~111, ~120, ~348
2. `private/VALUES_TECH_DEBT_ASSESSMENT.md` — 3 references
3. `internal/extensions/io/CLAUDE.local.md` — 1 reference

**Step 3: Verify**

```bash
# Ensure no remaining occurrences of the typo
grep -r "Bufferd" --include="*.go" --include="*.md" .
# Should return 0 results

make test && make lint
```

---

## State of the Code (updated 2026-03-01)

The Wile codebase is well-architected with clean package layering, consistent error handling conventions (sentinel + wrap), and comprehensive test coverage for core semantics. The structural, signals, and tech debt reviews have remediated the most critical issues. Of the 14 original findings: **10 complete/resolved** (F1, F2, F3, F4, F5, F6, F7, F8, F9, F14), **1 retracted** (F13), **2 deferred by design** (F10 god-object, F11 internal extensions), leaving **1 actionable item**.

The F6/F7 "coverage gap" findings were artifacts of measuring by file count ratio rather than Go statement coverage — actual coverage is 86.4% and 95.2% respectively. No correctness issues remain.

## Remaining Actionable Items

1. **F12: Subsystem benchmarks** [S effort] — No dedicated benchmarks exist for `machine/` (VM dispatch), `internal/tokenizer/`, or `registry/core/` (primitive call overhead). High-level Gabriel suite catches macro regressions but can't isolate micro-level ones.

## Verification Log

Initial verification: 2026-02-28. Each finding independently investigated against the codebase.
Completion verification: 2026-03-01. All claimed completion statuses confirmed against current sources.

| Finding | Status | Verified | Notes |
|---------|--------|----------|-------|
| F1 | COMPLETE | ✓ 2026-03-01 | `values/promotion.go` exists. Dispatch generators referenced in all 7 type files + `promotion_test.go`. |
| F2 | COMPLETE | ✓ 2026-03-01 | Part A: opcode checklist at `opcode.go:22`. Part B: `invokeTransformerClosure()` in `expander_time_continuation.go` unifies both dispatch sites. |
| F3 | COMPLETE | ✓ 2026-03-01 | `Complex.HashCode()` at `complex.go:321`. `BigComplex.HashCode()` at `big_complex.go:635`. |
| F4 | COMPLETE | ✓ 2026-03-01 | `test_helpers_test.go` deleted. All ~112 test files migrated to `registry/testhelpers`. PR #372. |
| F5 | COMPLETE | ✓ 2026-03-01 | Single `test-scheme` at Makefile:133. No duplicate. |
| F6 | RESOLVED | ✓ 2026-03-01 | Statement coverage: **86.4%**. Original 13% was file count ratio, not Go coverage. |
| F7 | RESOLVED | ✓ 2026-03-01 | Statement coverage: **95.2%**. Original 29% was file count ratio, not Go coverage. |
| F8 | COMPLETE | ✓ 2026-03-01 | Doc comment at `compile_validated.go:25`. Cross-ref at `compile_time_continuation.go:177-179`. |
| F9 | COMPLETE | ✓ 2026-03-01 | Comment at `big_complex.go:261` documenting intentional LessThan omission. |
| F10 | Open (deferred) | ✓ 2026-03-01 | Now 1586 lines, 71 functions (was 1562/66). Slight growth, same concern. |
| F11 | Open (deferred) | ✓ 2026-03-01 | io/eval still in `internal/extensions/`. No public equivalents exist. |
| F12 | Open | ✓ 2026-03-01 | 0 benchmarks in machine/, registry/core/, internal/tokenizer/. |
| F13 | RETRACTED | ✓ | No duplication existed. |
| F14 | COMPLETE | ✓ 2026-03-01 | Zero `Bufferd` occurrences in .go files. |

