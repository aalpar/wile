# Architectural Review — Refactoring Opportunities

Updated: 2026-02-13

## Overview

Full-codebase architectural review covering: numeric tower, registry/core primitives, machine package, error handling, internal packages, and values/API surface. Findings are organized by priority tier.

Completed items have been removed. See git history for the original document.

---

## Tier 2: Systemic Duplication (HIGH impact)

### ~~2.1 Numeric Tower Type-Switch Copy-Paste~~ — DEFERRED

> **⚠️ DEFERRED INDEFINITELY ⚠️**
>
> Multiple attempts to unify the numeric tower dispatch have failed. The 7×7
> type-switch matrix across arithmetic methods is deeply entangled with
> promotion semantics, exactness contagion, and special-case handling (e.g.,
> `(* 0 +inf.0)`). Each attempt introduced subtle regressions or required
> so much parameterization that the "unified" version was harder to reason
> about than the copy-paste it replaced.
>
> **This item is deferred until a clean abstraction emerges.** Do not
> attempt further unification without a concrete design that has been
> validated against the full R7RS numeric edge-case suite first.

**Scope:** 7 files, ~2,100 lines
**Files:** `values/{integer,big_integer,float,big_float,rational,complex,big_complex}.go`

**Known issues (for reference, NOT currently actionable):**
- Inconsistent case ordering across methods and types
- Conversion helpers reimplemented per-type rather than centralized
- Same 7-branch type switch copy-pasted across all arithmetic methods

### 2.2 Operation Boilerplate in machine/

**Scope:** ~36 files, ~1,500 lines
**Files:** `machine/operation_*.go`
**Effort:** Medium

Every VM operation implements three identical methods — `SchemeString()`, `IsVoid()`, `EqualTo()` — that differ only in the type name. The actual `Apply()` logic is typically 3-10 lines, but the boilerplate doubles each file's size.

**Also found:** `mc.value = []values.Value{v}` vs `mc.value = MultipleValues{v}` — inconsistent value-setting in operations, even though `mc.SetValue()` already exists.

**Recommended fix:** Code-generate the boilerplate via `go generate`, or embed a shared base struct. The type-assertion pattern in `EqualTo` already uses `sameType()`/`fieldMatches()` helpers — close to mechanizable.

### 2.3 Port Type Duplication in values/

**Scope:** 9+ port types, ~400 lines
**Files:** `values/{binary_input_port,binary_output_port,character_input_port,character_output_port,string_input_port,string_output_port,byte_vector_output_port,byte_vector_buffered_output_port,byte_vector_input_port}.go`
**Effort:** Medium

Every I/O method on every port type follows the exact same guard-and-delegate pattern:

```go
func (p *PortType) ReadByte() (byte, error) {
    err := p.guardClosed()
    if err != nil { return 0, err }
    return p.rdr.ReadByte()
}
```

Repeated for Read, ReadByte, UnreadByte, ReadRune, UnreadRune, Write, WriteByte, Flush across all port types. Additionally, `IsVoid()`, `EqualTo()`, `SchemeString()` are identical across all port types.

**Recommended fix:** The `portBase` struct already handles Close/IsClosed. Extending it (or creating a read/write mixin) would eliminate ~400 lines.

### 2.4 Compiler Nil-Guard Duplication in machine/

**Scope:** 13 instances across 4 files, plus duplicated expand-compile-execute pattern in 3 files
**Files:** `machine/compile_begin_for_syntax.go`, `machine/compile_define_for_syntax.go`, `machine/compile_eval_when.go`, `machine/compile_time_continuation.go`
**Effort:** Low

Every compile-time form starts with:

```go
if p.env == nil { return WrapForeignErrorf(ErrUnexpectedNil, "%s: nil environment") }
if p.template == nil { return WrapForeignErrorf(ErrUnexpectedNil, "%s: nil template") }
```

**Recommended fix:** Extract `ensureState(ctx string) error` and `executeAtCompileTime(...)` on `CompileTimeContinuation`.

---

## Tier 3: Convention Violations (MEDIUM impact)

### 3.3 Byte Validation Duplication

**Scope:** 3 sites
**File:** `registry/core/prim_byte_vectors.go` (lines 45-46, 78, 120)
**Effort:** Trivial

Repeated `fillInt.Value < 0 || fillInt.Value > 255` check. Extract to `ValidateByteValue()` helper in `registry/helpers/`.

---

## Tier 4: Organizational (LOW impact)

### 4.1 compile_time_continuation.go is 2,371 Lines

**File:** `machine/compile_time_continuation.go`
**Effort:** Medium

Mixed concerns: core compilation, quasiquote (900 lines), library system (500 lines), include (200 lines). Split by domain:
- `compile_time_continuation.go` (core: ~600 lines)
- `compile_time_continuation_quasiquote.go` (~900 lines)
- `compile_time_continuation_library.go` (~500 lines)
- `compile_time_continuation_include.go` (~200 lines)

### 4.2 Validator Prologue Duplication

**Scope:** 19 validators
**Files:** `internal/validate/validate_*.go`
**Effort:** Low

All 19 validators repeat the same `collectList` + `improper` check + arity guard prologue (~4 lines each). The list collection step uses a shared `collectList()` helper, but the error-reporting boilerplate (improper-list check + arity guard) is still duplicated. A `validateFormPrologue()` helper would deduplicate.

### 4.3 Bytecode Instruction Files in match/

**Scope:** 13 files of ~27 lines each
**Files:** `internal/match/bytecode_*.go`
**Effort:** Low

Each contains a single struct + `String()` method. Could consolidate by category (capture, compare, visit, control).

### 4.4 Optional Fill Argument Extraction

**Scope:** 3 sites
**Files:** `registry/core/prim_vectors.go` (PrimMakeVector), `registry/core/prim_byte_vectors.go` (PrimMakeBytevector), `registry/core/prim_strings.go` (PrimMakeString)
**Effort:** Low

Three `make-*` primitives each independently extract optional fill arguments with slightly different patterns. Could share a helper.

### 4.5 Empty List Handling Inconsistency

**Scope:** 3+ patterns
**Files:** `registry/core/prim_lists.go`, `registry/core/prim_byte_vectors.go`, `registry/core/prim_strings.go`
**Effort:** Low

Three different patterns for checking empty list arguments in variadic operations (check-first, check-in-fallback, check-after-assertion). Standardize on explicit `IsEmptyList` check first.

---

## What's Working Well (Preserve)

- **Registration system** — Extension registration is clean, consistent, single source of truth
- **Error propagation** — Properly wrapped error sites, zero silent ignores
- **Public API** — `wile.Value` wrapper isolates internals, error types are structured, FFI is type-safe
- **Stack manipulation** — Clean, encapsulated, no duplication
- **Syntax utilities** — `SyntaxForEach`, `SyntaxList`, etc. are well-abstracted
- **Bootstrap initialization** — Centralized, correct ordering
- **BoolToBoolean adoption** — Production code fully converted
- **Constructor patterns** — All types use consistent `NewX()` pattern
- **SchemeString output** — R7RS-compliant across all types
- **Validator error accumulation** — Centralized via ValidationResult
- **Two-layer error convention** — Sentinel + wrap pattern enforced across ~80 call sites (refactor/panic-to-error-convention branch)
- **fmt.Errorf eliminated** — No `fmt.Errorf` in production code
- **BigInteger Hashable** — Now implements `HashCode()`
- **Bare sentinels eliminated** — All `NewForeignError` calls now use sentinels (PR #217)
- **Test error idioms** — `errors.Is(err, io.EOF)` consistent across test and production code
- **Optional position parsing** — `helpers.ParseSubrange` consolidates `[start [end]]` extraction

---

## Recommended Execution Order

| Phase | Items | Risk | Lines Saved |
|-------|-------|------|-------------|
| 1 (Low-risk dedup) | 2.4 | Low | ~100 |
| 2 (Larger refactors) | 2.3, 4.1, 4.2 | Low-Medium | ~600 |
| 3 (Code generation) | 2.2 | Medium | ~1,500 |
| DEFERRED | ~~2.1~~ | — | — |