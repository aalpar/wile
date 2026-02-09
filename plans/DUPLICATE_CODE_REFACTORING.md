# Duplicate Code Refactoring Plan

**Status**: In Progress
**Created**: 2026-01-31
**Last Updated**: 2026-02-09
**Initial Issues**: 39 duplicate code blocks
**Remaining Issues**: 26 duplicate code blocks
**Eliminated**: 13 duplicates (33% reduction)

## Overview

The linter identified 39 duplicate code blocks across 6 packages. 13 have been eliminated (output port helpers, parity checks, integer division, variadic comparisons, optional position extraction, registry helpers). 26 remain in two categories below.

## Remaining Pattern Categories

### 1. Number Type Switch Duplicates (Multiple)

**Location**: `values/big_complex.go`, `values/big_float.go`

**Duplicates**:
- `big_complex.go`: lines 321-347, 359-385
- `big_float.go`: lines 102-123, 132-153, 165-186, 195-216

**Pattern**: Type switches over numeric types with similar case handling

**Solution**: Extract common numeric type conversion/comparison logic

See also: ALGEBRAIC_REDUCTIONS.md §I (Numeric Tower Dispatch) for the broader consolidation plan.

**Files to modify**:
- `values/big_complex.go`
- `values/big_float.go`

---

### 2. Match Package Duplicates (2 duplicates)

**Location**: `internal/match/match.go`

**Duplicates**:
- Lines 162-224
- Lines 392-454

**Pattern**: Bytecode execution logic

**Solution**: Extract common bytecode handling into a helper function

**Files to modify**:
- `internal/match/match.go`

---

## Remaining Duplicates Summary (26 total)

### Values Package Arithmetic Operations (24 duplicates)

Similar type-switch patterns in arithmetic methods across numeric types:

- **integer.go**: 6 duplicates in Add/Subtract/Multiply (3 pairs)
- **big_integer.go**: 6 duplicates in Add/Subtract/Multiply (3 pairs)
- **float.go**: 2 duplicates in Add/Subtract
- **big_float.go**: 6 duplicates (3 pairs)
- **rational.go**: 6 duplicates (3 pairs)
- **complex.go**: 2 duplicates in Subtract/Multiply
- **big_complex.go**: 2 duplicates

**Complexity**: High — requires understanding the numeric tower promotion rules and exactness preservation. See ALGEBRAIC_REDUCTIONS.md §I for consolidation approach.

### Match Package (2 duplicates)

- **internal/match/match.go**: Lines 162-224 duplicate of 392-454 (bytecode execution in pattern matcher VM)

**Complexity**: High — VM bytecode execution logic, requires deep understanding of the pattern matcher

## Next Steps

The remaining 26 duplicates are in performance-critical paths (numeric arithmetic) and complex VM logic (pattern matcher). These require deeper refactoring than the completed items:

1. **Values arithmetic**: Would benefit from a comprehensive refactoring of the numeric tower, possibly extracting common promotion logic
2. **Match bytecode**: Requires deep VM knowledge; defer until pattern matcher is better understood
