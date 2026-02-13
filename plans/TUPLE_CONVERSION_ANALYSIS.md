# Tuple Conversion Analysis

**Status:** Analysis complete, implementation not started

## Overview

Identifies locations where `*values.Pair` can be converted to `values.Tuple` to support both `*Pair` and `*ArrayList` uniformly. See `CLAUDE.md` for Tuple vs `*Pair` guidelines.

## Conversion Candidates

### HIGH PRIORITY — Easy Wins

| # | File | Functions | Operations Used | Risk |
|---|------|-----------|----------------|------|
| 1 | `machine/import_set_datum.go` | 7 `parseImportSet*FromDatum` functions (lines 109-282) | `Car()`, `Cdr()` only | LOW |
| 2 | `internal/schemeutil/syntax.go` | `DatumToSyntaxValue` (lines 104-126) | `Car()`, `Cdr()`, `ForEach()` | LOW |
| 3 | `internal/match/match.go` | `valuePairToSyntaxPair` (line 478) | `Car()`, `Cdr()` | LOW |

### MEDIUM PRIORITY

| # | File | Functions | Notes |
|---|------|-----------|-------|
| 4 | `registry/core/prim_lists.go` | `PrimAppend` (lines 91-156) | Already uses Tuple in validation, minor cleanup |

### CANNOT CONVERT

| # | File | Reason |
|---|------|--------|
| 1 | `internal/match/pattern_analyzer.go` | Uses `*Pair` pointer as map key (identity required) |
| 2 | `machine/native_template.go` | Mutates pair structure during deduplication |
| 3 | `internal/match/syntax_compiler.go` | Compile-time only; ArrayList never appears in macro patterns |

## Implementation

| Phase | Description |
|-------|-------------|
| 1 | Convert `import_set_datum.go` (7 functions) — change param types and assertions |
| 2 | Convert match package (`valuePairToSyntaxPair`) and `DatumToSyntaxValue` |
| 3 | Documentation (Tuple guidelines in CODING_STYLE.md) |

## Reference Implementations

Already converted and serving as patterns: `registry/helpers/list.go` (`ListToVector`, `CollectVectors`, `AssocLookup`) and `registry/helpers/args.go` (`RequireListArg`).
