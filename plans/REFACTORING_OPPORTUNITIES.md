# Refactoring Opportunities

All items complete. For larger structural reductions (numeric tower, port base types, operation boilerplate, etc.), see `ALGEBRAIC_REDUCTIONS.md`.

## Completed

| # | Refactoring | Impact | Commit |
|---|-------------|--------|--------|
| 1 | Form-Checking Predicates — unified `isExpandedDefineForm`/`isDefineSyntaxSyntax` into `isSyntaxFormWithKeyword` | ~20 lines | PR #170 (`36f5e54`) |
| 2 | CxR Accessor Primitives — replaced 28 hand-unrolled functions with `makeCxrPrimitive` factory + `cxrSpecs` table | ~240 lines | `f4498c0` |
| 3 | Compile-Time Form Argument Extraction — extracted `formArgs`/`formSingleArg` helpers | ~50 lines | PR #171 (`cc218c4`, `aa15b85`) |

> Items previously numbered 4 (Compile-Time Code Execution), 5 (Type Assertion Helpers), and 6 (Optional Range Argument Parsing) were moved to `CODE_CONSOLIDATION_ARCHITECTURAL.md`. Item 4 was completed as part of `f4498c0` (`expandCompileExecute` helper).
