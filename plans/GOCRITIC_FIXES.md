# gocritic Linter Fixes

**Status**: In Progress
**Created**: 2026-01-31
**Last Updated**: 2026-02-09
**Initial Issues**: 50 gocritic warnings
**Remaining Issues**: 19 (all ifElseChain)
**Eliminated**: 31 warnings (62% reduction)

## Overview

The linter identified 50 gocritic warnings. 31 have been fixed across 8 categories (deferInLoop, evalOrder, sprintfQuotedString, appendCombine, underef, sloppyTypeAssert, singleCaseSwitch, deprecatedComment). 19 remain, all ifElseChain warnings.

## Remaining: ifElseChain (19 warnings)

**Issue**: Long if-else-if chains that could be switch statements.

**Locations**:
- Production: `go/extensions/gointerop/prim_gointerop.go:98`, `go/extensions/math/prim_math.go:1332`, `go/match/match.go:287,517,721`, `go/tokenizer/tokenizer.go:1741`, `go/values/array_list.go:65`, `go/values/pair.go:255`
- Test: `go/parser/parser_test.go:1843,1852,2283,2336`, `go/registry/core/prim_identifier_test.go:161`, `go/values/numeric_tower_coverage_test.go:105,131,157,183,242,293`

**Approach**: Review each case individually. Some if-else chains are clearer than switch (e.g., type checks with different operations). Only convert where switch improves readability.

**Impact**: Case-by-case analysis needed. Some conversions improve clarity, others may not.

## Testing

- All fixes should pass `make test` with no regressions
- Verify `make lint` reduces gocritic count
