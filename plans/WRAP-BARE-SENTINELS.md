# Plan: Wrap Bare Sentinel Panics and Returns

**Status**: Complete
**Effort**: S (mechanical, ~30 edits + 1 ruleguard rule)
**Risk**: Low — wrapping doesn't change behavior, only enriches error context

## Problem

28 sites panic or return bare sentinel errors without `WrapForeignErrorf` wrapping. This violates the project's error handling invariant (CLAUDE.md: "NEVER panic with raw errors — always wrap with location context"). The convention audit found:

- 20 `panic(werr.ErrXxx)` without wrapping
- 7 `return nil, werr.ErrDivisionByZero` without wrapping
- 1 `err == readline.ErrInterrupt` instead of `errors.Is`

## Phases

### Phase 1: Wrap raw sentinel panics (20 sites)

Each `panic(werr.ErrXxx)` becomes `panic(werr.WrapForeignErrorf(werr.ErrXxx, "Site: what failed"))`.

#### Group A: `values/pair.go` (3 sites)

| Line | Current | Wrapped Message |
|------|---------|-----------------|
| 139 | `panic(werr.ErrNotAList)` | `"Pair.Append: receiver is not a proper list"` |
| 169 | `panic(werr.ErrNotAList)` | `"Pair.Append: improper list during spine copy"` |
| 236 | `panic(werr.ErrNotAList)` | `"Must: tail is not empty list"` |

#### Group B: `values/empty_list.go` (2 sites)

| Line | Current | Wrapped Message |
|------|---------|-----------------|
| 83 | `panic(werr.ErrNotAPair)` | `"emptyList.Car: empty list has no car"` |
| 88 | `panic(werr.ErrNotAPair)` | `"emptyList.Cdr: empty list has no cdr"` |

#### Group C: `values/promotion.go` (1 site)

| Line | Current | Wrapped Message |
|------|---------|-----------------|
| 308 | `panic(werr.ErrNotANumber)` | `"Promote: no promoter from %s to %s"` (use the NumericKind names) |

#### Group D: `internal/syntax/syntax_pair.go` (8 sites)

| Line | Current | Wrapped Message |
|------|---------|-----------------|
| 151 | `panic(werr.ErrNotAList)` | `"SyntaxPair.Append: receiver is void"` |
| 168 | `panic(werr.ErrNotAList)` | `"SyntaxPair.Append: traversal reached void"` |
| 172 | `panic(werr.ErrNotASyntaxValue)` | `"SyntaxPair.Append: value is not a SyntaxValue"` |
| 181 | `panic(werr.ErrNotAList)` | `"SyntaxPair.SyntaxAppend: receiver is void"` |
| 198 | `panic(werr.ErrNotAList)` | `"SyntaxPair.SyntaxAppend: traversal reached void"` |
| 212 | `panic(werr.ErrNotAList)` | `"SyntaxPair.Length: improper list"` |
| 338 | `panic(werr.ErrNotAList)` | `"SyntaxPair.AsVector: improper list"` |
| 357 | `panic(werr.ErrNotASyntaxList)` | `"SyntaxPair.AsSyntaxVector: improper list"` |

#### Group E: `internal/syntax/syntax_empty_list.go` (4 sites)

| Line | Current | Wrapped Message |
|------|---------|-----------------|
| 91 | `panic(werr.ErrNotAPair)` | `"syntaxEmptyList.Car: empty list has no car"` |
| 96 | `panic(werr.ErrNotAPair)` | `"syntaxEmptyList.Cdr: empty list has no cdr"` |
| 101 | `panic(werr.ErrNotAPair)` | `"syntaxEmptyList.SyntaxCar: empty list has no car"` |
| 106 | `panic(werr.ErrNotAPair)` | `"syntaxEmptyList.SyntaxCdr: empty list has no cdr"` |

#### Group F: `machine/stack.go` (2 sites)

| Line | Current | Wrapped Message |
|------|---------|-----------------|
| 43 | `panic(werr.ErrStackUnderflow)` | `"Stack.Pull: stack is empty"` |
| 55 | `panic(werr.ErrStackUnderflow)` | `"Stack.Pop: stack is empty"` |

**Verification**: `make lint && make test`

### Phase 2: Wrap bare division-by-zero returns (7 sites)

Each `return nil, werr.ErrDivisionByZero` becomes `return nil, werr.WrapForeignErrorf(werr.ErrDivisionByZero, "Type.Divide: division by exact zero")`.

| File | Line | Wrapped Message |
|------|------|-----------------|
| `values/integer.go` | 260 | `"Integer.Divide: division by exact zero"` |
| `values/float.go` | 156 | `"Float.Divide: division by exact zero"` |
| `values/rational.go` | 186 | `"Rational.Divide: division by exact zero"` |
| `values/big_integer.go` | 220 | `"BigInteger.Divide: division by exact zero"` |
| `values/big_float.go` | 228 | `"BigFloat.Divide: division by exact zero"` |
| `values/big_complex.go` | 264 | `"BigComplex.Divide: division by exact zero"` |
| `values/complex.go` | 151 | `"Complex.Divide: division by exact zero"` |

**Note**: All 7 sites check `o.IsZero() && o.IsExact()` before returning. The message says "exact zero" because R7RS only mandates an error for division by exact zero — inexact zero produces `+inf.0`.

**Verification**: `make lint && make test`

### Phase 3: Fix REPL error comparison (1 site)

| File | Line | Current | Fixed |
|------|------|---------|-------|
| `internal/repl/repl.go` | 161 | `if err == readline.ErrInterrupt {` | `if errors.Is(err, readline.ErrInterrupt) {` |

May need to add `"errors"` to the import block if not already present.

**Verification**: `make lint && make test`

### Phase 4: Add ruleguard rule to prevent regression

Add a new rule to `ruleguard/rules.go` that flags `panic(werr.ErrXxx)` patterns — any panic whose argument is a direct reference to a `werr.Err*` sentinel without `WrapForeignErrorf`.

```go
// noBareSentinelPanic flags panic calls with bare werr sentinel errors.
// Project convention: always wrap with WrapForeignErrorf for site context.
//
//	// Wrong:
//	panic(werr.ErrNotAList)
//
//	// Right:
//	panic(werr.WrapForeignErrorf(werr.ErrNotAList, "site: what failed"))
func noBareSentinelPanic(m dsl.Matcher) {
	m.Match(`panic(werr.$err)`).
		Where(m["err"].Text.Matches(`^Err[A-Z]`)).
		Report(`panic with bare sentinel: wrap with werr.WrapForeignErrorf(werr.$err, "site: context")`)
}
```

**Open question**: Should a similar rule flag `return ..., werr.ErrXxx` bare returns? The `return` case is harder to express in ruleguard because the sentinel can appear at any position. Worth investigating but not blocking — the 7 division-by-zero sites are the only known instances and they'll be fixed in Phase 2.

**Verification**: `make lint && make test`

## Execution Order

Phases 1-3 are independent — all are mechanical edits. Phase 4 depends on Phase 1 completing (otherwise the new rule would flag the sites we haven't fixed yet).

Recommended: do Phases 1+2+3 together as one commit, then Phase 4 as a second commit. Or all four in one commit if preferred.

## Checklist

- [x] Phase 1: Wrap 20 raw sentinel panics
- [x] Phase 2: Wrap 7 bare division-by-zero returns
- [x] Phase 3: Fix REPL error comparison
- [x] Phase 4: Add ruleguard rule
- [x] `make lint` passes
- [x] `make test` passes
- [x] Update TODO.md — mark items done
