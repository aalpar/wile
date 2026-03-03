# Fix Tuple ForEach Nil Semantics

**TODO item**: #1 [Medium, S]
**Status**: Draft

---

## Problem

`(*Pair).ForEach` returns `Void` (the `voidType{}` singleton) when the receiver is nil (`pair.go:184`):

```go
func (p *Pair) ForEach(ctx context.Context, fn ForEachFunc) (Value, error) {
    if p == nil {
        return Void, nil   // BUG: should return EmptyList
    }
    ...
```

The `Tuple` interface contract (`values.go:163-165`) says:

> ForEach calls fn for each element in order. Returns the tail value
> (EmptyList for proper lists, the improper cdr otherwise).

A nil `*Pair` dispatched through the `Tuple` interface means "traverse nothing" — the iteration is empty, and the tail of an empty proper list is `EmptyList`, not `Void`. Returning `Void` violates the contract and silently produces wrong behavior for any caller that chains on the tail (e.g. appending to it, comparing it with `EmptyList`).

**Note on Void vs nil `*Pair`**: `Void` is `voidType{}` (`values.go:52`), a separate singleton type. A nil `*Pair` reports `IsVoid() == true` (`pair.go:233`) but is NOT the Void singleton — they are distinct types. The nil guard returns the actual `voidType{}` object, not "itself."

**Contrast with `emptyListType.ForEach`** (`empty_list.go:62`), which correctly returns `p` (the EmptyList singleton):

```go
func (p emptyListType) ForEach(_ context.Context, _ ForEachFunc) (Value, error) {
    return p, nil  // Correct: returns EmptyList
}
```

**The same bug exists in `SyntaxPair`** (`internal/syntax/syntax_pair.go`), which has three parallel nil-guard sites returning the wrong value:

| Method | Line | Returns | Should return |
|--------|------|---------|---------------|
| `SyntaxPair.ForEach` | 232-233 | `values.Void` | `values.EmptyList` |
| `SyntaxPair.SyntaxForEach` | 255-256 | `SyntaxVoid` | `SyntaxEmptyList` |

---

## Root Cause

Nil guards in ForEach methods return void values instead of empty-list values. No tests pinned the contract for these paths.

---

## Behavior Changes

The fix changes observable behavior beyond just the ForEach return value:

**`(*Pair)(nil).Length()`**: `Length()` (`pair.go:162`) wraps ForEach in `Must()`, which panics if the tail is not EmptyList. Today: panics with `ErrNotAList`. After fix: returns 0. This is correct — a nil `*Pair` through the Tuple interface acts as an empty traversal.

**`(*SyntaxPair)(nil).Length()`**: Same pattern (`syntax_pair.go:205`). Today: panics. After fix: returns 0.

---

## Fix

### Phase 1 — `values/pair.go`

**1a. Nil guard** (line 184): Change `return Void, nil` → `return EmptyList, nil`

**1b. Loop exit** (line 203): Change `return pr, nil` → `return EmptyList, nil`

The loop exit at line 203 is reached only if a pair's cdr is a nil `*Pair` (possible via Go construction, not via Scheme). When `pr` is nil after the loop, `pr` is a nil `*Pair` — returning it as `Value` produces a non-nil interface with a nil concrete value, not a proper EmptyList. Returning `EmptyList` is correct: the loop consumed all pairs, so the traversal ended at a proper-list boundary.

### Phase 2 — `internal/syntax/syntax_pair.go`

**2a. `ForEach` nil guard** (line 233): Change `return values.Void, nil` → `return values.EmptyList, nil`

**2b. `ForEach` loop exit** (line 250): Change `return pr, nil` → `return values.EmptyList, nil`

Note: the SyntaxPair for loop has an additional `!pr.IsEmptyList()` condition (line 237/260). Since `pr` is typed `*SyntaxPair`, it can never hold the `SyntaxEmptyList` singleton (`*syntaxEmptyListType`). The loop exits when either `pr == nil` (nil `*SyntaxPair`) or `pr.IsEmptyList() == true` (a non-nil `*SyntaxPair` with both Values nil, per `syntax_pair.go:218-222`). In both cases, `return pr, nil` returns the wrong thing — a nil or degenerate `*SyntaxPair` instead of the canonical empty-list singleton. Changing to `return values.EmptyList, nil` normalizes both cases to the correct tail value.

**2c. `SyntaxForEach` nil guard** (line 256): Change `return SyntaxVoid, nil` → `return SyntaxEmptyList, nil`

**2d. `SyntaxForEach` loop exit** (line 273): Change `return pr, nil` → `return SyntaxEmptyList, nil`

Same reasoning as 2b — `pr` at loop exit is either nil `*SyntaxPair` or a non-nil `*SyntaxPair` with `IsEmptyList() == true`.

### Phase 3 — Tests

**3a.** Add to `values/pair_test.go` a `TestPair_ForEach` (table-driven):

| Input receiver | Expected tail | Expected elements | Expected error |
|----------------|---------------|-------------------|----------------|
| `(*Pair)(nil)` | `EmptyList` | none | `nil` |
| proper list `(1 2)` | `EmptyList` | `[1, 2]` | `nil` |
| improper list `(1 . 2)` | `Integer(2)` | `[1]` | `nil` |

Use `valuestest.SchemeEquals` for tail comparisons. Use `context.TODO()` as the context.

**3b.** Add to `internal/syntax/syntax_pair_test.go` a `TestSyntaxPair_ForEach` (table-driven):

| Input receiver | Expected tail |
|----------------|---------------|
| `(*SyntaxPair)(nil)` | `values.EmptyList` |
| proper syntax list | `SyntaxEmptyList` (satisfies `values.Value`) |

And a parallel `TestSyntaxPair_SyntaxForEach`:

| Input receiver | Expected tail |
|----------------|---------------|
| `(*SyntaxPair)(nil)` | `SyntaxEmptyList` |
| proper syntax list | `SyntaxEmptyList` |

### Phase 4 — Verify

```bash
go test -v -run TestPair_ForEach ./values/...
go test -v -run TestSyntaxPair_ForEach ./internal/syntax/...
go test -v -run TestSyntaxPair_SyntaxForEach ./internal/syntax/...
make lint && make covercheck
```

---

## Non-Changes

- `utils.ForEach` (`utils.go:66`) — delegates to the concrete type's `ForEach`; fix propagates automatically.
- `emptyListType.ForEach` — already correct.
- `syntaxEmptyListType.ForEach` — already correct.
- No sentinel changes, no new error types.

---

## Risk

Low. The code changes are mechanical (void → empty-list in nil guards and loop exits). The behavior change in `Length()` is correct: nil receivers through the Tuple interface consistently act as empty traversals. No callers depend on the current Void return — any code that compared the tail to Void was already wrong per the interface contract.
