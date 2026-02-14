# Plan: Empty List Handling Consistency (Item 4.5)

**Source:** `plans/ARCHITECTURAL_REVIEW_REFACTORING.md` §4.5
**Scope:** Low — 2 functions to fix
**Risk:** Low — behavioral equivalence, no logic changes

---

## Problem

Three patterns coexist for checking empty list arguments in variadic primitives:

| Pattern | Shape | Where |
|---------|-------|-------|
| **Check-first** ✓ | `IsEmptyList` → early return → Tuple assertion | Most functions |
| **Check-in-fallback** | Tuple assertion → `IsEmptyList` in `!ok` branch | `PrimStringAppend` |
| **Implicit** | No explicit check; relies on ForEach no-op + post-hoc length | `PrimAppend` |

### Why this matters

`emptyListType` implements `Tuple` (see `values/empty_list.go:29`), so:
- `o.(values.Tuple)` **succeeds** for `EmptyList`
- `EmptyList.Car()` / `EmptyList.Cdr()` **panic** with `ErrNotAPair`
- `EmptyList.ForEach()` is a safe no-op (returns `(self, nil)`)

This means:
1. **Check-in-fallback is dead code** — the `IsEmptyList` branch inside `!ok` is unreachable because the Tuple assertion succeeds for EmptyList.
2. **Implicit pattern obscures intent** — a reader must trace through ForEach to understand that `(append)` returns `()`.

Standardize on **check-first**: explicit `IsEmptyList` guard before the Tuple assertion.

---

## Sites to Change

### 1. `PrimStringAppend` — prim_strings.go:220-248

**Current (check-in-fallback):**
```go
o := mc.Arg(0)
tuple, ok := o.(values.Tuple)
if !ok {
    if values.IsEmptyList(o) {      // ← dead code
        mc.SetValue(values.NewString(""))
        return nil
    }
    return error
}
```

**Target (check-first):**
```go
o := mc.Arg(0)
if values.IsEmptyList(o) {
    mc.SetValue(values.NewString(""))
    return nil
}
tuple, ok := o.(values.Tuple)
if !ok {
    return error
}
```

### 2. `PrimAppend` — prim_lists.go:92-156

**Current (implicit):**
```go
o := mc.Arg(0)
args, ok := o.(values.Tuple)      // succeeds for EmptyList
if !ok {
    return error                   // no IsEmptyList check
}
// ForEach is no-op for EmptyList
// ...
if len(lists) == 0 {              // catches empty case post-hoc
    mc.SetValue(values.EmptyList)
    return nil
}
```

**Target (check-first):**
```go
o := mc.Arg(0)
if values.IsEmptyList(o) {
    mc.SetValue(values.EmptyList)
    return nil
}
args, ok := o.(values.Tuple)
if !ok {
    return error
}
// ForEach processes non-empty list
// ...
// Remove `len(lists) == 0` check — unreachable after check-first
```

---

## Sites Already Correct (no changes)

**Check-first pattern:**
- `PrimReverse` (prim_lists.go:162)
- `PrimLength` (prim_lists.go:189)
- `PrimListRef` (prim_lists.go:225)
- `PrimListCopy` (prim_lists.go:527)
- `PrimBytevector` (prim_byte_vectors.go:60)
- `PrimBytevectorAppend` (prim_byte_vectors.go:183)
- `PrimListToString` (prim_strings.go:163)
- `ListToVector` (helpers/list.go:27)

**Loop-guard pattern** (idiomatic — `for !IsEmptyList(x)` as loop condition):
- `PrimString` (prim_strings.go:33)
- `PrimMemq` (prim_lists.go:328)
- `PrimMemv` (prim_lists.go:348)
- `CollectVectors` (helpers/list.go:55)

---

## Verification

1. Run `go test ./registry/core/... -run "Append|StringAppend"` for targeted tests
2. Run full `make test` to confirm no regressions
3. Run `make lint` for formatting

---

## Execution

Single phase, two edits + verify. No new tests needed — empty-list behavior is unchanged.
