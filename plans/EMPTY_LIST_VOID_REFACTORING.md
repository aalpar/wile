# EmptyList and Void Type Refactoring Plan

## Problem Summary

`EmptyList` is defined as `var EmptyList = NewCons(nil, nil)` — a `*Pair` singleton. This creates fundamental type confusion:

1. **Type switches lie**: `case *values.Pair` matches EmptyList, requiring `IsEmptyList()` guards in ~13 type switches
2. **`pair?` needs defensive check**: `(pair? '())` must return `#f`, but Go says EmptyList IS a `*Pair`
3. **Singleton fragility**: If `NewCons(nil, nil)` is called elsewhere, pointer comparison fails
4. **Boilerplate everywhere**: ~160+ call sites for `IsEmptyList()`, many purely defensive
5. **`IsVoid()` on nil `*Pair`**: A nil `*Pair` pointer reports `IsVoid() == true`, conflating void with nil-pointer-of-pair-type

The same pattern repeats in `SyntaxEmptyList` (`*SyntaxPair` singleton, ~138 call sites) and `ArrayListEmptyList` (`*ArrayList` singleton).

`Void` already has its own type (`voidType struct{}`), which is correct. But `IsVoid() bool` on the `Value` interface forces 46 types to implement identical `return p == nil` boilerplate.

**FIXME comments in**: `pair.go:28`, `pair.go:201`, `array_list.go:27`, `syntax_pair.go:30`

## Design Options

### Option A: Minimal — EmptyList Gets Its Own Type

Introduce `type emptyListType struct{}` implementing `Value` and `Tuple`, analogous to `voidType`. Leave Void and interfaces unchanged.

```go
// values/empty_list.go
type emptyListType struct{}

func (emptyListType) SchemeString() string               { return "()" }
func (emptyListType) IsVoid() bool                       { return false }
func (emptyListType) EqualTo(v Value) bool               { return IsEmptyList(v) }
func (emptyListType) Length() int                        { return 0 }
func (emptyListType) Append(v Value) Value               { return v }
func (emptyListType) ForEach(_ context.Context, _ ForEachFunc) (Value, error) { return EmptyList, nil }
func (emptyListType) IsEmptyList() bool                  { return true }
func (emptyListType) IsList() bool                       { return true }
func (emptyListType) AsVector() *Vector                  { return NewVector() }
func (emptyListType) Car() Value                         { panic(ErrNotAPair) }
func (emptyListType) Cdr() Value                         { panic(ErrNotAPair) }

var EmptyList Value = emptyListType{}
```

Parallel changes for `syntaxEmptyListType` in syntax layer. Remove `ArrayListEmptyList`.

**Scope**: ~30-40 files changed
**Pros**: Fixes core type confusion; type switches become reliable; `pair?` simplifies; follows existing `voidType`/`eofType` pattern
**Cons**: Must audit all code assuming EmptyList is `*Pair`; syntax layer needs parallel work
**Risk**: Medium — compiler catches most breakage since type changes

---

### Option B: Medium — Dedicated Types + Remove `IsVoid()` from Value Interface

Everything from Option A, plus:

- **Remove `IsVoid() bool` from `Value` interface** — eliminates 46 identical boilerplate methods
- **Simplify `values.IsVoid()`** to `v == nil || v == Void`
- **Remove `IsEmptyList()` from `Tuple` interface** — use type assertion or utility function instead

```go
// Simplified Value interface
type Value interface {
    SchemeString() string
    EqualTo(Value) bool
}

// Simplified Tuple interface (no IsEmptyList)
type Tuple interface {
    Value
    Length() int
    Append(Value) Value
    ForEach(ctx context.Context, fn ForEachFunc) (Value, error)
    IsList() bool
    AsVector() *Vector
    Car() Value
    Cdr() Value
}
```

**Scope**: ~70-80 files changed
**Pros**: All Option A benefits; removes 46 boilerplate methods; interfaces become smaller; eliminates confusion where `(*Pair)(nil).IsVoid() == true`
**Cons**: Breaking interface change; larger blast radius; `ArrayList.IsVoid()` has complex logic beyond `p == nil`
**Risk**: Medium-High — interface change affects any external `Value` implementors

---

### Option C: Comprehensive — Algebraic Redesign

Everything from A and B, plus:

- **Restructure `ArrayList`** to not use void/empty-list sentinels internally
- **Minimize `Tuple` interface** to irreducible core: `Length`, `ForEach`, `Car`, `Cdr`
- **Move `Append`, `IsList`, `AsVector`** to type-specific methods and utility functions
- **Optional `ListTerminator` interface** for explicit list-end semantics

**Scope**: ~100+ files changed, multiple PRs
**Pros**: Cleanest algebraic design; types reflect domain structure; ArrayList becomes a real data structure
**Cons**: Very large scope; ArrayList restructuring is complex; high regression risk
**Risk**: High — should be done incrementally over multiple PRs

## Recommendation

**Implement Option A first** as a single PR. It solves the primary problem with manageable scope. The compiler catches most breakage since `EmptyList` changes from `*Pair` to a new type.

**Option B** (`IsVoid()` removal) can follow as a separate PR — it's mechanical.

**Option C** (ArrayList redesign, interface minimization) is a long-term goal, not an immediate refactor.

## Implementation Steps (Option A)

### Step 1: Create `emptyListType`
- New file: `go/values/empty_list.go`
- Implement `Value` and `Tuple` interfaces
- Change `var EmptyList = NewCons(nil, nil)` to `var EmptyList Value = emptyListType{}`

### Step 2: Update `Pair`
- File: `go/values/pair.go`
- Remove `EmptyList` var (moved to empty_list.go)
- Remove or simplify `Pair.IsEmptyList()` — a Pair can never be an empty list anymore
- Update `Pair.Append()`, `Pair.ForEach()`, `Pair.IsList()` to compare against `EmptyList` (which is still a `Value`, just not a `*Pair`)
- Update `Pair.SchemeString()` — remove empty list branch

### Step 3: Update `ArrayList`
- File: `go/values/array_list.go`
- Remove `ArrayListEmptyList` singleton
- Simplify `ArrayList.IsEmptyList()` — an ArrayList is never the empty list

### Step 4: Update utility functions
- File: `go/values/utils.go`
- Simplify `IsEmptyList()`: `return v == EmptyList` (or keep Tuple delegation for compatibility)
- Simplify `IsList()`: EmptyList check is now a simple equality test

### Step 5: Simplify predicates
- File: `go/registry/core/prim_predicates.go`
- `PrimPairQ`: Remove `!values.IsEmptyList(pr)` guard — EmptyList won't match `*Pair`
- `PrimNullQ`: No change needed (uses `values.IsEmptyList()`)

### Step 6: Create `syntaxEmptyListType`
- File: `go/syntax/syntax_pair.go` (or new `syntax_empty_list.go`)
- Parallel implementation for syntax layer
- Update `SyntaxEmptyList` from `*SyntaxPair` to new type
- Update `IsSyntaxEmptyList()` in `go/syntax/utils.go`

### Step 7: Fix type switches and call sites
- ~13 type switches on `*Pair` — remove `IsEmptyList()` guards where they existed only to exclude EmptyList
- ~7 direct pointer comparisons (`v == values.EmptyList`) — these still work since EmptyList is still a singleton
- Audit `ForEach` callers that do `tail.(*Pair)` on the returned tail

### Step 8: Remove FIXME comments
- Remove the 4 FIXME comments about "consider using types for EmptyList and Void"

## Critical Files

| File | Changes |
|------|---------|
| `go/values/empty_list.go` | **New** — `emptyListType` definition |
| `go/values/pair.go` | Remove EmptyList var, update methods |
| `go/values/utils.go` | Simplify `IsEmptyList()`, `IsList()` |
| `go/values/array_list.go` | Remove `ArrayListEmptyList`, simplify `IsEmptyList()` |
| `go/syntax/syntax_pair.go` | Create `syntaxEmptyListType`, update `SyntaxEmptyList` |
| `go/syntax/utils.go` | Update `IsSyntaxEmptyList()` |
| `go/registry/core/prim_predicates.go` | Simplify `PrimPairQ` |
| ~13 files with `case *Pair` switches | Remove EmptyList guards |
| ~20+ files using EmptyList directly | Audit for `*Pair` assumptions |

## Verification

1. `cd go && go build ./...` — compiler will catch most type mismatches
2. `cd go && go test ./...` — full test suite
3. Specific predicate tests: `cd go && go test -run "TestNullQ\|TestPairQ\|TestListQ\|TestVoidQ" ./registry/core/...`
4. Run R7RS conformance: `./dist/scheme --file lib/scheme/test/r7rs-tests.scm` (if available)
5. Check: `(pair? '())` → `#f`, `(null? '())` → `#t`, `(list? '())` → `#t`, `(eq? '() '())` → `#t`
