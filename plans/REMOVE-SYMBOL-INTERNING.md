# Remove Symbol Interning Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove symbol canonicalization (interning) and compare symbols by string key instead of pointer identity.

**Architecture:** Change `eq?`/`eqv?`/`memq`/`assq` to compare `*Symbol` by `.Key` string. Remove `InternSymbol` from `TopLevelEnvironment`, `EnvironmentFrame`, and `GlobalEnvironmentFrame`. Remove `symbolInterns` map + mutex from `TopLevelEnvironment`. Remove the dead `SymbolInterner` interface. ~50 `InternSymbol` call sites become identity or simple allocation.

**Benchmark evidence:** `InternThenEq` 16.9 ns/op vs `AllocThenStringEqEscaped` 14.1 ns/op — net 17% faster amortized. See `environment/intern_bench_test.go`.

---

### Task 1: Add `helpers.EqIdentity` — shared eq? comparator

**Files:**
- Modify: `registry/helpers/equality.go`

**Step 1: Add EqIdentity function**

Add after the existing `Eqv` function:

```go
// EqIdentity implements eq? semantics: pointer identity for all types
// except symbols, which compare by name (R7RS §6.1, §6.5).
func EqIdentity(a, b values.Value) bool {
	sa, ok := a.(*values.Symbol)
	if ok {
		sb, ok2 := b.(*values.Symbol)
		if ok2 {
			return sa.Key == sb.Key
		}
		return false
	}
	return a == b
}
```

**Step 2: Add `*Symbol` case to `Eqv`**

In `Eqv()`, add a `*values.Symbol` case in the type switch (after the existing cases). Currently symbols fall through to `a == b` at line 24.

```go
case *values.Symbol:
	sb, ok := b.(*values.Symbol)
	if ok {
		return sa.Key == sb.Key
	}
```

**Step 3: Run tests**

Run: `go test ./registry/...`

**Step 4: Commit**

```
feat: add EqIdentity helper and symbol case to Eqv
```

---

### Task 2: Wire `eq?`, `memq`, `assq` to use `EqIdentity`

**Files:**
- Modify: `registry/core/prim_equality.go`
- Modify: `registry/core/prim_lists.go`

**Step 1: Update PrimEqQ**

Replace the body of `PrimEqQ` (`prim_equality.go:26-33`):

```go
func PrimEqQ(mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)
	mc.SetValue(values.BoolToBoolean(helpers.EqIdentity(o0, o1)))
	return nil
}
```

**Step 2: Update PrimMemq**

Replace the comparator in `PrimMemq` (`prim_lists.go:329-332`):

```go
func PrimMemq(mc *machine.MachineContext) error {
	return helpers.MemberLookup(mc, "memq", helpers.EqIdentity)
}
```

**Step 3: Update PrimAssq**

Replace the comparator in `PrimAssq` (`prim_lists.go:411-413`):

```go
func PrimAssq(mc *machine.MachineContext) error {
	return helpers.AssocLookup(mc, "assq", helpers.EqIdentity)
}
```

**Step 4: Run tests**

Run: `go test ./registry/...`

All existing eq?/memq/assq tests should pass — interning is still active, so pointer equality still works. The new comparator is strictly more permissive.

**Step 5: Commit**

```
feat: use EqIdentity for eq?, memq, assq (symbol string compare)
```

---

### Task 3: Remove `InternSymbol` from environment types

**Files:**
- Modify: `environment/top_level_environment.go`
- Modify: `environment/environment_frame.go`
- Modify: `environment/global_environment_frame.go`
- Modify: `internal/syntax/syntax_symbol.go`

**Step 1: Remove from TopLevelEnvironment**

In `top_level_environment.go`:
- Delete `symbolInterns` field and `symbolInternsMu` field from the struct (lines 61-62)
- Delete `symbolInterns: make(...)` from `NewTopLevelEnvironment()` (line 101)
- Delete the `InternSymbol` method (lines 135-165)
- Delete `SymbolInternCount` method (lines 533-538)
- Update comments that reference symbol interning (lines 56-57, 371-391)

**Step 2: Remove from EnvironmentFrame**

In `environment_frame.go`:
- Delete the `InternSymbol` method (lines 883-895)
- Remove `InternSymbol` from the comment at line 125

**Step 3: Remove from GlobalEnvironmentFrame**

In `global_environment_frame.go`:
- Delete the `InternSymbol` method (lines 300-312)
- Remove `key = p.InternSymbol(key)` from `CreateGlobalBinding` (line 173)
- Remove `key = p.InternSymbol(key)` from `GetGlobalIndex` (line 197)
- Remove `key := p.InternSymbol(gi.Index)` from `GetOwnGlobalBinding` (line 216) — replace with `key := gi.Index`

**Step 4: Remove from EnvironmentFrame global methods**

In `environment_frame.go`:
- Remove `key = p.InternSymbol(key)` from `GetGlobalIndex` (line 745)
- Remove `key = p.InternSymbol(key)` from `GetGlobalIndexAcrossPhases` (line 776)

**Step 5: Delete SymbolInterner interface**

In `internal/syntax/syntax_symbol.go`, delete the `SymbolInterner` interface (lines 35-40). It's defined but never referenced outside the file.

**Step 6: Run tests, fix compilation**

Run: `go test ./environment/... ./internal/syntax/...`

Expect compilation errors in test files that call `InternSymbol` — fix by removing the calls (replace `env.InternSymbol(values.NewSymbol("x"))` with `values.NewSymbol("x")`).

**Step 7: Commit**

```
refactor: remove InternSymbol from environment types
```

---

### Task 4: Remove InternSymbol calls from all call sites

**Files (grouped by package):**

**engine.go** (3 sites):
- Line 289: `sym := p.env.InternSymbol(values.NewSymbol(name))` → `sym := values.NewSymbol(name)`
- Line 296: same pattern
- Line 307: same pattern

**registry/apply.go** (4 sites):
- Lines 92, 98, 117, 129: same pattern

**registry/core/prim_strings.go** (1 site):
- Line 207: `sym = mc.EnvironmentFrame().InternSymbol(sym)` → delete line

**registry/core/prim_syntax.go** (2 sites):
- Lines 199-200: `sym0 := env.InternSymbol(...)` → `sym0 := values.NewSymbol(id0.Sym.Key)`

**machine/*.go** (~20 sites):
- `compile_time_continuation.go`: lines 86, 208, 389
- `compile_validated.go`: lines 240, 425, 540, 563, 642
- `compile_syntax_rules.go`: line 418
- `compile_syntax_form.go`: line 127
- `compile_syntax_case.go`: line 312
- `compile_define_for_syntax.go`: line 90
- `compile_time_continuation_include.go`: line 257
- `expander_time_continuation.go`: lines 163, 519, 971, 1032, 1266
- `library_bindings.go`: lines 230, 250, 266, 292, 312
- `operation_syntax_case.go`: line 143
- `phase_registry.go`: line 45

**extensions/*.go** (4 sites):
- `extensions/introspection/prim_introspection.go`: lines 56, 83, 111
- `extensions/system/prim_system.go`: line 181

**internal/*.go** (2 sites):
- `internal/parser/parser_syntax.go`: line 40
- `internal/repl/meta.go`: line 204
- `internal/extensions/eval/prim_eval.go`: line 485

**Transformation patterns:**
- `env.InternSymbol(values.NewSymbol("name"))` → `values.NewSymbol("name")`
- `env.InternSymbol(sym)` → `sym`
- `env.InternSymbol(expr.Sym)` → `expr.Sym`

**Step 1: Apply all transformations**

Work package by package. Each site is mechanical: remove the `InternSymbol` wrapper.

**Step 2: Run full test suite**

Run: `go test ./...`

**Step 3: Run lint**

Run: `make lint`

Remove any unused imports of `values` or `environment` that result from the changes.

**Step 4: Commit**

```
refactor: remove all InternSymbol call sites (~50 sites)
```

---

### Task 5: Update machine/native_template.go comment

**Files:**
- Modify: `machine/native_template.go`

**Step 1: Remove stale comment**

Line 45 references "bypassing the runtime InternSymbol → RLock → map lookup path." — update or remove this comment since InternSymbol no longer exists.

**Step 2: Commit with Task 4 or separately**

---

### Task 6: Clean up tests and benchmarks

**Files:**
- Modify: `environment/intern_bench_test.go` — remove `BenchmarkInternSymbolHit`, `BenchmarkInternSymbolMiss`, `BenchmarkInternThenEq`. Keep the comparison benchmarks.
- Remove: tests that assert `SymbolInternCount` or pointer identity of interned symbols
- Modify: `environment/top_level_environment_test.go` — remove `TestTopLevelEnvironment_SymbolInternCount` and related tests

**Step 1: Update benchmark file**

Remove the interning benchmarks (they'd no longer compile). Keep the comparison and memq benchmarks as the baseline record.

**Step 2: Fix all test compilation errors**

Search test files for `InternSymbol` calls and replace with direct symbol creation. Approximately 40 test sites across `environment/`, `machine/`, `registry/core/`.

**Step 3: Run full suite**

Run: `make lint && make covercheck`

**Step 4: Commit**

```
test: update tests and benchmarks for symbol interning removal
```

---

### Task 7: Update documentation

**Files:**
- Modify: `environment/CLAUDE.local.md` — remove references to symbol interning, update type diagrams
- Modify: `values/CLAUDE.local.md` — update Symbol type description (remove "Interned per-VM")
- Modify: `machine/CLAUDE.local.md` — remove "Symbol interning" from Gotchas
- Modify: `CLAUDE.md` — update architecture section if it mentions interning

**Step 1: Update CLAUDE files**

Remove references to `InternSymbol`, `symbolInterns`, interning delegation, and per-instance interning from all CLAUDE documentation files.

**Step 2: Commit**

```
docs: update documentation for symbol interning removal
```

---

## Verification Checklist

- [ ] `make lint` passes
- [ ] `make covercheck` passes (all 29 packages ≥ 80%)
- [ ] `go test ./...` passes
- [ ] No remaining references to `InternSymbol` in non-test Go files
- [ ] No remaining `symbolInterns` field in `TopLevelEnvironment`
- [ ] `eq?` returns `#t` for `(eq? 'foo 'foo)` and `(eq? 'foo (string->symbol "foo"))`
- [ ] `memq`/`assq` work correctly on symbol lists
- [ ] `eqv?` returns `#t` for same-name symbols
