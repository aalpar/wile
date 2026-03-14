# Environment Package Cleanup

**Status:** Complete
**Verified:** 2026-03-14 — all tasks confirmed against current sources

**Goal:** Eliminate technical debt in `environment/` identified by staff-engineer assessment — constructor duplication, dead delegation, semantic inconsistency, and minor correctness issues.

**Architecture:** All changes are internal to `environment/`. No public API signatures change (except removing methods that have zero external callers). The package has 143 downstream files — every change must preserve backward compatibility or prove zero external usage.

---

## Copier Interface Analysis

**Question:** Should a `Copier` interface be introduced for types implementing `Copy()`?

**Finding: No.** No code dispatches on `Copy()` polymorphically. Every call site knows the concrete type. YAGNI.

---

## Task Summary

| Task | Description | Status |
|------|-------------|--------|
| 1 | Extract `initRuntimeFrame` constructor helper | Complete |
| 2 | Remove dead `InternSyntax` delegation methods | Complete |
| 3 | Fix `LibraryRegistry` delegation chain | Complete |
| 4 | Fix `HasLocalVariableBinding` semantics | Complete |
| 5 | Consolidate `MaybeCreateLocalBinding` using `resolveLocal` | Complete |
| 6 | Document `GetLocalIndexWithScopes` walk coupling | Superseded |
| 7 | Fix `GlobalIndex.EqualTo` nil check + concrete `Copy` return | Complete |
| 8 | Switch `LoadPathStack` to `RWMutex` | Complete |

---

## Task 1: Extract shared constructor helper in TopLevelEnvironment — Complete

**Evidence:** `initRuntimeFrame` extracted at `top_level_environment.go:486`. Used by `NewTopLevelEnvironment` (line 100), `NewChildTopLevelEnvironment` (line 387), `NewSchemeReportEnvironment` (line 410). `NewChildRuntime` correctly uses `newPhaseRegistryForChild` (line 500) to avoid the initialization ordering issue.

---

## Task 2: Remove dead InternSyntax delegation methods — Complete

**Evidence:** Only `TopLevelEnvironment.InternSyntax` exists (`top_level_environment.go:104`). `EnvironmentFrame.InternSyntax` and `GlobalEnvironmentFrame.InternSyntax` have been removed. Zero external callers of `InternSyntax` exist anywhere in the codebase.

---

## Task 3: Fix LibraryRegistry delegation chain — Complete

**Evidence:** `EnvironmentFrame.LibraryRegistry()` delegates directly to `p.topLevel.LibraryRegistry()` (`environment_frame.go:312`). `GlobalEnvironmentFrame.LibraryRegistry` and `SetLibraryRegistry` have been removed. Zero `.global.LibraryRegistry` call sites remain.

---

## Task 4: Fix HasLocalVariableBinding semantics — Complete

**Evidence:** `HasLocalVariableBinding` uses `resolveLocal` with `checkScopes=true` (`environment_frame.go:576`). Walks all scope-compatible bindings in the parent chain, not just the innermost by name. Aligns with `GetLocalIndexWithScopes` per Flatt's hygiene model. Test `TestHasLocalVariableBinding_OuterScopeCompatible` (line 905) verifies the fix.

---

## Task 5: Consolidate MaybeCreateLocalBinding using resolveLocal — Complete

**Evidence:** `MaybeCreateLocalBinding` delegates parent-chain walk to `resolveLocal` (`environment_frame.go:537`). The hand-rolled loop has been eliminated.

---

## Task 6: Document GetLocalIndexWithScopes walk coupling — Superseded

`GetLocalIndexWithScopes` delegates to `resolveLocal` directly. The coupling that needed documenting no longer exists.

---

## Task 7: Fix GlobalIndex.EqualTo nil check and Copy return types — Complete

**Evidence:**
- **7A:** `GlobalIndex.EqualTo` has correct `p == nil || value == nil` order and simplified `return v.Index.EqualTo(p.Index)` (`global_environment_frame.go:59`). Note: the plan's claim that `p == nil` is "unreachable" is incorrect — Go allows method calls on nil pointer receivers. The check is valid and correctly ordered.
- **7B:** `GlobalEnvironmentFrame.Copy()` returns `*GlobalEnvironmentFrame` (`global_environment_frame.go:105`). No type assertions at call sites.

---

## Task 8: Switch LoadPathStack to RWMutex — Complete

**Evidence:** `LoadPathStack` uses `sync.RWMutex` (`load_path_stack.go:38`). Read-only methods (`Current`, `Depth`) use `RLock`; writers (`Push`, `Pop`) use `Lock`.
