# Extension API Contract System — Remaining Work

**Status:** Phase 1 complete (PRs #577, #578). Phases 2-4 open.

**Design doc:** `plans/2026-03-26-extension-contracts-design.md`
**Phase 2+ impl:** `plans/2026-03-26-extension-contracts-phase2-impl.md`

---

## Phase 2: Core Primitive Contracts (Mechanical)

Add `ParamTypes` and `ReturnType` to all remaining `registry/core/` primitives (~172).
Work through one file at a time:

| File | Primitives | Notes |
|------|-----------|-------|
| `predicates.go` | ~19 | All: `(obj) → boolean` |
| `arithmetic.go` | ~21 | Numeric tower types; some variadic |
| `pairs.go` | ~5 | `car`/`cdr` return `TypeAny` |
| `lists.go` | ~13 | Mixed: some `TypeList`, some `TypeAny` |
| `vectors.go` | ~13 | Similar to strings pattern |
| `byte_vectors.go` | ~10 | Similar to vectors pattern |
| `characters.go` | ~3 | `TypeCharacter` param/return |
| `control.go` | ~7 | `TypeProcedure` params |
| `equality.go` | ~3 | `(obj obj) → boolean` |
| `strings.go` | remaining | Already done in Phase 1 Task 7 |
| `exceptions.go` | ~9 | Mixed types |
| `hashtables.go` | ~10 | `TypeHashtable` first param |
| `boxes.go` | ~4 | Custom opaque types → `TypeAny` |
| `parameters.go` | ~4 | `TypeProcedure` or `TypeAny` |
| `prompts.go` | ~7 | `TypeProcedure` params |
| `syntax.go` | ~6 | Expand-time, `TypeAny` |
| `syntax_loc.go` | ~6 | Expand-time, `TypeAny` |
| `opaque.go` | ~2 | `TypeAny` |
| `reflection.go` | ~5 | `TypeProcedure` first param |
| `cont_marks.go` | ~8 | Mixed |

**Process per file:**
1. Read the `Prim*` implementations to verify actual type expectations
2. Add `ParamTypes` and `ReturnType` to each spec
3. Run `go test -v ./registry/core/...`
4. Commit per file or per logical group

---

## Phase 3: Extension Primitive Contracts (Mechanical)

Same process for `extensions/` (~133 primitives):

| Package | Primitives |
|---------|-----------|
| `extensions/files/` | ~13 |
| `extensions/math/` | ~35 |
| `extensions/system/` | ~8 |
| `extensions/process/` | ~8 |
| `extensions/threads/` | ~30 |
| `extensions/gointerop/` | ~33 |
| `extensions/introspection/` | ~6 |

Plus `internal/extensions/{io,eval,namespace,all}/`.

---

## Phase 4: Runtime Enforcement (Separate PR)

See `plans/2026-03-26-extension-contracts-phase2-impl.md` for detailed tasks.

**Prerequisite:** All primitives contracted (Phases 2-3 complete).
