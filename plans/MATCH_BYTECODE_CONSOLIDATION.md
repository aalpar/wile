# Match Bytecode Consolidation Plan

From: `ARCHITECTURAL_REVIEW_REFACTORING.md` §4.3

## Problem

13 bytecode instruction files in `internal/match/`, each containing a single struct + `String()` method (~23-39 lines including copyright header). The one-file-per-type layout adds navigational overhead without providing meaningful separation — the types are tightly coupled (all implement `SyntaxCommand`, all consumed by the same type switch in `match.go`).

## Current Layout

```
bytecode_capture_car.go          ByteCodeCaptureCar{Binding string}           imports: fmt
bytecode_capture_cdr.go          ByteCodeCaptureCdr{Binding string}           imports: fmt
bytecode_compare_car.go          ByteCodeCompareCar{Value syntax.SyntaxValue} imports: fmt, syntax
bytecode_compare_cdr.go          ByteCodeCompareCdr{Value syntax.SyntaxValue} imports: fmt, syntax
bytecode_done.go                 ByteCodeDone{}                               imports: (none)
bytecode_jump.go                 ByteCodeJump{Offset int}                     imports: fmt
bytecode_pop_context.go          ByteCodePopContext{EllipsisID int}           imports: fmt
bytecode_push_context.go         ByteCodePushContext{EllipsisID int}          imports: fmt
bytecode_require_car_empty.go    ByteCodeRequireCarEmpty{}                    imports: (none)
bytecode_skip_if_empty.go        ByteCodeSkipIfEmpty{Offset int}             imports: fmt
bytecode_skip_if_tail_count.go   ByteCodeSkipIfTailCount{Offset, Count int}  imports: fmt
bytecode_visit_car.go            ByteCodeVisitCar{}                           imports: (none)
bytecode_visit_cdr.go            ByteCodeVisitCdr{}                           imports: (none)
```

## Target Layout (4 files by category)

```
internal/match/
  bytecode_navigate.go    ── Navigation: VisitCar, VisitCdr, Done, RequireCarEmpty
  bytecode_compare.go     ── Literal matching: CompareCar, CompareCdr
  bytecode_capture.go     ── Variable capture: CaptureCar, CaptureCdr
  bytecode_control.go     ── Loop control & context: Jump, SkipIfEmpty, SkipIfTailCount, PushContext, PopContext
```

### Grouping Rationale

```
┌─────────────────────────────────────────────────────────┐
│ bytecode_navigate.go (tree traversal)                   │
│   VisitCar   ── descend into nested list                │
│   VisitCdr   ── advance to next sibling                 │
│   Done       ── pop nesting level                       │
│   RequireCarEmpty ── assert car is ()                   │
│   Imports: (none)                                       │
│   Rationale: all move through or validate input tree    │
│              position. RequireCarEmpty is a VisitCar     │
│              variant (replaces it for empty patterns).   │
├─────────────────────────────────────────────────────────┤
│ bytecode_compare.go (literal matching)                  │
│   CompareCar ── match literal at car position           │
│   CompareCdr ── match literal at cdr (improper tail)    │
│   Imports: fmt, internal/syntax                         │
│   Rationale: both compare current position against a    │
│              literal SyntaxValue. Only types needing     │
│              the syntax package.                         │
├─────────────────────────────────────────────────────────┤
│ bytecode_capture.go (variable binding)                  │
│   CaptureCar ── bind pattern variable (car)             │
│   CaptureCdr ── bind pattern variable (cdr/rest)        │
│   Imports: fmt                                          │
│   Rationale: both bind a named variable. Symmetric      │
│              car/cdr pair like the compare instructions. │
├─────────────────────────────────────────────────────────┤
│ bytecode_control.go (ellipsis loop machinery)           │
│   SkipIfEmpty      ── exit loop if list empty           │
│   SkipIfTailCount  ── exit loop when N remain           │
│   Jump             ── unconditional loop back           │
│   PushContext       ── start ellipsis capture iteration  │
│   PopContext        ── end ellipsis capture iteration    │
│   Imports: fmt                                          │
│   Rationale: all exist solely for ellipsis `...`        │
│              pattern handling. Together they form        │
│              the while-loop: skip → push → body →       │
│              pop → jump.                                 │
└─────────────────────────────────────────────────────────┘
```

## Execution Steps

### 1. Create consolidated files

Create 4 new files, each containing:
- Copyright header (same as existing)
- `package match`
- Combined imports (union of what the grouped types need)
- All type definitions + `String()` methods, preserving existing doc comments
- Types ordered within each file to match the logical flow (e.g., VisitCar → VisitCdr → Done → RequireCarEmpty)

### 2. Delete the 13 individual files

Remove all `bytecode_*.go` files that were consolidated.

### 3. Verify

- `go build ./internal/match/...` — compiles
- `go test ./internal/match/...` — all tests pass
- `go vet ./internal/match/...` — clean
- `make lint` — clean

### 4. Update documentation

Update the instruction table in `internal/match/CLAUDE.local.md` to reflect new file names (4 files instead of 13 file-per-type listing).

## What Does NOT Change

- No type renames
- No interface changes
- No behavior changes
- No changes to `match.go`, `syntax_compiler.go`, `syntax_adapter.go`, or any test files
- The `SyntaxCommand` interface stays in `syntax_compiler.go`

## Risk

Minimal — purely organizational. All types, methods, and imports are preserved verbatim. The only change is which `.go` file each type lives in, which is invisible to Go's type system.

## Net Effect

13 files → 4 files. Removes ~130 lines of duplicated copyright headers/package declarations. No functional change.
