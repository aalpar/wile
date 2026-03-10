# MachineContext Decomposition

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Status:** Complete

**Goal:** Split the 1,639-line `machine/machine_context.go` into 5 files by responsibility, improving navigability without changing behavior.

**Architecture:** Pure file split within the `machine` package. All methods stay on `*MachineContext`. No new types, no import changes, no behavior changes. Each extracted file gets the copyright header and only the imports it needs. Test file (`machine_context_test.go`) stays as-is.

**Prior art:** The expander decomposition (PR #444) split `expander_time_continuation.go` into `expander_body.go`, `expander_lambda.go`, `expander_let_syntax.go`, `expander_primitive_forms.go`. Same pattern here with `machine_context_` prefix.

---

## File Plan

After decomposition:

| File | Responsibility | ~Lines | Key symbols |
|------|---------------|--------|-------------|
| `machine_context.go` | Struct, constructors, accessors, `Run()`, `RunWithEscapeHandling()`, resolve/error helpers, thread/counter accessors | ~880 | `MachineContext`, `NewMachineContext`, `Run`, `RunWithEscapeHandling` |
| `machine_context_continuation.go` | Continuation save/restore, delimited continuation chain ops | ~252 | `Restore`, `RestoreAndRelease`, `PopContinuation`, `SaveContinuation`, `CurrentContinuation`, `CallDepth`, `FindPrompt`, `SliceContinuationAt`, `GraftContinuation`, `SetPromptTag`, `PromptTag` |
| `machine_context_winding.go` | Dynamic-wind stack management, unwind/rewind | ~132 | `WindingStack`, `SetWindingStack`, `PushWindingFrame`, `PopWindingFrame`, `UnwindTo`, `unwindStackTo`, `RewindTo`, `RestoreWithWinding`, `RestoreWithWindingFrom` |
| `machine_context_subcontext.go` | Sub-context creation for foreign calls and threads | ~82 | `NewSubContext`, `SubContextParams`, `CaptureSubContextParams`, `NewThreadSubContext` |
| `machine_context_apply.go` | Callable dispatch (closures, foreign, case-lambda, parameters, composable continuations) | ~293 | `Apply`, `applyForeign`, `buildRestArg`, `ApplyCaseLambda`, `ApplyCallable`, `returnImmediate`, `applyParameter`, `applyComposableContinuation` |

## Cross-Group Dependencies

Methods call across groups — this is expected and fine (same package):

- `Run()` calls `SaveContinuation`, `RestoreAndRelease`, `ApplyCallable` (core → continuation, core → apply)
- `RunWithEscapeHandling()` calls `FindPrompt`, `Restore`, `RestoreWithWindingFrom`, `UnwindTo`, `ApplyCallable` (core → all groups)
- `applyForeign` calls `RestoreAndRelease` (apply → continuation)
- `returnImmediate` calls `RestoreAndRelease` (apply → continuation)
- `applyComposableContinuation` calls `RestoreWithWindingFrom`, `GraftContinuation`, `Restore` (apply → winding, continuation)
- `applyParameter` calls `NewSubContext` (apply → subcontext)
- `unwindStackTo`, `RewindTo` call `NewSubContext`, `ApplyCallable` (winding → subcontext, apply)

All within the same package — no import issues.

## Constraints

- **No behavior changes.** This is a file split only.
- **No test file splitting.** `machine_context_test.go` stays as-is.
- **Each phase must pass `make lint && make test`.** Run after each extraction.
- **Each phase is one commit.** Squash-friendly for PR review.
- **Import minimization.** Each new file gets only the imports its methods use.
- **Copyright header.** Every new file gets the Apache 2.0 header.

---

## Task 1: Extract continuation mechanics

**Files:**
- Create: `machine/machine_context_continuation.go`
- Modify: `machine/machine_context.go` (remove extracted methods)

**Step 1: Create `machine_context_continuation.go`**

Move these symbols (in source order):

| Symbol | Current lines | Type |
|--------|--------------|------|
| `Restore` | 224–254 | method |
| `RestoreAndRelease` | 256–339 | method (include preceding comment block starting at 256) |
| `PopContinuation` | 341–374 | method (include preceding comment block starting at 341) |
| `SaveContinuation` | 376–412 | method (include preceding comment block starting at 376) |
| `CurrentContinuation` | 414–417 | method |
| `CallDepth` | 419–422 | method |
| `FindPrompt` | 1453–1463 | method |
| `SliceContinuationAt` | 1465–1486 | method |
| `GraftContinuation` | 1488–1499 | free function |
| `SetPromptTag` | 1501–1505 | method |
| `PromptTag` | 1507–1510 | method |

Imports needed: `"github.com/aalpar/wile/werr"` (for `WrapForeignErrorf` in `PopContinuation`, `SaveContinuation`).

No standard library imports needed — `Restore`/`RestoreAndRelease` reference `restoreInlineEvals`, `releaseStack`, `releaseEnvFrame`, `releaseContinuation`, `acquireStack`, `acquireSubContext`, `inlineEvalsCap`, `NewMachineContinuationFromMachineContext` — all in-package symbols (no import needed).

**Step 2: Remove the moved symbols from `machine_context.go`**

Delete lines 224–422 and lines 1453–1510 from the original file. Adjust nothing else — the remaining code references these methods by name, which works within the same package.

**Step 3: Run `goimports -w` on both files**

This fixes import blocks automatically.

**Step 4: Verify**

```bash
make lint && go test -count=1 ./machine/...
```

Expected: all pass, no behavior change.

**Step 5: Commit**

```
refactor(machine): extract continuation mechanics from machine_context.go

Move Restore, RestoreAndRelease, PopContinuation, SaveContinuation,
CurrentContinuation, CallDepth, FindPrompt, SliceContinuationAt,
GraftContinuation, SetPromptTag, PromptTag into
machine_context_continuation.go.

Part of MachineContext decomposition (F10 in TODO.md).
```

---

## Task 2: Extract winding/dynamic-wind

**Files:**
- Create: `machine/machine_context_winding.go`
- Modify: `machine/machine_context.go` (remove extracted methods)

**Step 1: Create `machine_context_winding.go`**

Move these symbols (in source order from the original file, post-Task-1 line numbers will differ — use symbol names):

| Symbol | Type |
|--------|------|
| `WindingStack` | method |
| `SetWindingStack` | method |
| `PushWindingFrame` | method |
| `PopWindingFrame` | method |
| `UnwindTo` | method |
| `unwindStackTo` | method |
| `RewindTo` | method |
| `RestoreWithWinding` | method |
| `RestoreWithWindingFrom` | method |

Imports needed: none from outside the package. `unwindStackTo` and `RewindTo` call `p.NewSubContext()`, `ReleaseSubContext()`, `p.ApplyCallable()` — all in-package. `RestoreWithWindingFrom` calls `FindCommonWindingPrefix` (in `dynamic_wind.go`) and `p.Restore` (now in `machine_context_continuation.go`) — both in-package.

**Step 2: Remove the moved symbols from `machine_context.go`**

**Step 3: Run `goimports -w` on both files**

**Step 4: Verify**

```bash
make lint && go test -count=1 ./machine/...
```

**Step 5: Commit**

```
refactor(machine): extract winding mechanics from machine_context.go

Move WindingStack, SetWindingStack, PushWindingFrame, PopWindingFrame,
UnwindTo, unwindStackTo, RewindTo, RestoreWithWinding,
RestoreWithWindingFrom into machine_context_winding.go.

Part of MachineContext decomposition (F10 in TODO.md).
```

---

## Task 3: Extract sub-context creation

**Files:**
- Create: `machine/machine_context_subcontext.go`
- Modify: `machine/machine_context.go` (remove extracted symbols)

**Step 1: Create `machine_context_subcontext.go`**

Move these symbols:

| Symbol | Type |
|--------|------|
| `SubContextParams` | struct (include comment) |
| `NewSubContext` | method (include the large comment block above it, lines 1035–1048 in original) |
| `CaptureSubContextParams` | method |
| `NewThreadSubContext` | free function |

Imports needed:
- `"context"` (used in `SubContextParams.Ctx`)
- `"github.com/aalpar/wile/environment"` (used in `SubContextParams.Env`)
- `"github.com/aalpar/wile/values"` (used in `NewThreadSubContext` parameter `*values.Thread`)

**Step 2: Remove the moved symbols from `machine_context.go`**

**Step 3: Run `goimports -w` on both files**

**Step 4: Verify**

```bash
make lint && go test -count=1 ./machine/...
```

**Step 5: Commit**

```
refactor(machine): extract sub-context creation from machine_context.go

Move NewSubContext, SubContextParams, CaptureSubContextParams,
NewThreadSubContext into machine_context_subcontext.go.

Part of MachineContext decomposition (F10 in TODO.md).
```

---

## Task 4: Extract apply dispatch

**Files:**
- Create: `machine/machine_context_apply.go`
- Modify: `machine/machine_context.go` (remove extracted methods)

**Step 1: Create `machine_context_apply.go`**

Move these symbols (in source order):

| Symbol | Type |
|--------|------|
| `Apply` | method |
| `applyForeign` | method |
| `buildRestArg` | method |
| `ApplyCaseLambda` | method |
| `ApplyCallable` | method |
| `returnImmediate` | method |
| `applyParameter` | method |
| `applyComposableContinuation` | method |

Imports needed:
- `"errors"` (used in `applyForeign` for `errors.As`)
- `"fmt"` (used in `applyParameter`, `applyComposableContinuation`)
- `"github.com/aalpar/wile/environment"` (used in `Apply` for `environment.EnvironmentFrame`, `environment.Binding`)
- `"github.com/aalpar/wile/values"` (used in `ApplyCallable`, `applyParameter`, etc.)
- `"github.com/aalpar/wile/werr"` (used in `Apply`, `applyForeign`, `ApplyCaseLambda`, `ApplyCallable`, `applyComposableContinuation`)

In-package references (no import needed): `checkArity`, `bindArgs`, `acquireEnvFrame`, `ForeignClosure`, `CaseLambdaClosure`, `Parameter`, `ComposableContinuation`, `MachineClosure`, `immediateReturnTemplate`, `goErrorToSchemeException`, `ErrPromptAbort`, `ErrExceptionEscape`, `GraftContinuation`, `ReleaseSubContext`.

**Step 2: Remove the moved symbols from `machine_context.go`**

After this extraction, `machine_context.go` should no longer need `"github.com/aalpar/wile/environment"` — verify by checking remaining references. The `Run()` method calls `resolveGlobalBinding`/`resolveLocalBinding` which use `environment.Binding` and `environment.GlobalIndex`, so the import stays.

**Step 3: Run `goimports -w` on both files**

**Step 4: Verify**

```bash
make lint && go test -count=1 ./machine/...
```

**Step 5: Commit**

```
refactor(machine): extract apply dispatch from machine_context.go

Move Apply, applyForeign, buildRestArg, ApplyCaseLambda, ApplyCallable,
returnImmediate, applyParameter, applyComposableContinuation into
machine_context_apply.go.

Part of MachineContext decomposition (F10 in TODO.md).
```

---

## Task 5: Final verification and TODO update

**Step 1: Full verification**

```bash
make lint && make covercheck
```

**Step 2: Verify line counts**

```bash
wc -l machine/machine_context.go machine/machine_context_continuation.go machine/machine_context_winding.go machine/machine_context_subcontext.go machine/machine_context_apply.go
```

Expected approximate totals:
- `machine_context.go`: ~880 lines
- `machine_context_continuation.go`: ~270 lines (content + header + imports)
- `machine_context_winding.go`: ~150 lines
- `machine_context_subcontext.go`: ~100 lines
- `machine_context_apply.go`: ~320 lines

**Step 3: Update TODO.md**

Mark F10 as complete:
```
- [x] **F10: MachineContext decomposition** [Medium, Postponed]: ...
```

Add the commit details.

**Step 4: Update `machine/CLAUDE.local.md`**

In the "VM Runtime" table, update the `machine_context.go` entry and add 4 new entries for the extracted files.

**Step 5: Commit**

```
docs: mark MachineContext decomposition complete in TODO.md
```
