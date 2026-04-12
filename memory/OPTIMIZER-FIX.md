# Peephole Optimizer Fix: CallForeignCached + call/cc Double-Restore

**Status:** Complete
**Priority:** High (was: optimizer disabled, TestOpcodeFusion skipped)
**Branch:** feat/stdlib-embed-install

---

## Bug Description

`callForeignCached` in non-tail mode unconditionally calls
`mc.RestoreAndRelease(mc.cont)` after the foreign function returns (when
the template pointer is unchanged). But if the foreign function internally
calls `mc.ApplyCallable` with a `*ForeignClosure`, `applyForeign` consumes
the `SaveContinuation` frame from `mc.cont`. Then `callForeignCached` tries
to restore from the now-consumed frame — a **double-restore** that corrupts
the continuation chain (or panics on nil `mc.cont`).

### Concrete Reproducer

```scheme
;; call/cc fused to CallForeignCached in non-tail position:
(list (call-with-current-continuation procedure?))
;; => should return (#t), but crashes with nil mc.cont
```

**Trace:**

1. Optimizer fuses `(call/cc procedure?)` to non-tail `CallForeignCached(call/cc)`
2. `SaveContinuation` pushes frame F onto `mc.cont`
3. `callForeignCached` calls `PrimCallCC(mc)`:
   - `mc.SliceContinuationAt(nil)` deep-copies continuation chain
   - `mc.Parent()` returns `mc.cont` = F (non-nil) → **inline mode**
   - `mc.ApplyCallable(procedure?, capturedK)`:
     - `procedure?` is a `*ForeignClosure` → dispatches to `applyForeign`
     - `applyForeign` runs PrimProcedureQ, sets value to `#t`
     - `applyForeign`: `mc.template == savedTemplate` → `mc.RestoreAndRelease(mc.cont)`
     - **Frame F is consumed**. `mc.cont` = F.parent
4. PrimCallCC returns nil (no error)
5. `callForeignCached`: `mc.template == savedTemplate` (restored by step 3)
6. `callForeignCached`: `mc.RestoreAndRelease(mc.cont)` → **double-restore**
   - If F.parent is nil → nil pointer panic
   - If F.parent exists → wrong frame restored, execution corrupted

### Why the Unfused Path Works

In the unfused path, `OperationForeignFunctionCall.Apply` (the `OpComplex`
side-table handler) doesn't do its own restore. It just increments `mc.pc`
(line 98 of `operations_call.go`). The bytecode `RestoreContinuation` opcode
handles the restore as a separate VM step. This means there's no risk of
double-restore — the restore happens exactly once, as the next instruction.

### Why `applyForeign` Doesn't Have This Bug

`applyForeign` (line 144 of `machine_context_apply.go`) guards the restore:

```go
if p.cont != nil {
    p.RestoreAndRelease(p.cont)
} else {
    p.template = immediateReturnTemplate
    p.pc = 0
}
```

The nil-guard prevents crashes, though it doesn't prevent incorrect
behavior if `mc.cont` was changed to a different (non-nil) frame. The
pointer-identity check proposed below is strictly stronger.

---

## Fix

### Phase 1: Fix `callForeignCached` (core bug)

**File:** `machine/call_foreign_cached.go`

Save `mc.cont` before calling the foreign function. After the call, only
restore if `mc.cont` is still the expected frame (pointer identity check):

```go
func callForeignCached(mc *MachineContext, instr Instruction, tail bool) (*MachineContext, error) {
    // ... (arity check, env setup unchanged) ...

    savedTemplate := mc.template
    savedCont := mc.cont  // NEW: save expected continuation frame
    err = fcls.fn(mc)
    if err != nil {
        return nil, applyCallableError(mc, err)
    }

    if mc.template != savedTemplate {
        return mc, nil
    }

    if tail {
        mc = mc.returnImmediate()
    } else {
        // Only restore if the continuation wasn't already consumed by
        // the foreign function (e.g., PrimCallCC inline mode calling a
        // ForeignClosure via applyForeign, which does its own restore).
        if mc.cont == savedCont {
            mc.RestoreAndRelease(mc.cont)
        }
    }
    return mc, nil
}
```

**Why pointer identity is correct:**

| Scenario | mc.cont after fn | savedCont | Match? | Action |
|----------|-----------------|-----------|--------|--------|
| Normal foreign function | F (unchanged) | F | Yes | Restore (correct) |
| PrimCallCC inline + MachineClosure | F (unchanged*) | F | Yes | But template check returns early — never reaches this |
| PrimCallCC inline + ForeignClosure | F.parent (consumed) | F | No | Skip restore (correct) |
| PrimCallCC sub-context mode | F (unchanged) | F | Yes | Restore (correct) |

*In the MachineClosure case, `mc.Apply` changes `mc.template`, so the
template check catches it before the cont check is reached.

### Phase 2: Add regression test

**File:** `machine/call_foreign_cached_test.go` (new, internal test)

Create a minimal test that reproduces the double-restore via
`callForeignCached` + `PrimCallCC` inline mode + `ForeignClosure` argument.
This is the exact scenario from the trace above.

### Phase 3: Re-enable optimizer and fusion tests

**Files:**
- `engine.go`: Remove the `// tpl.Optimize()` comment, re-enable the call
- `opcode_fusion_test.go`: Remove `t.Skip`

### Phase 4: Add integration-level call/cc + fused-call tests

**File:** `opcode_fusion_test.go` or new `callcc_fusion_test.go`

Test cases that exercise call/cc through fused code paths:

```scheme
;; Non-tail call/cc with ForeignClosure arg (the exact bug)
(call-with-current-continuation procedure?)  ; => #t

;; call/cc returning a value through fused non-tail position
(+ 1 (call-with-current-continuation (lambda (k) (k 2))))  ; => 3

;; call/cc escape through fused call chain
(let ((r (call-with-current-continuation (lambda (k) k))))
  (if (procedure? r) (r 42) r))  ; => 42

;; Nested fused calls with call/cc argument
(string-length (call-with-current-continuation (lambda (k) (k "hello"))))  ; => 5
```

---

## Scope and Constraints

- **Promoted opcodes are NOT affected.** They delete `SaveContinuation`
  entirely and use inline Go functions (no `ApplyCallable`). No
  continuation is saved, so no double-restore is possible.

- **Tail `CallForeignCachedTail` is NOT affected.** Tail calls use
  `returnImmediate()` (not `RestoreAndRelease`), so there's no
  SaveContinuation frame to double-consume.

- **`callForeignCachedReassigned` is NOT affected.** It delegates to
  `mc.ApplyCallable` which handles continuation management internally.

- **Library and syntax-rules `Optimize()` calls are NOT affected.** They
  also call `tpl.Optimize()` (`compile_library_forms.go:116`,
  `compile_syntax_rules.go:652`, `compile_closure.go:111`). These compile
  library/macro/lambda bodies. The bug only manifests at runtime when
  `callForeignCached` runs — the optimizer itself is correct. However,
  these sites currently run the optimizer but the engine doesn't, creating
  an inconsistency. Investigate whether library/closure templates can
  trigger the bug (likely not — they're sub-templates executed within the
  same VM loop).

---

## Verification

1. `go test -run TestCallForeignCachedCallCC ./machine/` — Phase 2 test
2. `go test -run TestOpcodeFusion ./` — Phase 3 re-enabled tests
3. `go test -timeout 120s ./...` — full suite
4. `make lint` — no regressions
5. `make bench-gabriel` — verify no performance regression from the
   pointer comparison (should be negligible: one pointer save + compare
   per non-tail fused call)
