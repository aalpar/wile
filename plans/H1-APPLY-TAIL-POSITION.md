# H1: apply in Tail Position Grows Go Stack

**Source:** `plans/R7RS-CONFORMANCE-REVIEW.md` (H1), `plans/R7RS-CONFORMANCE-FIXES.md` (excluded)
**Status:** Design approved, awaiting implementation plan

---

## Problem

R7RS §3.5 requires proper tail recursion: tail calls must run in constant stack space.
`apply` in tail position creates recursive Go stack frames via `PrimApply` →
`NewSubContext` → `ApplyCallable` → `Run()`. Each `Run()` is a new Go stack frame.

```scheme
(define (f n) (if (zero? n) 'done (apply f (list (- n 1)))))
(f 1000000)  ;; Go stack overflow at ~300K
```

**Root cause:** `PrimApply` (`registry/core/prim_control.go:28-89`) is a foreign function
that creates a sub-context and calls `Run()` for every invocation. The compiler treats
`(apply f args)` as a regular function call to the `apply` global binding — it has no
knowledge that apply should support tail calls.

## Solution

Make `apply` a compile-time special form (same dual-registration pattern as `dynamic-wind`).
The compiler recognizes direct `(apply ...)` calls and emits bytecode that flattens the arg
list onto the eval stack, then dispatches via the existing `OpApply`. No sub-context, no
`Run()`, no Go stack growth.

### Prior art in this codebase

- `dynamic-wind`: registered as both a compile-time binding (`specialforms.go`) and a
  runtime primitive (`PrimDynamicWind`). Direct calls compile to bytecode
  (`compileValidatedDynamicWind`); indirect calls use the runtime fallback.
- `PrimCallCC` inline mode (`prim_control.go:133-144`): demonstrates that foreign functions
  can configure VM state (template/env/pc) and return to the VM loop without Go stack
  growth. `applyForeign` detects template changes and skips continuation restore.
- `map`/`for-each`: converted from Go to Scheme (`bootstrap.go`) to make iteration frames
  visible to the continuation system. Same class of problem (Go frames blocking Scheme
  semantics).

## Architecture

### Dual registration

| Path | When | Mechanism |
|------|------|-----------|
| Compile-time | `(apply f args)` as direct call | Compiler emits `OpUnpackListToStack` + `OpApply` |
| Runtime | `apply` used as first-class value | `PrimApply` ForeignClosure (unchanged) |

### New opcode: OpUnpackListToStack

Takes a proper list from the value register. Iterates it, pushing each element to the eval
stack in order. Errors on improper list or non-list.

Single-purpose, composable. No arg — it always reads from the value register and pushes
to the eval stack.

### Compilation: (apply f a b finalList)

R7RS §6.10: `(apply proc arg1 ... args)` — `proc` is a procedure, `arg1 ...` are zero or
more prefix args, `args` is the final list. The actual arguments are the prefix args
concatenated with the elements of `args`.

**Non-tail position:**
```
SaveContinuation →after
<compile f>             PUSH        ; stack: [f]
<compile a>             PUSH        ; stack: [f, a]
<compile b>             PUSH        ; stack: [f, a, b]
<compile finalList>                 ; value = finalList
OpUnpackListToStack                 ; stack: [f, a, b, x1, x2, ...]
Pull                                ; value = f
OpApply                             ; calls f(a, b, x1, x2, ...)
after:
```

**Tail position:** Same without SaveContinuation/patch. The callee's RestoreContinuation
returns to caller's caller. Proper tail call for all callable types.

This is identical to a regular function call with `OpUnpackListToStack` inserted before
`Pull` to flatten the list onto the stack.

### Expander integration

A primitive expander for `apply` that expands each sub-expression (proc, prefix args,
final list). Same pattern as the `dynamic-wind` primitive expander.

### Validator integration

A `ValidatedApply` node with:
- `Proc` — procedure expression
- `PrefixArgs` — zero or more prefix argument expressions
- `FinalList` — final list expression

Follows the Tier 1 validated form pattern (like `ValidatedDynamicWind`).

### Runtime fallback

`PrimApply` remains unchanged for indirect calls:

```scheme
(define a apply)
(a + '(1 2))  ; uses PrimApply ForeignClosure
```

Indirect recursive tail-apply is not a realistic pattern. The sub-context overhead for
these rare calls is acceptable.

## Deferred: static list elimination

When the final arg is `(list expr1 ... exprN)` with `list` resolving to the built-in
binding, the compiler could rewrite:

```scheme
(apply f a (list b c))  →  compiles as  →  (f a b c)
```

Eliminates list allocation, iteration, and OpUnpackListToStack entirely. Meaningful win
for macro-generated code and CPS patterns. Deferred to a follow-up — pure optimization,
not correctness.

## Testing strategy

| Test | What it verifies |
|------|-----------------|
| `(apply f (list (- n 1)))` with n=1000000 | No Go stack overflow (H1 fix) |
| `(apply + '(1 2 3))` | Correctness with foreign closures |
| `(apply (case-lambda ((x) x) ((x y) (+ x y))) '(1 2))` | CaseLambdaClosure dispatch |
| `(apply f 1 2 (list 3 4))` | Prefix args + final list |
| `(apply f '())` | Empty final list |
| `(+ 1 (apply f '(2)))` | Non-tail position |
| `(call/cc (lambda (k) (apply k '(42))))` | Continuation inside apply |
| `(dynamic-wind before (lambda () (apply f '(x))) after)` | Winding context |
| `(define a apply) (a + '(1 2))` | Runtime fallback (first-class use) |
| `(apply apply (list + '(1 2)))` | Nested apply |

## Files affected

| File | Change |
|------|--------|
| `registry/core/specialforms.go` | Add `apply` compile-time binding |
| `machine/primitive_expanders_registry.go` | Add `apply` primitive expander |
| `internal/validate/` | Add `ValidatedApply` node |
| `internal/forms/` | Register apply form |
| `machine/compile_validated.go` | Add `compileValidatedApply` |
| `machine/operation_unpack_list_to_stack.go` | New opcode |
| `machine/operation.go` or `machine/operations.go` | Register new opcode constant |
| `machine/machine_context.go` | Handle new opcode in `Run()` loop |
| `registry/core/prim_control_test.go` | Tail recursion depth test |
| `machine/compile_validated_test.go` | Compilation tests |
| `machine/operation_test.go` | Opcode unit tests |
