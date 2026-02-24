# Engine.go Refactoring Plan

## Problem

`engine.go` has four structural issues:

1. **Hand-unrolled call methods** — `callClosure` and `callCaseLambda` are identical except for the Apply call
2. **Duplicated env setup** — main init (lines 109-130) and library env factory (lines 192-211) repeat the same 4-step sequence
3. **Duplicated compile pipeline** — `compileExpr` and `runBootstrapMacroStx` share expand+compile+optimize
4. **175-line NewEngine** — mixed concerns: registry building, env setup, library config, closer collection

## Changes

### 1. `callCallable` replaces `callClosure` + `callCaseLambda`

Single method using `sub.ApplyCallable(callable, args...)` (already exists on MachineContext).

`Call()` special-cases only `Parameter` (0-arg doesn't need VM) and `ComposableContinuation` (can't call from Go). Everything else goes through `callCallable`.

`callParameter`'s converter call changes from `callClosure` → `callCallable`.

### 2. `applyBaseEnvironment` extracts shared env setup

```
func applyBaseEnvironment(ctx, env, reg, macroSources) error
```

Four steps: `reg.Apply` → `RegisterSyntaxCompilers` → `RegisterPrimitiveExpanders` → `loadBootstrapMacros`.

Called by NewEngine and library env factory. Callers own error wrapping.

### 3. `expandAndCompile` extracts shared compile pipeline

```
func expandAndCompile(ctx, env, stx) (*machine.NativeTemplate, error)
```

Expand → compile → optimize. Used by `compileExpr` (wraps in CompilationError) and `runBootstrapMacroStx` (also runs result).

Collapses "expansion error" / "compilation error" distinction — underlying error already carries phase info.

### 4. `registerExtensionLibraries` extracted from NewEngine

The 35-line extension→library registration loop (lines 146-180) becomes a standalone function.

Combined with #2, the library setup block in NewEngine shrinks from ~80 to ~20 lines.

## Execution Order

1. Extract `expandAndCompile` (#3) — no callers change signature
2. Extract `applyBaseEnvironment` (#2) — simplifies NewEngine + factory
3. Unify `callCallable` (#1) — self-contained
4. Extract `registerExtensionLibraries` (#4) — final cleanup

## Expected Result

~60 lines removed, three duplications eliminated, NewEngine cut roughly in half.
