# machine/ Package Decomposition Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Split `machine/` into `machine/` (types + VM runtime) and `machine/compilation/` (compiler + expander + library system) with interface boundaries for reusability.

**Architecture:** Two interfaces break dependency cycles — `ExpanderCtx` (in `machine/`) prevents the runtime from importing compilation types, `MacroEvaluator` (in `compilation/`) abstracts the VM so consumers can substitute a different backend. All shared types stay in `machine/` root.

**Tech Stack:** Go 1.24, no new dependencies. Internal refactoring only.

**Design doc:** `plans/2026-03-30-machine-decomposition-design.md`

---

## Phase 1: Interface Introduction (within machine/, no file moves)

Phase 1 validates the interface design by introducing both interfaces within the existing
single package. All tests must continue to pass. No file moves, no external impact.

### Task 1: Define ExpanderCtx interface

**Files:**
- Create: `machine/expander_ctx.go`
- Modify: `machine/expander_context.go`

**Step 1: Write the interface**

Create `machine/expander_ctx.go`:

```go
package machine

import (
    "github.com/aalpar/wile/environment"
    "github.com/aalpar/wile/internal/syntax"
)

// ExpanderCtx abstracts the expander context stored on MachineContext during
// macro expansion. This interface enables the compilation sub-package to
// provide the concrete ExpanderContext without creating a circular import.
//
// Consumers (e.g., syntax-local-* primitives in internal/extensions/eval/)
// access these methods through mc.ExpanderContext().
type ExpanderCtx interface {
    Env() *environment.EnvironmentFrame
    Expand(syntax.SyntaxValue) (syntax.SyntaxValue, error)
    ExpandOnce(syntax.SyntaxValue) (syntax.SyntaxValue, bool, error)
    IntroductionScope() *syntax.Scope
    SetIntroductionScope(*syntax.Scope)
    UseSiteScope() *syntax.Scope
    SetUseSiteScope(*syntax.Scope)
}
```

**Step 2: Verify ExpanderContext already satisfies the interface**

Check that every method in the interface exists on `*ExpanderContext` in
`machine/expander_context.go`. The existing methods match exactly — no changes
needed to `expander_context.go`.

**Step 3: Write a compile-time interface check**

Add to `machine/expander_ctx.go`:

```go
// Compile-time check: *ExpanderContext implements ExpanderCtx.
var _ ExpanderCtx = (*ExpanderContext)(nil)
```

**Step 4: Run tests**

Run: `go build ./machine/ && make test`
Expected: PASS (interface is additive, no behavioral change)

**Step 5: Commit**

```
feat: define ExpanderCtx interface in machine/

Preparation for machine/ package decomposition. The interface abstracts
the ExpanderContext so MachineContext can store it without importing the
future compilation/ sub-package.
```

---

### Task 2: Switch MachineContext to use ExpanderCtx interface

**Files:**
- Modify: `machine/machine_context.go` (field type + getter/setter)

**Step 1: Change the field type**

In `machine/machine_context.go`, find the `expanderCtx` field on `MachineContext`
(line ~67):

```go
// Before:
expanderCtx      *ExpanderContext

// After:
expanderCtx      ExpanderCtx
```

**Step 2: Update SetExpanderContext signature**

Find `SetExpanderContext` (line ~821):

```go
// Before:
func (p *MachineContext) SetExpanderContext(ctx *ExpanderContext) {

// After:
func (p *MachineContext) SetExpanderContext(ctx ExpanderCtx) {
```

**Step 3: Update ExpanderContext() return type**

Find `ExpanderContext()` (line ~828):

```go
// Before:
func (p *MachineContext) ExpanderContext() *ExpanderContext {

// After:
func (p *MachineContext) ExpanderContext() ExpanderCtx {
```

**Step 4: Search for all callers of SetExpanderContext**

Run: `grep -rn 'SetExpanderContext\|\.ExpanderContext()' --include='*.go'`

Callers of `SetExpanderContext` pass `*ExpanderContext` — this satisfies
`ExpanderCtx` implicitly, so no caller changes are needed.

Callers of `.ExpanderContext()` receive the interface. Check that they only
call methods defined on `ExpanderCtx` (Env, Expand, ExpandOnce,
IntroductionScope, SetIntroductionScope, UseSiteScope, SetUseSiteScope).

Key file to check: `internal/extensions/eval/prim_eval.go` — uses
`expanderCtx.Env()`, `expanderCtx.Expand()`, `expanderCtx.ExpandOnce()`,
`expanderCtx.IntroductionScope()`, `expanderCtx.UseSiteScope()`. All are
on the interface.

Also check: `machine/expander_time_continuation.go` — if any caller
type-asserts back to `*ExpanderContext`, it will need updating. Search for
`\.ExpanderContext()\.` patterns that call methods NOT on the interface.

**Step 5: Run tests**

Run: `make test`
Expected: PASS

**Step 6: Commit**

```
refactor: MachineContext uses ExpanderCtx interface

Change expanderCtx field from *ExpanderContext to the ExpanderCtx interface.
No behavioral change — the concrete type still satisfies the interface.
```

---

### Task 3: Define MacroEvaluator interface

**Files:**
- Create: `machine/macro_evaluator.go`

**Step 1: Write the interface and default implementation**

Create `machine/macro_evaluator.go`:

```go
package machine

import (
    "context"

    "github.com/aalpar/wile/environment"
    "github.com/aalpar/wile/internal/syntax"
    "github.com/aalpar/wile/values"
    "github.com/aalpar/wile/werr"
)

// MacroEvaluator abstracts VM execution for the compiler and expander.
// The compilation pipeline uses this interface to evaluate transformer
// expressions at compile time and invoke macro transformers during expansion,
// without a direct dependency on MachineContext.Run().
//
// The default implementation (NewVMMacroEvaluator) delegates to the real VM.
// Consumers who import the compiler without the VM can provide a stub
// implementation (e.g., one that rejects all macro evaluation).
type MacroEvaluator interface {
    // EvalTemplate evaluates a compiled template in the given environment
    // and returns the resulting value. Used by compileAndEvalLambdaTransformer
    // to evaluate define-syntax lambda transformers at compile time.
    EvalTemplate(ctx context.Context, tpl *NativeTemplate, env *environment.EnvironmentFrame) (values.Value, error)

    // InvokeTransformer calls a closure as a macro transformer with the given
    // input form. expanderCtx is set on the VM context for auxiliary syntax
    // hygiene (R7RS Section 4.3.2). Used during macro expansion.
    InvokeTransformer(ctx context.Context, cls Closure, input syntax.SyntaxValue, expanderCtx ExpanderCtx) (*MachineContext, error)
}

// vmMacroEvaluator is the default MacroEvaluator using the real VM.
type vmMacroEvaluator struct{}

// NewVMMacroEvaluator returns a MacroEvaluator backed by the real VM.
func NewVMMacroEvaluator() MacroEvaluator {
    return &vmMacroEvaluator{}
}

func (p *vmMacroEvaluator) EvalTemplate(ctx context.Context, tpl *NativeTemplate, env *environment.EnvironmentFrame) (values.Value, error) {
    cont := NewMachineContinuation(nil, tpl, env)
    mc := NewMachineContext(ctx, cont)
    err := mc.Run()
    if err != nil {
        return nil, err
    }
    return mc.GetValue(), nil
}

func (p *vmMacroEvaluator) InvokeTransformer(ctx context.Context, cls Closure, input syntax.SyntaxValue, expanderCtx ExpanderCtx) (*MachineContext, error) {
    return invokeTransformerClosure(ctx, cls, input, expanderCtx)
}
```

**Important:** `InvokeTransformer` returns `*MachineContext` because callers
read the value register and then release the sub-context. This matches the
existing `invokeTransformerClosure` contract.

Note: `invokeTransformerClosure` currently takes `*ExpanderContext`. Its
signature must be updated to take `ExpanderCtx` (the interface) to match.
Do that in Task 4.

**Step 2: Run build**

Run: `go build ./machine/`
Expected: PASS (interface is additive)

**Step 3: Commit**

```
feat: define MacroEvaluator interface in machine/

Abstracts VM execution for the compiler and expander. The default
vmMacroEvaluator delegates to the real VM. Enables future consumers
to provide a stub implementation for compiler-only usage.
```

---

### Task 4: Update invokeTransformerClosure to accept ExpanderCtx

**Files:**
- Modify: `machine/expander_time_continuation.go`

**Step 1: Change invokeTransformerClosure signature**

In `machine/expander_time_continuation.go` (line ~269):

```go
// Before:
func invokeTransformerClosure(ctx context.Context, cls Closure, inputForm syntax.SyntaxValue, expanderCtx *ExpanderContext) (*MachineContext, error) {

// After:
func invokeTransformerClosure(ctx context.Context, cls Closure, inputForm syntax.SyntaxValue, expanderCtx ExpanderCtx) (*MachineContext, error) {
```

The body calls `mc.SetExpanderContext(expanderCtx)` which now accepts
`ExpanderCtx` (changed in Task 2). No other body changes needed.

**Step 2: Verify all callers pass compatible types**

Run: `grep -n 'invokeTransformerClosure' machine/*.go`

Callers pass `*ExpanderContext` which satisfies `ExpanderCtx`. No changes needed.

**Step 3: Run tests**

Run: `make test`
Expected: PASS

**Step 4: Commit**

```
refactor: invokeTransformerClosure accepts ExpanderCtx interface
```

---

### Task 5: Thread MacroEvaluator into ExpanderTimeContinuation

**Files:**
- Modify: `machine/expander_time_continuation.go` (struct + constructor + call sites)

**Step 1: Add MacroEvaluator field to ExpanderTimeContinuation**

```go
type ExpanderTimeContinuation struct {
    ctx          context.Context
    env          *environment.EnvironmentFrame
    libraryScope *syntax.Scope
    evaluator    MacroEvaluator  // NEW: abstracts VM calls for macro expansion
}
```

**Step 2: Update NewExpanderTimeContinuation**

```go
func NewExpanderTimeContinuation(ctx context.Context, env *environment.EnvironmentFrame, evaluator MacroEvaluator) *ExpanderTimeContinuation {
    q := &ExpanderTimeContinuation{
        ctx:       ctx,
        env:       env,
        evaluator: evaluator,
    }
    return q
}
```

**Step 3: Replace direct VM call in expandMacroInvocation**

Find the call to `invokeTransformerClosure` inside `expandMacroInvocation`
(or wherever the expander calls it). Replace with:

```go
// Before:
mc, err := invokeTransformerClosure(p.ctx, cls, expr, expanderCtx)

// After:
mc, err := p.evaluator.InvokeTransformer(p.ctx, cls, expr, expanderCtx)
```

Search for ALL calls to `invokeTransformerClosure` within `expander_*.go`
files and replace them with `p.evaluator.InvokeTransformer(...)`.

**Step 4: Fix all callers of NewExpanderTimeContinuation**

Run: `grep -rn 'NewExpanderTimeContinuation' --include='*.go'`

Every call site must now pass a `MacroEvaluator`. For Phase 1, all callers
are within `machine/` — pass a package-level default or thread it through.

The callers are in:
- `compile_transformer.go` — `NewExpanderTimeContinuation(ctx, expandEnv)`
- `compile_time_continuation_library.go` — `NewExpanderTimeContinuation(ctctx.ctx, p.env)`
- `compile_cond_expand.go` — `NewExpanderTimeContinuation(ctctx.ctx, p.env)`
- `compile_define_for_syntax.go`
- `compile_syntax_case.go`
- `compile_time_continuation_include.go`
- `compile_helpers.go`
- `compile_eval_when.go`

All of these are in `CompileTimeContinuation` methods, so the compiler needs
a `MacroEvaluator` field too (Task 6). For now, add the field to the compiler
and thread it through.

**Step 5: Run tests**

Run: `make test`
Expected: FAIL (callers not yet updated — proceed to Task 6)

---

### Task 6: Thread MacroEvaluator into CompileTimeContinuation

**Files:**
- Modify: `machine/compile_time_continuation.go` (struct + constructor)
- Modify: `machine/compile_transformer.go` (use evaluator for EvalTemplate)
- Modify: All `compile_*.go` files that create ExpanderTimeContinuation

**Step 1: Add MacroEvaluator field to CompileTimeContinuation**

```go
type CompileTimeContinuation struct {
    env             *environment.EnvironmentFrame
    template        *NativeTemplate
    sourceStack     []*syntax.SourceContext
    libraryCallback func(*CompiledLibrary)
    libraryScope    *syntax.Scope
    fileResolver    FileResolver
    evaluator       MacroEvaluator  // NEW
}
```

**Step 2: Update NewCompiletimeContinuation**

```go
func NewCompiletimeContinuation(tpl *NativeTemplate, env *environment.EnvironmentFrame, evaluator MacroEvaluator) *CompileTimeContinuation {
```

Thread `evaluator` into the struct.

**Step 3: Update all compile_*.go files that create ExpanderTimeContinuation**

Every `NewExpanderTimeContinuation(ctx, env)` becomes
`NewExpanderTimeContinuation(ctx, env, p.evaluator)`.

There are ~8 call sites (listed in Task 5 Step 4). Update each one.

**Step 4: Update compileAndEvalLambdaTransformer to use evaluator**

In `machine/compile_transformer.go` (line ~84), the function currently calls
`NewMachineContext` and `mc.Run()` directly. Two changes:

a) The function needs access to the evaluator. It's called from
`compileTransformerToMachineClosure` which is called from expander and
compiler contexts. Thread the evaluator parameter through.

b) Replace the direct VM call:

```go
// Before:
cont := NewMachineContinuation(nil, tpl, expandEnv)
mc := NewMachineContext(ctx, cont)
err = mc.Run()
if err != nil {
    return nil, werr.WrapForeignErrorf(err, "error evaluating transformer")
}
result := mc.GetValue()

// After:
result, err := evaluator.EvalTemplate(ctx, tpl, expandEnv)
if err != nil {
    return nil, werr.WrapForeignErrorf(err, "error evaluating transformer")
}
```

Also update the cast below — `result` is now `values.Value`, cast to `*MachineClosure`.

**Step 5: Update compileTransformerToMachineClosure signature**

```go
// Before:
func compileTransformerToMachineClosure(ctx context.Context, env *environment.EnvironmentFrame, transformerExpr syntax.SyntaxValue, libraryScope *syntax.Scope) (values.Value, error) {

// After:
func compileTransformerToMachineClosure(ctx context.Context, env *environment.EnvironmentFrame, transformerExpr syntax.SyntaxValue, libraryScope *syntax.Scope, evaluator MacroEvaluator) (values.Value, error) {
```

Update all callers:
- `compile_define_syntax.go` — passes `p.evaluator`
- `expander_body.go` — needs evaluator threaded from ExpanderTimeContinuation
- `expander_let_syntax.go` — same

**Step 6: Fix all callers of NewCompiletimeContinuation**

Run: `grep -rn 'NewCompiletimeContinuation' --include='*.go'`

Callers outside `machine/` (in `engine.go`, `internal/bootstrap/`, `internal/repl/`)
must pass `NewVMMacroEvaluator()`. This is the one external-facing change in Phase 1.

**Step 7: Run tests**

Run: `make lint && make test`
Expected: PASS

**Step 8: Commit**

```
refactor: thread MacroEvaluator through compiler and expander

CompileTimeContinuation and ExpanderTimeContinuation now receive a
MacroEvaluator for VM calls. All existing code passes NewVMMacroEvaluator().
No behavioral change — preparation for machine/ decomposition Phase 2.
```

---

### Task 7: Verify and clean up

**Step 1: Run full validation**

Run: `make lint && make covercheck`
Expected: PASS

**Step 2: Verify the interface is sufficient**

Search for any remaining direct VM calls in compiler/expander code:

```
grep -n 'NewMachineContext\|\.Run()\|acquireMacroContext\|acquireSubContext' machine/compile_*.go machine/expander_*.go
```

The only remaining hits should be inside `vmMacroEvaluator` methods and
`invokeTransformerClosure` (called via the interface). If any direct VM
calls remain in compiler/expander code, they must be routed through the
evaluator.

**Step 3: Commit if any fixes were needed**

---

## Phase 2: Package Split

Phase 2 moves files from `machine/` to `machine/compilation/`. This is
mechanical but large (~59 source files, ~40 test files). Each task below
is one commit.

### Task 8: Create machine/compilation/ and move compiler files

**Files:**
- Create: `machine/compilation/` directory
- Move: 26 `compile_*.go` files + `compile_time_call_context.go`

**Step 1: Create the directory and move files**

```bash
mkdir machine/compilation
```

Move all `machine/compile_*.go` files (excluding test files) to
`machine/compilation/`. Change `package machine` to `package compilation`
in each file.

**Step 2: Add qualified imports**

In every moved file, replace unqualified references to machine/ types:
- `NativeTemplate` → `machine.NativeTemplate`
- `MachineClosure` → `machine.MachineClosure`
- `ForeignClosure` → `machine.ForeignClosure`
- `Closure` → `machine.Closure`
- `NewMachineContinuation` → `machine.NewMachineContinuation`
- `OpCode` constants → `machine.OpPush`, `machine.OpBranch`, etc.
- `ExpanderCtx` → `machine.ExpanderCtx`
- etc.

Add `"github.com/aalpar/wile/machine"` to import blocks.

**Step 3: Export any unexported functions needed by compilation/**

Check for unexported functions in `machine/` that the moved files call.
These must be exported (capitalized) or moved along.

**Step 4: Build**

Run: `go build ./machine/... `
Expected: PASS (may require iterating on import fixes)

**Step 5: Commit**

```
refactor: move compiler files to machine/compilation/
```

---

### Task 9: Move expander files to compilation/

**Files:**
- Move: `expander_time_continuation.go`, `expander_*.go`, `expander_context.go`,
  `quasi_expand.go`, `er_macro_*.go`

Same process as Task 8: change package, add qualified imports, export needed symbols.

**Commit:**

```
refactor: move expander files to machine/compilation/
```

---

### Task 10: Move macro operations to compilation/

**Files:**
- Move: `operation_syntax_rules_transform.go`, `operation_syntax_case.go`,
  `operation_build_syntax.go`

These implement `machine.InlinedOperation`. After the move, their `Apply`
method signature uses `*machine.MachineContext`.

**Commit:**

```
refactor: move macro operations to machine/compilation/
```

---

### Task 11: Move library system to compilation/

**Files:**
- Move: `library_registry.go`, `library_bindings.go`, `library_loader.go`,
  `library_discovery.go`, `import_set_datum.go`

`CompiledLibrary`, `LibraryName`, `LibraryRegistry` move with these files.
Since engine.go references `LibraryRegistry` and `CompiledLibrary`, update
those imports.

**Commit:**

```
refactor: move library system to machine/compilation/
```

---

### Task 12: Move infrastructure files to compilation/

**Files:**
- Move: `file_resolver.go`, `features.go`, `peephole.go`, `edit_plan.go`,
  `letrec_semantics.go`
- Move: `syntax_compiler.go`, `syntax_compilers_registry.go`,
  `primitive_expander.go`, `primitive_expanders_registry.go`,
  `phase_registry.go`, `register.go`, `named_handler_base.go`

**Commit:**

```
refactor: move compilation infrastructure to machine/compilation/
```

---

### Task 13: Move MacroEvaluator to compilation/

**Files:**
- Move: `machine/macro_evaluator.go` → `machine/compilation/macro_evaluator.go`
- Modify: `machine/expander_ctx.go` (stays — interface remains in machine/)

After this move, `MacroEvaluator` is defined in `compilation/` and references
`*ExpanderContext` (also in `compilation/`). The `InvokeTransformer` method
signature can now use the concrete `*ExpanderContext` instead of the
`machine.ExpanderCtx` interface, if desired.

Update external callers (engine.go, etc.) to import
`machine/compilation.NewVMMacroEvaluator()`.

**Commit:**

```
refactor: move MacroEvaluator interface to machine/compilation/
```

---

### Task 14: Update external imports

**Files:**
- Modify: `engine.go`, `options.go`, `compiled.go`
- Modify: `internal/bootstrap/environment_tiny.go`
- Modify: `internal/repl/repl.go`, `internal/repl/debug.go`, `internal/repl/meta.go`

Replace `machine.NewCompiletimeContinuation` → `compilation.NewCompiletimeContinuation`, etc.

Add `"github.com/aalpar/wile/machine/compilation"` import.

**Commit:**

```
refactor: update external imports for machine/compilation/
```

---

### Task 15: Migrate test files

**Files:**
- Move: ~40 test files from `machine/` to `machine/compilation/`

Test files that exercise compiler/expander logic move to `compilation/`.
Test files that exercise VM runtime stay in `machine/`.

Cross-concern test files (e.g., `compile_time_continuation_mutual_test.go`)
that need both packages should use `package compilation_test` and import both
`machine` and `compilation`.

**Step 1: Identify which test files go where**

Rule: if the test file's name starts with `compile_`, `expander_`, `library_`,
`syntax_rules_`, `syntax_case_`, `hygiene_`, `let_shadow_macro_`, or matches
a moved source file, it goes to `compilation/`.

**Step 2: Move and fix imports**

**Step 3: Run full test suite**

Run: `make lint && make covercheck`
Expected: PASS

**Commit:**

```
refactor: migrate test files for machine/compilation/ split
```

---

### Task 16: Final validation

**Step 1: Verify no circular imports**

Run: `go build ./...`

**Step 2: Verify machine/ does NOT import compilation/**

Run: `go list -f '{{.Imports}}' ./machine/ | grep compilation`
Expected: no output

**Step 3: Full test suite**

Run: `make lint && make covercheck`
Expected: PASS

**Step 4: Update documentation**

Update `machine/CLAUDE.md` and `machine/CLAUDE.local.md` to reflect the new
package structure. Create `machine/compilation/CLAUDE.md`.

Update `plans/CLAUDE.md` to reference this plan.

**Commit:**

```
docs: update documentation for machine/compilation/ split
```

---

## Remaining Work (not in this plan)

After Phase 2, the engine (`wile/`) still has concrete imports of `compilation/`
types — compiler constructors, library system, file resolver, registration
functions. The VM runtime (`machine/`) is independent, but the engine is not.

Full bidirectional decoupling requires a `Compiler` interface in `machine/`
so the engine can compile code without being coupled to the specific compiler
implementation. The method set should be designed from the actual call sites
in engine.go after Phase 2 establishes the package boundary. This is tracked
separately — not part of this plan.
