# machine/ Package Decomposition Design

**Status**: Approved
**Date**: 2026-03-30
**Related**: `plans/ARCHITECTURE.md` (Module Decomposition), `TODO.md` line 43

## Motivation

`machine/` is 102 source files (20.7K LOC) mixing VM runtime, compiler, and expander.
The compiler cannot be reused without the VM. The goal is reusability: a consumer
should be able to import the compiler independently and provide a different VM backend
(or a stub that rejects macros for static analysis).

## Design Decision: Approach B with Phase C as Phase 1

Three approaches were evaluated:

| Approach | Split | Interfaces needed | Risk |
|----------|-------|-------------------|------|
| A: Full three-way | compiler/ + expander/ + bytecode/ | 3 (MacroEvaluator, Expander, TransformerCompiler) | High — fights natural compiler↔expander coupling |
| B: Compilation vs Runtime | compilation/ + root | 2 (MacroEvaluator, ExpanderCtx) | Medium — one clean boundary |
| C: Bottom-up types only | bytecode/ + root | 0 | Low — doesn't achieve reusability |

**Chosen: B.** The compiler and expander are mutually dependent (compiler creates expanders
to expand before compiling; expander compiles transformers for internal `define-syntax`).
This coupling is semantic — R7RS requires it. Splitting them apart would require two
additional interfaces for no real reuse scenario. The practical reuse target is
"compilation pipeline without VM runtime," which Approach B delivers with one interface.

Phase 1 introduces the interfaces within the existing package (validating the design).
Phase 2 performs the file moves.

## Package Structure

```
machine/                     # Types + VM runtime (~8.8K source LOC)
└── compilation/             # Compiler + Expander + Library system (~11.9K source LOC)
```

### machine/ (root) retains

**All shared type definitions** (referenced by 65+ external consumers):

| Category | Types |
|----------|-------|
| Bytecode | NativeTemplate, Instruction, OpCode, Operation, InlinedOperation |
| Closures | MachineClosure, ForeignClosure, CaseLambdaClosure, Closure, ForeignFunction |
| VM State | MachineContext, vmState, MachineContinuation, Stack, MultipleValues |
| Continuations | CapturedContinuation, ComposableContinuation, ContinuationMarkSet |
| Control Flow | DynamicWindFrame, WindingStack, ExceptionHandler, ErrExceptionEscape |
| Errors/Tags | SchemeError, PromptTag, ErrPromptAbort, BarrierToken |
| Other | Parameter, StackFrame, StackTrace, Pool[T], FreeList[T] |

**VM execution logic** (~35 files):
- machine_context.go (Run() loop, dispatch)
- machine_context_apply.go, _continuation.go, _winding.go, _subcontext.go
- operations_*.go (stack, load/store, control, call, closure, winding)
- call_promoted.go, call_promoted_arithmetic.go, call_foreign_cached.go
- operation_cont_mark.go, operation_helpers.go
- pool.go, pool_generic.go, counters.go, debugger.go, arity.go, closure.go

**One new interface**: `ExpanderCtx` (see Interface Design below).

### compilation/ contains

**Compiler** (26 files, ~7K LOC):
- compile_time_continuation.go + all compile_*.go

**Expander** (12 files, ~6.1K LOC):
- expander_time_continuation.go + all expander_*.go
- expander_context.go (implements machine.ExpanderCtx)
- quasi_expand.go, er_macro_*.go

**Macro operations** (3 files, ~800 LOC):
- operation_syntax_rules_transform.go, operation_syntax_case.go, operation_build_syntax.go
- Implement machine.InlinedOperation — VM dispatches them polymorphically through the side table

**Library system** (5 files, ~1.5K LOC):
- library_registry.go, library_bindings.go, library_loader.go, library_discovery.go
- import_set_datum.go

**Infrastructure** (7 files, ~1.8K LOC):
- file_resolver.go, features.go, peephole.go, edit_plan.go, letrec_semantics.go

**Phase registration** (6 files, ~600 LOC):
- syntax_compiler.go, syntax_compilers_registry.go
- primitive_expander.go, primitive_expanders_registry.go
- phase_registry.go, register.go

**MacroEvaluator interface + default implementation** (see Interface Design below).

## Interface Design

### ExpanderCtx (defined in machine/)

Replaces the concrete `*ExpanderContext` field on `MachineContext`, preventing
`machine/` → `compilation/` import.

```go
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

`MachineContext.expanderCtx` changes from `*ExpanderContext` to `ExpanderCtx`.
The concrete `ExpanderContext` struct moves to `compilation/` and implements
this interface. Consumers like `internal/extensions/eval/prim_eval.go` call
`mc.ExpanderContext()` which returns the interface — method set is identical.

### MacroEvaluator (defined in compilation/)

Abstracts the two sites where compilation calls the VM: transformer evaluation
(`compileAndEvalLambdaTransformer`) and transformer invocation
(`invokeTransformerClosure`).

```go
type MacroEvaluator interface {
    // EvalTemplate evaluates a compiled template and returns the result.
    // Used by compileAndEvalLambdaTransformer for define-syntax lambda transformers.
    EvalTemplate(ctx context.Context, tpl *machine.NativeTemplate, env *environment.EnvironmentFrame) (values.Value, error)

    // InvokeTransformer calls a closure as a macro transformer with the given input form.
    // expanderCtx is set on the VM context for auxiliary syntax hygiene (R7RS §4.3.2).
    InvokeTransformer(ctx context.Context, cls machine.Closure, input syntax.SyntaxValue, expanderCtx *ExpanderContext) (values.Value, error)
}
```

Default implementation in `compilation/`:

```go
type vmMacroEvaluator struct{}

func (p *vmMacroEvaluator) EvalTemplate(ctx context.Context, tpl *machine.NativeTemplate, env *environment.EnvironmentFrame) (values.Value, error) {
    cont := machine.NewMachineContinuation(nil, tpl, env)
    mc := machine.NewMachineContext(ctx, cont)
    if err := mc.Run(); err != nil {
        return nil, err
    }
    q := mc.GetValue()
    machine.ReleaseSubContext(mc)
    return q, nil
}
```

**Why this lives in compilation/, not machine/:** The interface references
`*ExpanderContext` (a compilation/ type). The implementation calls
`machine.NewMachineContext` and `mc.Run()` — that's compilation/ → machine/,
the allowed direction.

**Reuse payoff:** A consumer who imports compilation/ without machine/ provides
their own MacroEvaluator — e.g., a stub that rejects all macros (for analyzing
macro-free code) or a different evaluation backend.

## Dependency Flow

```
wile/ (engine) ──────► machine/ (types + runtime)
       │                     ▲
       └────────► compilation/ ─┘
```

One-way imports, no cycles:

| From | To | What flows |
|------|-----|------------|
| compilation/ | machine/ | All shared types, ExpanderCtx interface, pool/context constructors |
| wile/ | machine/ | Types, MachineContext constructors, Run() |
| wile/ | compilation/ | Compiler/expander constructors, MacroEvaluator, registration, FileResolver, LibraryRegistry |
| extensions, registry | machine/ | Types only (MachineContext, closures) — no change |
| internal/forms/ | neither | Already uses `any` for compiler types — no change |
| internal/match/ | neither | Already uses `any`/interfaces — no change |
| internal/extensions/eval/ | machine/ | mc.ExpanderContext() returns machine.ExpanderCtx (interface instead of concrete) |

**Key invariant:** machine/ never imports compilation/. Macro operations in
compilation/ implement machine.InlinedOperation — the VM dispatches them
polymorphically through the side table without knowing their concrete type.

## Phasing

### Phase 1: Interface Introduction (within machine/, no file moves)

1. Define `ExpanderCtx` interface in machine/
2. Make `ExpanderContext` implement it
3. Change `MachineContext.expanderCtx` from `*ExpanderContext` to `ExpanderCtx`
4. Update `SetExpanderContext` / `ExpanderContext()` signatures
5. Update `internal/extensions/eval/prim_eval.go` to use interface methods
6. Define `MacroEvaluator` interface (still in machine/ temporarily)
7. Extract VM calls from `compileAndEvalLambdaTransformer` and `invokeTransformerClosure` into a `vmMacroEvaluator` struct
8. Thread `MacroEvaluator` through `CompileTimeContinuation` and `ExpanderTimeContinuation` constructors
9. Validate: `make lint && make covercheck`

Pure internal refactor. All tests pass, no external changes, no file moves.
Validates that the interface is sufficient.

### Phase 2: Package Split

1. Create `machine/compilation/` directory
2. Move files (per Section 1) — change `package machine` → `package compilation`
3. Add qualified imports (machine.NativeTemplate, machine.MachineContext, etc.)
4. Move `MacroEvaluator` interface and `vmMacroEvaluator` from machine/ to compilation/
5. Move `ExpanderContext` struct to compilation/ (interface stays in machine/)
6. Update wile/ engine files (engine.go, options.go, compiled.go) to import compilation/
7. Update internal/bootstrap/, internal/repl/ imports
8. Migrate test files to matching package
9. Validate: `make lint && make covercheck`

File move estimate:

| Category | Source files | Test files | Source LOC |
|----------|-------------|------------|------------|
| Compiler | 26 | ~20 | ~7,000 |
| Expander | 12 | ~8 | ~6,100 |
| Macro ops | 3 | ~3 | ~800 |
| Library | 5 | ~4 | ~1,500 |
| Infrastructure | 7 | ~3 | ~1,800 |
| Phase registration | 6 | ~2 | ~600 |
| **Total** | **~59** | **~40** | **~17,800** |

## External Impact

**Zero changes:**
- All extensions (extensions/*/)
- All core primitives (registry/core/)
- Registry helpers (registry/helpers/)
- internal/forms/, internal/match/, internal/validate/
- internal/tokenizer/, internal/parser/, internal/syntax/

**Minor changes (import path only):**
- engine.go, options.go, compiled.go
- internal/bootstrap/environment_tiny.go
- internal/repl/
- internal/extensions/eval/prim_eval.go (ExpanderCtx interface type)

No changes to Go module path. compilation/ is a sub-package, not a separate module.

## Open Questions

1. **edit_plan.go placement:** Operates on NativeTemplate (machine/) but only called during
   compilation. Move to compilation/ since it's a compilation-only tool?

2. **Cross-concern test files:** Files like `compile_time_continuation_mutual_test.go` exercise
   compiler→expander→VM round-trips. May need `_test` package importing both.

3. **Pool access from compilation/:** The macro evaluator needs `acquireMacroContext` /
   `ReleaseSubContext`. Ensure these pool operations are exported from machine/.

4. **namedHandlerBase:** Unexported type used by SyntaxCompiler and PrimitiveExpander.
   Moves with them to compilation/.

## Remaining Work: VM Independence from Compiler ¹

This plan addresses the compiler→VM direction (via `MacroEvaluator`). The reverse
direction remains: `wile/` (the engine) still has concrete imports of `compilation/`
types — compiler constructors, library system, file resolver, registration functions.
The VM runtime (`machine/`) is already independent (it never imports `compilation/`),
but the engine is not.

Full bidirectional decoupling requires a `Compiler` interface in `machine/` so the
engine can compile code without being coupled to the specific compiler implementation.
This would let consumers use the VM with pre-compiled templates (no compiler at all)
or with a different compiler backend. The interface method set will become clear after
Phase 2 establishes the package boundary — it should be designed from the actual
call sites in engine.go, not speculatively.

## Decision Log

| Decision | Rationale |
|----------|-----------|
| Compiler + expander stay together | Mutually dependent: compiler creates expanders, expander compiles transformers. Cycle is semantic (R7RS), not accidental. |
| Types stay in machine/ root | 65+ external consumers reference MachineContext, closures, etc. Moving types would change every extension. |
| ExpanderCtx as interface in machine/ | Prevents machine/ → compilation/ import. Small method set (7 methods), not on hot path. |
| MacroEvaluator defined in compilation/ | References ExpanderContext (compilation/ type). Implementation calls machine/ constructors — allowed direction. |
| Default MacroEvaluator in compilation/ | The implementation uses machine.MachineContext directly. No reverse dependency needed. |
| Phase 1 before Phase 2 | Validates interface design without file-move risk. If the interface is wrong, fixing it in one package is cheap. |
| internal/forms/ unchanged | Already uses `any` for CompilerFunc parameters to break import cycles. Package split doesn't affect it. |
| Macro operations move to compilation/ | Their Apply() methods call expander internals. VM dispatches them via InlinedOperation interface — no import needed. |
