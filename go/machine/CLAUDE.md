# CLAUDE.md

Package `machine` implements the Scheme virtual machine, compiler, and macro expander.

## Purpose

- **Bytecode compilation**: Converts validated syntax into stack-based operations
- **Runtime execution**: Executes bytecode via continuation-based evaluator
- **Macro expansion**: Supports syntax-rules and syntax-case
- **Library system**: R7RS library support with imports/exports

## Architecture

```
ValidatedExpr → CompileTimeContinuation → NativeTemplate → MachineContext → Result
```

## Key Types

| Type | Purpose |
|------|---------|
| `NativeTemplate` | Compiled function (parameters, literals, operations, sourceMap) |
| `MachineClosure` | Callable function (template + environment) |
| `MachineContext` | Execution state (env, value, evals stack, continuation, PC) |
| `MachineContinuation` | Saved state for function calls |
| `Operation` | Single bytecode instruction |
| `CompileTimeContinuation` | Compiler state |
| `CompileTimeCallContext` | Compilation context (tail position, expression mode) |
| `CompiledLibrary` | Loaded library with exports |

## Key Files

| File | Purpose |
|------|---------|
| `compile_time_continuation.go` | Main compiler (2000+ lines) |
| `compile_time_call_context.go` | Compilation context (tail position, expression mode) |
| `machine_context.go` | Execution context and VM loop |
| `operation_*.go` | 50+ bytecode instruction implementations |
| `library.go` | Library system |
| `debugger.go` | Breakpoint and stepping support |
| `compile_syntax_rules.go` | R7RS syntax-rules compilation with custom ellipsis |
| `compile_syntax_form.go` | `(syntax template)` compilation with escape form optimization |
| `expander_time_continuation.go` | Macro expander (quasiquote, let-syntax, syntax-case) |

## Gotchas

- **Environment copying critical**: `Apply` copies local env for each call to prevent recursion bugs
- **Tail call optimization**: Non-tail calls emit SaveContinuation; tail calls skip it
- **Continuation escape**: `ErrContinuationEscape` propagates through foreign calls
- **Phase separation**: Expand and runtime phases have separate environments
- **Quasiquote depth**: Tracks nesting level for correct unquote handling
- **Symbol interning**: Symbols interned through environment for `eq?` identity
- **Eval stack vs value**: Arguments on stack, result in value register
- **SubContext for foreign calls**: Fresh stacks but shared global environment
- **Run() does NOT reset pc**: The VM loop in `Run()` starts from the current `pc` value. Callers must set `pc` appropriately: `Apply` sets `pc=0` for fresh closure invocation, `Restore` preserves saved `pc` for continuation resumption. Do NOT add `pc=0` to `Run()` - it would break `raise-continuable` resumption semantics.
- **Let bindings shadow macros**: Per R7RS §4.2.2, local variable bindings in `let`/`let*`/`letrec` shadow outer macro definitions. The expander checks for local variable bindings before looking up macros (`hasLocalVariableBinding` in `expander_time_continuation.go`).
- **let-syntax/letrec-syntax fully handled during expansion**: These forms are primitive expanders that complete all macro binding and body expansion before compilation begins. The expander (`expandLetSyntaxImpl` in `expander_time_continuation.go`) creates a child expand environment with local macro bindings, compiles syntax-rules transformers, expands the body with the child environment, and returns the expanded body. The let-syntax/letrec-syntax wrapper disappears after expansion - only the expanded body reaches the compiler.
- **let-syntax body defines wrapped in lambda**: When `expandLetSyntaxImpl` detects `define` forms in the expanded body, it wraps the body in `((lambda () (begin body...)))` to create a new runtime stack frame. This prevents defines from leaking to the outer scope. Without defines, the body compiles directly.
- **Nested let-syntax uses expand environment chain**: Each let-syntax creates a child expand environment with the enclosing expand environment as parent. This enables inner macros to reference outer macros during expansion. The parent chain is established during expansion, not compilation.
- **Library bodies use letrec\* semantics**: R7RS §5.3.2 requires all defined names to be visible to all initializers, enabling forward references. `compileLibraryBegin` and `processFormsWithLetrecSemantics` implement two-pass compilation: pass 1 pre-declares all `define` bindings via `predeclareDefineBinding`, pass 2 compiles all forms with bindings visible.
- **Auxiliary syntax lookup checks three environments**: When importing/exporting `else` and `=>`, `CopyLibraryBindingsToEnv` checks: (1) runtime env, (2) expand env for macros, (3) compile env for auxiliary syntax. These keywords are registered as compile-time bindings in `registry/core/specialforms.go`.
- **Vector quasiquote with unquote-splicing**: `expandQuasiquote` handles `\`#(... ,@expr ...)` by detecting splicing, segmenting elements, and generating `(list->vector (append (list ...) expr (list ...)))` instead of the simple `(list->vector (list ...))` form.
- **Custom ellipsis in syntax-rules**: R7RS §4.3.2 allows `(syntax-rules <ellipsis> (<literal>...) <clause>...)` to specify a custom ellipsis identifier. `CompileSyntaxRules` detects this form and threads the custom ellipsis through pattern compilation and template expansion.
- **Ellipsis escape form optimization**: `compile_syntax_form.go` recognizes `(<ellipsis> <template>)` escape forms at compile time. `templateContainsEllipsis` skips escaped content, and `compileSyntaxTemplateToOps` compiles just the inner template. This avoids unnecessary runtime expansion for templates using escape forms.
- **Ellipsis cannot be in literals list**: `extractLiterals` validates that the ellipsis identifier (default `...` or custom) cannot appear in the literals list per R7RS §4.3.2.

## Testing

Uses quicktest with unit tests for operations, integration tests for compilation/execution.

### Test File Organization

This package uses **consolidation** across 45+ test files organized by functional area:

| Pattern | Examples | Tests For |
|---------|----------|-----------|
| `operation_*.go` | `operation_test.go`, `operation_misc_test.go` | Bytecode operations |
| `compile_*.go` | `compile_time_continuation_test.go`, `compile_syntax_case_test.go` | Compilation phases |
| `library_*.go` | `library_test.go`, `library_internal_test.go` | Library system |
| `syntax_rules_*.go` | `syntax_rules_test.go`, `syntax_rules_internal_test.go` | Macro expansion |
| `machine_*.go` | `machine_context_test.go`, `machine_closure_test.go` | VM execution |
| `*_coverage_test.go` | `coverage_improvement_test.go` | Additional edge cases |

When adding tests:
- Operation constructors → `operation_test.go`
- Compilation logic → relevant `compile_*_test.go`
- VM execution → `machine_context_test.go`
- Cross-cutting concerns → `hygiene_test.go` or appropriate `*_internal_test.go`

## References

See `BIBLIOGRAPHY.md` at project root for:
- Flatt 2016 "Binding as Sets of Scopes" - the hygiene model used for macro expansion
- R7RS §4.3 (Macros) and §5.4 (Syntax definitions)
