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
| `CompiledLibrary` | Loaded library with exports |

## Key Files

| File | Purpose |
|------|---------|
| `compile_time_continuation.go` | Main compiler (2000+ lines) |
| `machine_context.go` | Execution context and VM loop |
| `operation_*.go` | 50+ bytecode instruction implementations |
| `library.go` | Library system |
| `debugger.go` | Breakpoint and stepping support |

## Gotchas

- **Environment copying critical**: `Apply` copies local env for each call to prevent recursion bugs
- **Tail call optimization**: Non-tail calls emit SaveContinuation; tail calls skip it
- **Continuation escape**: `ErrContinuationEscape` propagates through foreign calls
- **Phase separation**: Expand and runtime phases have separate environments
- **Quasiquote depth**: Tracks nesting level for correct unquote handling
- **Symbol interning**: Symbols interned through environment for `eq?` identity
- **Eval stack vs value**: Arguments on stack, result in value register
- **SubContext for foreign calls**: Fresh stacks but shared global environment

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
