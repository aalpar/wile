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
