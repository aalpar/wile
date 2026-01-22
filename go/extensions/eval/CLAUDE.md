# CLAUDE.md

Package `eval` provides eval and environment primitives.

## Purpose

- Runtime evaluation (eval, load)
- Environment creation and access
- Macro expansion utilities (expand, expand-once, compile)
- Syntax local operations for advanced macros

## Key Files

| File | Purpose |
|------|---------|
| `register.go` | Extension registration |
| `prim_eval.go` | eval, load implementations |
| `prim_environment.go` | Environment primitives |
| `prim_expand.go` | expand, expand-once, compile |
| `prim_syntax_local.go` | Syntax-local operations |

## Primitives (Runtime only)

### Evaluation

| Primitive | Args | Purpose |
|-----------|------|---------|
| `eval` | 2 | Evaluate expression in environment |
| `load` | 1 | Load and evaluate file |

### Environments

| Primitive | Args | Purpose |
|-----------|------|---------|
| `interaction-environment` | 0 | Get current interaction environment |
| `scheme-report-environment` | 1 | Get R5RS environment (version 5) |
| `null-environment` | 1 | Get minimal syntax-only environment |
| `environment` | 1+ | Create environment from library specs |

### Macro Expansion

| Primitive | Args | Purpose |
|-----------|------|---------|
| `expand` | 1 | Fully expand expression |
| `expand-once` | 1 | Expand one level |
| `compile` | 1 | Compile expression to bytecode |

### Syntax Local (Advanced Macros)

| Primitive | Args | Purpose |
|-----------|------|---------|
| `syntax-local-value` | 1 | Get compile-time value of identifier |
| `make-compile-time-value` | 1 | Create compile-time value |
| `syntax-local-introduce` | 1 | Introduce new scope to syntax |
| `syntax-local-identifier-as-binding` | 1 | Get identifier as binding form |

## Usage

```go
import "wile/extensions/eval"

// Use with registry
reg := registry.NewRegistry()
eval.AddToRegistry(reg)
```

## Gotchas

- **Environment versions**: scheme-report-environment only accepts version 5
- **Syntax-local**: Only valid during macro expansion phase
