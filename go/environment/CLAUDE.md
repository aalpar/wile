# CLAUDE.md

Package `environment` provides variable binding and scoping for the compiler.

## Purpose

Manages the relationship between variables and values across:
- **Lexical scoping**: Parent chain for enclosing scopes
- **Phase separation**: Runtime, Expand (macro), and Compile phases
- **Hygienic macros**: Scope-based identifier resolution
- **Symbol interning**: Ensures `(eq? 'x 'x)` across phases

## Key Types

**EnvironmentFrame** - Main environment with two axes:
- `parent` - Lexical parent (for closures)
- `local` - LocalEnvironmentFrame for lambda params, let-bound vars
- `global` - GlobalEnvironmentFrame for global bindings
- `meta` - Next phase (Expand/Compile)

**Binding** - Variable binding with metadata:
- `value` - The bound value
- `bindingType` - Variable/Syntax/Primitive
- `scopes` - For hygienic macro expansion
- `source` - Source location (optional)

**LocalIndex** `[2]int` - Locates local bindings:
- `[0]` - Slot index within frame
- `[1]` - Depth (parent frames to traverse)

**PhaseRegistry** - Indexed phase environment storage:
- `envs map[int]*EnvironmentFrame` - O(1) phase access by integer index
- Thread-safe via `sync.RWMutex` for concurrent macro expansion
- Owned by TopLevel, shared across all child environments
- Supports negative phases (for future `for-template` support)

## Phase Navigation

Phases are indexed by integer level with O(1) access via `PhaseRegistry`:

```go
env.AtPhase(0)  // Phase 0: Runtime (same as TopLevel)
env.AtPhase(1)  // Phase 1: Expansion (for-syntax)
env.AtPhase(2)  // Phase 2: Compile-time (for-meta 2)
env.AtPhase(-1) // Phase -1: Template (for-template, future)
env.AtPhase(n)  // Arbitrary phase N

// Convenience methods (use AtPhase internally)
env.Runtime()   // AtPhase(PhaseRuntime) = AtPhase(0)
env.Expand()    // AtPhase(PhaseExpand) = AtPhase(1)
env.Compile()   // AtPhase(PhaseCompile) = AtPhase(2)
env.TopLevel()  // Traverses parent chain to root
```

**Phase Constants**:
```go
PhaseTemplate = -1  // for-template
PhaseRuntime  = 0   // runtime execution
PhaseExpand   = 1   // macro expansion (for-syntax)
PhaseCompile  = 2   // compile-time (for-meta 2)
```

## Binding Lookup

Two-phase lookup in `GetBinding()`:
1. Local phase: Traverse local bindings up through parents
2. Global phase: Search global bindings (stops at TopLevel)

## let-syntax Environment Chain

`let-syntax` and `letrec-syntax` create child expand environments during macro expansion:

```go
// In expander_time_continuation.go expandLetSyntaxImpl():
localExpandEnv := environment.NewLocalEnvironment(numBindings)
childExpandEnv := environment.NewEnvironmentFrameWithParent(localExpandEnv, p.env)
```

Key points:
- **Parent is the enclosing expand environment** (`p.env`), not `p.env.Expand()`
- This preserves the environment chain for nested let-syntax forms
- Inner macros can reference outer macros through the parent chain
- Local macro bindings use `BindingTypeSyntax` with scopes for hygiene

Example environment chain for nested let-syntax:
```
TopLevel.Expand()
    └── outer let-syntax childExpandEnv (has macro 'outer')
            └── inner let-syntax childExpandEnv (has macro 'inner', can see 'outer')
```

## Gotchas

- **GetIndex() has known bug**: Skips first frame in loops (documented in code)
- **CreateLocalBinding is "MaybeCreate"**: Returns `(index, false)` if exists
- **Phase environments parent to TopLevel**: All phase environments (Expand, Compile, etc.) have `parent` pointing to TopLevel for symbol interning access
- **Symbol interning shared**: All phases access same interning maps via TopLevel
- **Copy() shares parent and phases**: Only local/global are copied; parent and PhaseRegistry are shared
- **LibraryRegistry as `any`**: Stored as `any` to avoid circular imports
- **PhaseRegistry is thread-safe**: Uses `sync.RWMutex` for concurrent access during macro expansion

## Testing

Uses quicktest with coverage of binding operations, phase hierarchy, and scope-based lookup.

### Test File Organization

This package uses **1:1 mapping** - each source file has a corresponding test file:

| Source File | Test File |
|-------------|-----------|
| `binding.go` | `binding_test.go` |
| `environment_frame.go` | `environment_frame_test.go` |
| `environment_mapping.go` | `environment_mapping_test.go` |
| `global_environment_frame.go` | `global_environment_frame_test.go` |
| `local_environment_frame.go` | `local_environment_frame_test.go` |
| `local_index.go` | `local_index_test.go` |
| `meta_frame.go` | `meta_frame_test.go` |
| `phase_registry.go` | `phase_registry_test.go` |

When adding new functionality, add tests to the corresponding test file.
