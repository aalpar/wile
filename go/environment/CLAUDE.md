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

## Phase Navigation

```go
env.Runtime()  // Creates/returns runtime phase environment
env.Expand()   // Creates/returns expand phase environment
env.Compile()  // Creates/returns compile phase environment
env.TopLevel() // Traverses to root environment
```

## Binding Lookup

Two-phase lookup in `GetBinding()`:
1. Local phase: Traverse local bindings up through parents
2. Global phase: Search global bindings (stops at TopLevel)

## Gotchas

- **GetIndex() has known bug**: Skips first frame in loops (documented in code)
- **CreateLocalBinding is "MaybeCreate"**: Returns `(index, false)` if exists
- **Phase hierarchy**: Expand/Compile have parent pointing to TopLevel, not each other
- **Symbol interning shared**: All phases access same interning maps
- **Copy() shares parent**: Only local/global are copied, parent is shared
- **LibraryRegistry as `any`**: Stored as `any` to avoid circular imports

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

When adding new functionality, add tests to the corresponding test file.
