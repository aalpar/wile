# CLAUDE.md

Package `define_syntax` implements a stack-based VM for macro pattern matching.

## Purpose

Provides a pattern matching engine for `syntax-rules` macros:
- Compiles patterns into bytecode operations
- Executes operations to match input syntax trees
- Captures pattern variable bindings into a tree structure

## Key Types

**MacroMachine** - Core VM with:
- `target` - Input wrapped in list for uniform handling
- `curr` - Current position in input (always a Pair)
- `stack` - Saved positions for nested list navigation
- `tree` - Root of TreeEntry binding structure
- `operations` - Sequence of MacroOp instructions

**TreeEntry** - Stores pattern variable bindings:
- `bindings` - Map from Symbol to []Value (supports ellipsis)
- `children` - Nested TreeEntry nodes for sub-patterns

**MacroOp** - Interface for all operations (`Apply(*MacroMachine) error`)

## Bytecode Operations

| Operation | Purpose |
|-----------|---------|
| `MacroOpMatch` | Exact literal value matching |
| `MacroOpCapture` | Pattern variable verification |
| `MacroOpBind` | Bind value to pattern variable (placeholder) |
| `MacroOpNext` | Advance to next list element |
| `MacroOpStartList` | Descend into nested list |
| `MacroOpEndList` | Return from nested list |
| `MacroOpMatchEmptyList` | Verify list termination |
| `MacroOpMatchUntil` | Ellipsis pattern support (placeholder) |

## Gotchas

- **MacroOpBind is placeholder**: Currently does nothing except advance PC
- **MacroOpMatchUntil is placeholder**: Ellipsis handling not fully implemented
- **MaybeAppendKeyword bug**: Adds to `literals` instead of `keywords`
- **SetTarget wraps input**: Always wraps in list; expects `MacroOpStartList` first
- **No bounds checking on pop**: `MacroOpEndList` panics if stack is empty

## Testing

Uses quicktest with table-driven tests for operations, deduplication, and error cases.
