# CLAUDE.md

Package `match` implements pattern matching for `syntax-rules` macros.

## Purpose

Layer 1 of the macro system - unhygienic pattern matching VM:
- Compiles R7RS patterns into bytecode
- Executes bytecode to capture pattern variable bindings
- Supports ellipsis patterns for zero-or-more repetitions

## Key Types

**Matcher** - Core VM:
- `variables` - Pattern variable names
- `codes` - Bytecode instructions
- `captureStack` - Captured bindings during matching
- `valueStack` - Input tree position tracking
- `ellipsisVars` - Maps ellipsis IDs to captured variables

**SyntaxCompiler** - Compiles patterns to bytecode

**SyntaxMatcher** - Layer 2 bridge for syntax objects with hygiene

## Bytecode Instructions

| Instruction | Purpose |
|-------------|---------|
| `ByteCodeCompareCar` | Compare car with literal |
| `ByteCodeCaptureCar` | Capture car as pattern variable |
| `ByteCodeVisitCar/Cdr` | Navigate into list |
| `ByteCodeDone` | Signal completion |
| `ByteCodePushContext` | Start ellipsis iteration |
| `ByteCodePopContext` | Close iteration context |
| `ByteCodeSkipIfEmpty` | While-loop check for zero iterations |
| `ByteCodeJump` | Unconditional jump for loops |

## Gotchas

- **SkipIfEmpty essential**: Without it, ellipsis uses do-while (at least once); with it, while-loop (zero ok)
- **RequireCarEmpty for ()**: Special instruction to validate empty list patterns
- **Ellipsis bytecode relocation**: Pattern bytecode moved into loop structure
- **Ellipsis IDs**: Unique IDs track which variables each ellipsis captures
- **Syntax preservation**: SyntaxMatcher.syntaxMap preserves original syntax for captured variables
- **Done validation complex**: Context-aware checking of cdr based on next instruction

## Testing

Uses quicktest with suite pattern. Tests cover bytecode execution, pattern compilation, and expansion.

### Test File Organization

Tests are organized by functional area:

| Test File | Tests For |
|-----------|-----------|
| `match_test.go` | Core Matcher VM bytecode execution |
| `syntax_compiler_test.go` | Pattern compilation to bytecode |
| `syntax_adapter_test.go` | SyntaxMatcher bridge layer |
| `pattern_analyzer_test.go` | Pattern analysis utilities |
| `expand_test.go` | Template expansion |

When adding tests, choose the file matching the functional area being tested.
