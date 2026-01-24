# CLAUDE.md

Package `match` implements pattern matching for `syntax-rules` macros.

## Purpose

Layer 1 of the macro system - unhygienic pattern matching VM:
- Compiles R7RS patterns into bytecode
- Executes bytecode to capture pattern variable bindings
- Supports ellipsis patterns for zero-or-more repetitions
- Supports R7RS §4.3.2 custom ellipsis identifiers
- Supports R7RS §4.3.2 ellipsis escape forms `(<ellipsis> <template>)`

## Key Types

**Matcher** - Core VM:
- `variables` - Pattern variable names
- `codes` - Bytecode instructions
- `captureStack` - Captured bindings during matching
- `valueStack` - Input tree position tracking
- `ellipsisVars` - Maps ellipsis IDs to captured variables
- `ellipsisID` - Custom ellipsis identifier (default `"..."`)

**SyntaxCompiler** - Compiles patterns to bytecode:
- `ellipsis` - Custom ellipsis identifier for pattern compilation
- `literals` - Literal identifiers that match by name, not as variables
- `skipMacroKeyword` - When true, skips the first pattern element (macro keyword placeholder)
- `macroKeywordPassed` - Tracks whether the first element has been processed

**SyntaxMatcher** - Layer 2 bridge for syntax objects with hygiene:
- `ellipsisID` - Custom ellipsis identifier passed to underlying Matcher

## Bytecode Instructions

| Instruction | Purpose |
|-------------|---------|
| `ByteCodeCompareCar` | Compare car with literal |
| `ByteCodeCompareCdr` | Compare cdr with literal (improper list patterns) |
| `ByteCodeCaptureCar` | Capture car as pattern variable |
| `ByteCodeCaptureCdr` | Capture cdr as pattern variable (improper list patterns) |
| `ByteCodeVisitCar/Cdr` | Navigate into list |
| `ByteCodeDone` | Signal completion |
| `ByteCodePushContext` | Start ellipsis iteration |
| `ByteCodePopContext` | Close iteration context |
| `ByteCodeSkipIfEmpty` | While-loop check for zero iterations |
| `ByteCodeSkipIfTailCount` | Exit ellipsis loop when N elements remain (ellipsis-in-middle) |
| `ByteCodeJump` | Unconditional jump for loops |

## Gotchas

- **SkipIfEmpty essential**: Without it, ellipsis uses do-while (at least once); with it, while-loop (zero ok)
- **RequireCarEmpty for ()**: Special instruction to validate empty list patterns
- **Ellipsis bytecode relocation**: Pattern bytecode moved into loop structure
- **Ellipsis IDs**: Unique IDs track which variables each ellipsis captures
- **Syntax preservation**: SyntaxMatcher.syntaxMap preserves original syntax for captured variables
- **Done validation complex**: Context-aware checking of cdr based on next instruction
- **Custom ellipsis threading**: Custom ellipsis identifier must be threaded through `NewSyntaxCompilerWithEllipsis`, `NewSyntaxMatcherFull`, and `NewMatcherFull` to work correctly
- **Underscore checks literals**: `_` is a wildcard only if NOT in the literals list; `compileSymbolElement` checks this
- **Escape form detection**: `expandValue` checks for `(<ellipsisID> <template>)` and calls `expandEscapedTemplate` which expands without treating ellipsis specially
- **Free identifiers with pre-resolved bindings**: In `valueToSyntaxWithOrigin`, free identifiers (non-pattern-variables like `if`, `begin`, helper functions) receive pre-resolved bindings from the `freeIds` map. At macro definition time, free identifiers are resolved to their `GlobalIndex` and stored in `freeIds`. During template expansion, these bindings are attached to symbols via `WithResolvedBinding()`. This ensures R7RS §4.3 compliance: macro-introduced identifiers refer to definition-time bindings even when the macro is used in a different library context.
- **Improper list patterns**: Patterns like `(_ a . rest)` use `ByteCodeCaptureCdr` to capture the remaining input. After capturing, the value stack position is updated to empty to prevent `Done` from seeing "extra" elements.
- **Ellipsis-in-middle patterns**: Patterns like `(_ a b ... x y)` use `ByteCodeSkipIfTailCount` to exit the loop when exactly N elements remain for the trailing pattern. The compiler counts trailing elements via `countPatternTailElements`.
- **Macro keyword placeholder**: R7RS §4.3.2 specifies that the first element of each syntax-rules pattern is the macro keyword placeholder and should not be matched. `CompileSyntaxPatternWithLiterals` enables `skipMacroKeyword` to skip bytecode generation for the first element. This allows patterns like `(foo _)` to match `(my-macro bar)` where `foo` != `my-macro`.

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

## References

See `BIBLIOGRAPHY.md` at project root for R7RS §4.3.2 (syntax-rules pattern language).
