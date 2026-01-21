# Macro System Design

This document describes the design and implementation of Wile's hygienic macro system.

## Overview

Wile implements R7RS `syntax-rules` macros using Flatt's "sets of scopes" hygiene model (POPL 2016). The system consists of three layers:

```
┌─────────────────────────────────────────────────────────────┐
│  Layer 3: Hygiene Layer                                     │
│  - Scope creation and propagation                           │
│  - Variable resolution with scope matching                  │
│  - Files: syntax/scope_utils.go, machine/operation_syntax_  │
│           rules_transform.go                                │
├─────────────────────────────────────────────────────────────┤
│  Layer 2: Syntax Adapter                                    │
│  - Bridges syntax objects ↔ raw values                      │
│  - Preserves syntax for captured pattern variables          │
│  - File: match/syntax_adapter.go                            │
├─────────────────────────────────────────────────────────────┤
│  Layer 1: Pattern Matching VM                               │
│  - Unhygienic bytecode-based pattern matcher                │
│  - Captures bindings, handles ellipsis repetition           │
│  - Files: match/match.go, match/syntax_compiler.go          │
└─────────────────────────────────────────────────────────────┘
```

## Processing Pipeline

```
Source Code
    │
    ▼
┌─────────┐
│ Parser  │ → Syntax objects with source locations
└────┬────┘
     │
     ▼
┌──────────┐
│ Expander │ → Detects macro invocations, invokes transformers
└────┬─────┘
     │ (recursive until no macros remain)
     ▼
┌──────────┐
│ Compiler │ → Bytecode operations
└────┬─────┘
     │
     ▼
┌────┐
│ VM │ → Execution
└────┘
```

## Key Data Structures

### Syntax Objects (`syntax/`)

Every parsed expression is wrapped in a syntax object that carries:

- **Source location**: File, line, column for error reporting
- **Scope set**: List of scopes for hygiene (see below)
- **Origin chain**: Tracks which macros produced this syntax

```go
type SyntaxSymbol struct {
    Sym           *values.Symbol
    sourceContext *SourceContext  // Contains Scopes []*Scope
}
```

### Scopes (`syntax/scope.go`)

A scope is a unique identifier created at specific points:

- **Intro scope**: Created for each macro invocation
- **Use-site scope**: Marks identifiers from the macro call site
- **Binding scope**: Marks binding positions (let, lambda parameters)

```go
type Scope struct {
    ID     int64
    Name   string  // For debugging
}
```

### Transformer Closure (`machine/compile_syntax_rules.go`)

A `syntax-rules` form compiles to a `MachineClosure` containing:

- **Compiled clauses**: Pattern bytecode + template for each `(pattern template)` pair
- **Literals set**: Symbols that match literally, not as pattern variables
- **Free identifiers**: Template symbols that shouldn't get intro scope

```go
type SyntaxRulesClause struct {
    pattern      syntax.SyntaxValue
    template     syntax.SyntaxValue
    bytecode     []match.SyntaxCommand
    matcher      *match.SyntaxMatcher
    patternVars  map[string]struct{}
    freeIds      map[string]struct{}   // For recursive macros
    macroScope   *syntax.Scope
}
```

## Hygiene Model: Sets of Scopes

The hygiene algorithm ensures that:
1. Macro-introduced bindings don't capture user variables
2. User bindings don't capture macro-introduced references
3. Recursive macros work correctly

### Core Invariant

A reference resolves to a binding if and only if:

```
bindingScopes ⊆ useScopes
```

The binding's scope set must be a **subset** of the reference's scope set.

### Example: The `swap!` Macro

```scheme
(define-syntax swap!
  (syntax-rules ()
    ((swap! x y)
     (let ((tmp x))
       (set! x y)
       (set! y tmp)))))

(let ((tmp 5) (a 1) (b 2))
  (swap! a b)
  tmp)  ; => 5, NOT captured by macro's tmp
```

**Without hygiene**: The macro's `tmp` would capture the user's `tmp`.

**With hygiene**:
1. When `swap!` expands, a fresh intro scope `S1` is created
2. The macro's `tmp` gets scope set `{S1}`
3. The user's `tmp` has scope set `{}` (no macro scopes)
4. Resolution: `{S1} ⊆ {}` is FALSE, so they don't match
5. Each `tmp` resolves to its own binding

### Implementation in Code

**Scope creation** (`operation_syntax_rules_transform.go:136`):
```go
introScope := syntax.NewScope(nil)
```

**Scope addition** (`syntax_adapter.go:298`):
```go
if introScope != nil && !isFreeIdentifier {
    sym = sym.AddScope(introScope).(*syntax.SyntaxSymbol)
}
```

**Scope matching** (`scope_utils.go:25`):
```go
func ScopesMatch(useScopes, bindingScopes []*Scope) bool {
    // bindingScopes ⊆ useScopes
    for _, bindScope := range bindingScopes {
        found := false
        for _, useScope := range useScopes {
            if bindScope == useScope {
                found = true
                break
            }
        }
        if !found {
            return false
        }
    }
    return true
}
```

## Pattern Matching VM (Layer 1)

The pattern matcher is a stack-based VM that operates on raw `values.Value` types.

### Bytecode Instructions

| Instruction | Purpose |
|-------------|---------|
| `ByteCodeCompareCar` | Compare car with literal value |
| `ByteCodeCaptureCar` | Capture car as pattern variable binding |
| `ByteCodeVisitCar` | Navigate into car (descend into nested list) |
| `ByteCodeVisitCdr` | Navigate to cdr (next element) |
| `ByteCodeDone` | End current level, pop value stack |
| `ByteCodePushContext` | Start ellipsis iteration (push capture context) |
| `ByteCodePopContext` | End ellipsis iteration (pop capture context) |
| `ByteCodeSkipIfEmpty` | Skip loop body if list is empty (zero iterations) |
| `ByteCodeJump` | Unconditional jump for loops |
| `ByteCodeRequireCarEmpty` | Assert car is empty list |

### Ellipsis Handling

Ellipsis patterns (`...`) match zero or more repetitions:

```scheme
(pattern element ...)  ; Matches zero or more elements
```

The compiler generates a loop structure:
```
SkipIfEmpty +N      ; Jump past loop if empty (zero iterations OK)
PushContext         ; New capture context for this iteration
<pattern bytecode>  ; Match one element
PopContext          ; Save this iteration's captures
Jump -M             ; Loop back to check for more
```

Each iteration's captures are stored in child contexts, enabling template expansion to produce repeated output.

## Syntax Adapter (Layer 2)

The adapter bridges syntax objects and the unhygienic VM.

### Key Operations

**`syntaxToValue`**: Strips syntax wrappers for pattern matching
```go
SyntaxPair → values.Pair
SyntaxSymbol → values.Symbol
SyntaxObject → underlying value
```

**`valueToSyntax`**: Re-wraps expanded values with syntax
```go
values.Pair → SyntaxPair (with intro scope)
values.Symbol → SyntaxSymbol (with intro scope, unless free identifier)
```

**`syntaxMap`**: Preserves original syntax for captured variables

When a pattern variable is captured, the adapter stores a mapping from the raw value back to its original syntax object. During expansion, captured values are looked up in this map to preserve their original scopes (they should NOT get the intro scope).

## Macro Expansion (Layer 3)

### Expander Flow (`expander_time_continuation.go`)

1. **Check for macro**: Is the head symbol bound to a `BindingTypeSyntax`?
2. **Check for shadowing**: Does a local variable shadow the macro? (R7RS §4.2.2)
3. **Invoke transformer**: Call the compiled transformer closure
4. **Recursive expansion**: Expand the result (macros can expand to macros)

### Free Identifier Handling

Free identifiers in a template are symbols that are NOT pattern variables. They refer to bindings outside the macro:

- The macro's own name (for recursive macros like `and`, `or`)
- References to other macros (`if`, `let`, `lambda`)
- References to primitives and library functions

These identifiers must NOT receive the intro scope, or they would fail to resolve.

```go
// In compileClause:
freeIds := make(map[string]struct{})
collectFreeIdentifiers(template, patternVars, freeIds)
```

### Let Bindings Shadow Macros

Per R7RS §4.2.2, local variable bindings shadow macros:

```scheme
(define-syntax foo (syntax-rules () ((foo) 1)))
(let ((foo 2))
  (foo))  ; => Error: foo is not a procedure (it's 2, not the macro)
```

The expander checks for local variable bindings before macro lookup (`hasLocalVariableBinding`).

## Bootstrap Macros

R7RS derived expressions are implemented as macros in `runtime/environment_tiny.go`:

| Macro | Expands To |
|-------|------------|
| `and` | Nested `if` expressions |
| `or` | Nested `let` + `if` |
| `let` | `lambda` application |
| `let*` | Nested `let` |
| `letrec` | `let` + `set!` |
| `cond` | Nested `if` |
| `case` | `let` + `cond` + `memv` |
| `when` | `if` (one-armed) |
| `unless` | `if` (negated) |
| `guard` | Exception handling |
| `do` | Named `let` with loop |

These are loaded during environment initialization and use the same macro system as user-defined macros.

## File Reference

| File | Purpose |
|------|---------|
| `match/match.go` | Pattern matching VM |
| `match/syntax_compiler.go` | Pattern → bytecode compiler |
| `match/syntax_adapter.go` | Syntax ↔ value conversion |
| `match/expand.go` | Template expansion |
| `syntax/scope.go` | Scope type definition |
| `syntax/scope_utils.go` | Scope set operations, `ScopesMatch` |
| `syntax/syntax_symbol.go` | Symbol with scopes |
| `syntax/syntax_pair.go` | Pair with recursive scope propagation |
| `machine/compile_syntax_rules.go` | `syntax-rules` compilation |
| `machine/operation_syntax_rules_transform.go` | Runtime macro expansion |
| `machine/expander_time_continuation.go` | Expansion-phase walker |
| `runtime/environment_tiny.go` | Bootstrap macro definitions |

## References

- **Flatt 2016**: "Binding as Sets of Scopes" - https://www.cs.utah.edu/plt/scope-sets/
- **R7RS §4.3**: Macros - https://small.r7rs.org/attachment/r7rs.pdf
- See `BIBLIOGRAPHY.md` at project root for complete references
