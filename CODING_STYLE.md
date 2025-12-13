# Coding Style Guide

This document describes the coding conventions used throughout the Wile Scheme interpreter codebase.

## Receiver Naming

All method receivers use single-letter names:

| Letter | Usage |
|--------|-------|
| `p` | Standard receiver for all pointer types |
| `c` | Compiler-related types (e.g., `*CompileTimeContinuation`) |

```go
// Standard pattern - always 'p' for pointer receiver
func (p *Integer) Add(o Number) Number { ... }
func (p *Pair) Car() Value { ... }
func (p *EnvironmentFrame) GetBinding(sym *Symbol) *Binding { ... }
```

**Never use:**
- Descriptive names like `this`, `self`, `receiver`, `integer`
- Multi-letter abbreviations like `str`, `env`, `tok`

## Variable Naming

### Constructor Intermediate Variable: `q`

All `New*` constructors use `q` for the intermediate variable:

```go
func NewInteger(v int64) *Integer {
    q := &Integer{Value: v}
    return q
}

func NewCons(car, cdr Value) *Pair {
    q := &Pair{car: car, cdr: cdr}
    return q
}
```

### Standard Variable Names

| Name | Usage |
|------|-------|
| `i`, `j` | Loop counters |
| `n` | Count, length, or bytes read |
| `l` | Length variable |
| `v` | Temporary value in type switches |
| `ok` | Boolean result from type assertions |
| `err` | Error values |
| `o` | "Other" operand in binary operations |
| `pr` | Pair reference in list traversal |
| `curr` | Current position in iteration |
| `bs` | Byte slice |
| `r` | Radix (in tokenizer) |
| `n`, `m` | Count of items |
| `ctx` | first parameter if type `context.Context` |
| `ctctx` | parameter of type `CompileTimeCallContext` |
| `mc` | parameter of type `MachineContext` |
| `etctx` | parameter of type `ExpandTimeCallContext` |
| `cpctx` | parameter of type `captureContext` |

```go
// Type assertion pattern
if other, ok := v.(*Integer); ok { ... }

// Binary operation pattern
func (p *Integer) Add(o Number) Number { ... }

// List iteration pattern
for curr := q; curr != EmptyList; curr = curr.Cdr().(*Pair) { ... }
```

### Test Variable Names

Table-driven test structs use numbered inputs/outputs:

```go
tcs := []struct {
    in0   Value   // First input
    in1   Value   // Second input
    out   bool    // Expected output
    err0  error   // Expected error (if any)
}{
    {in0: NewInteger(1), in1: NewInteger(1), out: true},
}
```

## Function Naming

### Prefixes

| Prefix | Meaning | Returns |
|--------|---------|---------|
| `New` | Constructor | `*Type` |
| `Is` | Predicate | `bool` |
| `Set` | Setter | Usually nothing |
| `Get` | Getter | Value |
| `May` | Optional operation (may not perform) | Varies |
| `Must` | Required operation (panics on failure) | Varies |
| `As` | Value conversion | A type other than the reciver |

### Constructor Variants

When multiple constructors exist, use `From` suffix to indicate source:

```go
NewInteger(v int64) *Integer
NewBigIntegerFromInt64(v int64) *BigInteger
NewBigIntegerFromString(s string, base int) *BigInteger
NewRationalFromBigInt(num, denom *big.Int) *Rational
NewComplexFromParts(realPart, imagPart float64) *Complex
```

### Accessor Methods

| Method | Purpose |
|--------|---------|
| `Datum()` | Returns underlying data representation |
| `Value()` | Direct property access |
| `Car()`, `Cdr()` | List accessors (Lisp convention) |
| `String()` | Go standard stringer interface |
| `SchemeString()` | Scheme-formatted string representation |

## Error Handling

### Error Types

All errors use the `values` package error types:

1. **Static errors** - Pre-created constants for common errors:
   ```go
   var (
       ErrNotANumber     = NewStaticError("not a number")
       ErrDivisionByZero = NewStaticError("division by zero")
       ErrNoSuchBinding  = NewStaticError("no such binding")
   )
   ```

2. **Foreign errors** - New error instances, foreign to Scheme:
   ```go
   values.NewForeignError("custom error message")
   ```

3. **Wrapped errors** - Adding context to existing errors:
   ```go
   values.WrapForeignErrorf(err, "context: %s", details)
   values.WrapForeignErrorf(ErrNoSuchBinding, "variable %q not found", name)
   ```

4. **Native errors** - Native means Scheme error objects.
   ```go
   return values.NewError( "something went wrong" )
   ```

### Error Pattern Preference

Prefer in this order:
1. Wrap existing static error with context (`WrapForeignErrorf`)
2. Create new error (`NewForeignError`)
3. Define static error constant (`NewStaticError`)

## Type Declarations

### Struct Definition Order

1. Type declaration
2. Interface assertion checks
3. Constructor(s)
4. Accessor methods
5. Operator methods
6. Interface implementation methods

```go
// 1. Type declaration
type Integer struct {
    Value int64
}

// 2. Interface assertion checks
var (
    _ Value  = (*Integer)(nil)
    _ Number = (*Integer)(nil)
)

// 3. Constructor
func NewInteger(v int64) *Integer {
    q := &Integer{Value: v}
    return q
}

// 4. Accessor
func (p *Integer) Datum() int64 {
    return p.Value
}

// 5. Operators
func (p *Integer) Add(o Number) Number { ... }

// 6. Interface methods
func (p *Integer) IsVoid() bool { return false }
func (p *Integer) EqualTo(v Value) bool { ... }
func (p *Integer) SchemeString() string { ... }
```

### Sentinel Values

Singletons use unexported types with exported variables:

```go
type voidType struct{}  // unexported
var Void Value = voidType{}  // exported singleton

type eofType struct{}
var EofObject Value = eofType{}

var EmptyList = NewCons(nil, nil)
```

### Type Aliases for Clarity

Use type aliases to distinguish similar numeric types:

```go
type LiteralIndex int
type KeywordIndex int
type LocalIndex []int
type GlobalIndex []int
```

## Test Conventions

### Test Function Naming

Pattern: `Test{Type}_{Method}` or `Test{Type}_{Behavior}`

```go
func TestInteger_EqualTo(t *testing.T) { ... }
func TestInteger_Add(t *testing.T) { ... }
func TestVector_Creation(t *testing.T) { ... }
func TestPair_ForEach(t *testing.T) { ... }
```

### Table-Driven Tests

```go
func TestInteger_Add(t *testing.T) {
    tcs := []struct {
        in0 *Integer
        in1 Number
        out Number
    }{
        {in0: NewInteger(1), in1: NewInteger(2), out: NewInteger(3)},
        {in0: NewInteger(-1), in1: NewInteger(1), out: NewInteger(0)},
    }
    for _, tc := range tcs {
        result := tc.in0.Add(tc.in1)
        qt.Assert(t, result, SchemeEquals, tc.out)
    }
}
```

### Testing Framework

Use `quicktest` (`qt`) with `SchemeEquals` custom checker:

```go
qt.Assert(t, actual, qt.Equals, expected)
qt.Assert(t, actual, SchemeEquals, expected)  // For Value comparison
qt.Assert(t, err, qt.IsNil)
qt.Assert(t, result, qt.IsNotNil)
```

## File Organization

### File Naming

- One primary type per file: `{type_lowercase}.go`
- Tests in parallel: `{type_lowercase}_test.go`
- Related types grouped by prefix: `character_input_port.go`, `character_output_port.go`
- Utility functions: `utils.go`
- Error definitions: `error.go`, `foreign_error.go`

### Package Documentation

Each package contains a `CLAUDE.md` file with:
- Package overview
- File listing with descriptions
- Key types and their relationships
- Testing instructions
- Gotchas and common pitfalls

## Import Organization

Group imports with internal packages first:

```go
import (
    "wile/environment"
    "wile/syntax"
    "wile/values"

    "context"
    "fmt"
    "io"
    "strings"
)
```

## Comments

### Package Documentation

Use structured comments with Markdown-style headings:

```go
// Package tokenizer implements lexical analysis for Scheme source code.
//
// # Token Types
//
// Tokens are categorized by TokenizerState values:
//   - Delimiters: OpenParen, CloseParen, EmptyList
//   - Literals: Integer, Float, String, Character
```

### Method Documentation

- Minimal documentation for obvious methods
- Document non-obvious behavior or edge cases
- No doc comments required for standard interface implementations

### Inline Comments

- Sparse - code should be self-documenting
- Use for complex logic or non-standard patterns
- Always include space after `//`

## Constants

### Message Constants

Group related message constants:

```go
const (
    MessageRuneError       = "rune error"
    MessageExpectingNumber = "expecting number"
    MessageExpectingToken  = "expecting token"
)
```

### Enum-like Constants

Use `iota` for sequential values:

```go
type TokenizerState int

const (
    TokenizerStateUnknown TokenizerState = iota
    TokenizerStateEOF
    TokenizerStateOpenParen
    // ...
)
```

## Method Interface Order

Standard interface methods should appear in this order:

1. `Datum()` - Access underlying data
2. Type-specific operations
3. `IsVoid() bool`
4. `EqualTo(Value) bool`
5. `SchemeString() string`
6. `String() string` (if different from SchemeString)

## Return Values

### No Named Returns

Functions use unnamed return values:

```go
// Correct
func (p *Integer) Add(o Number) Number {
    return NewInteger(p.Value + o.(*Integer).Value)
}

// Avoid
func (p *Integer) Add(o Number) (result Number) {
    result = NewInteger(p.Value + o.(*Integer).Value)
    return
}
```

### Multiple Returns

Standard patterns:

```go
(Value, error)           // Value with possible error
(Value, bool)            // Value with found/ok flag
(*Type, bool)            // Concrete type with success flag
```

## Miscellaneous

### Context Parameter

When needed, `ctx context.Context` is always the first parameter after receiver:

```go
func (p *MachineContext) Run(ctx context.Context) (Value, error) { ... }
```

### Temporary Variable Names

Generated temporary variables use the pattern `__T_<base32>`:

```go
func NewTemporaryVariableName() string {
    return "__T_" + base32Encode(counter)
}
```

### Avoid

- Factory naming (`Create*`, `Make*` for types)
- Hungarian notation
- Excessive abbreviations beyond established patterns
- Documentation comments on trivial methods

