# Coding Style Guide

This document describes the coding conventions used throughout the Wile Scheme interpreter codebase.

## Return Values

| Letter | Usage                                                                                                                                                                                          |
|--------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `q`    | Variable name for value that is eventually returned. Only used when returning single value or returning two values value, the first value being `q` and the second value being an `error` type |

## Receiver Naming

All method receivers use single-letter names:

| Letter | Usage                                                     |
|--------|-----------------------------------------------------------|
| `p`    | Standard receiver                                         |
| `c`    | Compiler-related types (e.g., `*CompileTimeContinuation`) |

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

### Return Values: `q`

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
| `q` | Return value |
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

## Control Flow

### If Statement Assignments

**Never** combine assignment and comparison in a single `if` statement. Always separate them:

```go
// Correct - assignment on separate line
err := doSomething()
if err != nil {
    return err
}

// Correct - multiple assignments separated
result, err := process(input)
if err != nil {
    return nil, err
}

// Avoid - combined assignment and comparison
if err := doSomething(); err != nil {  // DON'T
    return err
}

// Avoid - combined with type assertion
if v, ok := x.(SomeType); ok {  // DON'T
    // ...
}
```

**Rationale:**
- Improves readability by keeping operations atomic
- Makes debugging easier (can set breakpoints on assignment)
- Maintains consistent code structure
- Variables are available in the outer scope when needed

**Exception:** Short-circuit boolean expressions are acceptable:
```go
if x != nil && x.IsValid() {  // OK - no assignment
    // ...
}
```

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

## Numeric Type Patterns

### Type Switch Case Ordering

When handling all numeric types in a type switch, use consistent ordering from most to least specific (integer tower ascending, then complex):

```go
switch v := o.(type) {
case *Integer:
    // Handle native int64
case *BigInteger:
    // Handle arbitrary precision exact integer
case *Float:
    // Handle native float64
case *BigFloat:
    // Handle arbitrary precision inexact float
case *Rational:
    // Handle exact rational
case *Complex:
    // Handle native complex128
case *BigComplex:
    // Handle arbitrary precision complex
default:
    // Not a number
}
```

**Rationale**: This ordering mirrors the numeric tower (integer → real → complex) and groups exact types (Integer, BigInteger, Rational) separately from inexact (Float, BigFloat, Complex, BigComplex partial).

### Zero Optimization Pattern

Short-circuit arithmetic operations when one operand is zero:

```go
func (p *SomeNumeric) Add(o Number) Number {
    if p.IsZero() {
        return o
    }
    switch v := o.(type) {
    case *Integer:
        if v.IsZero() {
            return p
        }
        // ... actual computation
    }
}
```

**Apply to**: `Add` (either operand zero), `Multiply` (either operand zero returns zero), `Subtract` (subtrahend zero returns minuend).

### Type Promotion Pattern

When arithmetic involves mixed types, promote to the more general type:

```go
// Integer + BigInteger → promote Integer to BigInteger
case *BigInteger:
    bi := NewBigIntegerFromInt64(p.Value)
    return bi.Add(v)

// Integer + Float → promote Integer to Float (inexact contagion)
case *Float:
    return NewFloat(float64(p.Value) + v.Value)

// Integer + Complex → promote Integer to Complex
case *Complex:
    return NewComplex(complex(float64(p.Value), 0) + v.Value)
```

**Promotion hierarchy**: Integer → BigInteger → Float → BigFloat → Rational → Complex → BigComplex

### Exactness Contagion Pattern

Operations involving inexact numbers return inexact results:

```go
// Exact + Exact = Exact
NewInteger(1).Add(NewInteger(2))  // → Integer(3)

// Exact + Inexact = Inexact
NewInteger(1).Add(NewFloat(2.0))  // → Float(3.0)

// Exception: exact zero dominates multiplication
NewInteger(0).Multiply(NewFloat(1.5))  // → Integer(0)
```

### Simplification Pattern

When a complex operation produces a real result (imaginary part zero), return the appropriate real type:

```go
func maybeSimplify(real, imag Number) Number {
    if imag.IsZero() {
        return real  // Return real type, not complex
    }
    return NewBigComplex(real, imag)
}
```

### Arithmetic Method Template

Standard structure for Number interface arithmetic methods:

```go
func (p *SomeType) Add(o Number) Number {
    // 1. Zero optimization for receiver
    if p.IsZero() {
        return o
    }
    // 2. Type switch with consistent ordering
    switch v := o.(type) {
    case *Integer:
        if v.IsZero() {
            return p
        }
        // Promote or compute
    case *BigInteger:
        // ...
    // ... other cases
    default:
        panic("unsupported type")
    }
}
```

## Primitive Implementation Patterns

### Standard Primitive Structure

All primitives follow this pattern:

```go
func PrimXxx(_ context.Context, mc *machine.MachineContext) error {
    // 1. Extract arguments
    arg := mc.Arg(0)

    // 2. Type assertion and validation
    typed, ok := arg.(*values.SomeType)
    if !ok {
        return values.WrapForeignErrorf(values.ErrSomeError, "xxx: expected type but got %T", arg)
    }

    // 3. Computation
    result := compute(typed)

    // 4. Set result and return
    mc.SetValue(result)
    return nil
}
```

### Variadic Primitive Pattern

For primitives accepting variable arguments:

```go
// ParamCount: 2, IsVariadic: true
// mc.Arg(0) = first argument (direct)
// mc.Arg(1) = rest of arguments as Pair

func PrimXxxVariadic(_ context.Context, mc *machine.MachineContext) error {
    first := mc.Arg(0)
    rest, ok := mc.Arg(1).(*values.Pair)
    if !ok {
        return values.WrapForeignErrorf(values.ErrBadSyntax, "xxx: invalid arguments")
    }

    result := first
    for curr := rest; curr != values.EmptyList; {
        next := curr.Car()
        result = process(result, next)
        cdr, ok := curr.Cdr().(*values.Pair)
        if !ok {
            break
        }
        curr = cdr
    }
    mc.SetValue(result)
    return nil
}
```

### Comparison Chain Pattern

For variadic comparison primitives (=, <, >, etc.):

```go
func compareChain(first values.Value, rest *values.Pair, cmp func(a, b values.Number) bool) (bool, error) {
    prev, err := toNumber(first)
    if err != nil {
        return false, err
    }
    for curr := rest; curr != values.EmptyList; {
        next, err := toNumber(curr.Car())
        if err != nil {
            return false, err
        }
        if !cmp(prev, next) {
            return false, nil
        }
        prev = next
        cdr, ok := curr.Cdr().(*values.Pair)
        if !ok {
            break
        }
        curr = cdr
    }
    return true, nil
}
```

### Fold Pattern for Associative Operations

For variadic associative operations (+, *, gcd, lcm):

```go
func foldNumbers(identity values.Number, args *values.Pair, op func(a, b values.Number) values.Number) (values.Number, error) {
    result := identity
    for curr := args; curr != values.EmptyList; {
        n, err := toNumber(curr.Car())
        if err != nil {
            return nil, err
        }
        result = op(result, n)
        cdr, ok := curr.Cdr().(*values.Pair)
        if !ok {
            break
        }
        curr = cdr
    }
    return result, nil
}
```

### Error Message Pattern

Include primitive name and expected vs actual type:

```go
// Good - includes context
return values.WrapForeignErrorf(values.ErrNotANumber, "add: expected number but got %T", arg)

// Good - specific to primitive
return values.WrapForeignErrorf(values.ErrBadIndex, "vector-ref: index %d out of bounds for vector of length %d", idx, len)

// Avoid - missing context
return values.NewForeignError("not a number")
```

## Helper Function Patterns

### Private Helper Naming

Helper functions private to a file use descriptive lowercase names:

```go
// In prim_xxx.go
func extractInteger(v values.Value, name string) (int64, *big.Int, bool, error)
func toBigFloat(n Number) *BigFloat
func maybeSimplify(real, imag Number) Number
```

### Conversion Helper Pattern

For type conversions that may fail:

```go
func toXxx(v values.Value) (*XxxType, error) {
    typed, ok := v.(*XxxType)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrSomeError, "expected xxx but got %T", v)
    }
    return typed, nil
}
```

### Part Accessor Helper Pattern

For operations that work on parts of composite types:

```go
// Generic helper that handles multiple types via type switch
func addParts(a, b Number) Number {
    switch va := a.(type) {
    case *BigInteger:
        switch vb := b.(type) {
        case *BigInteger:
            return va.Add(vb)
        case *BigFloat:
            return toBigFloat(va).Add(vb)
        }
    case *BigFloat:
        return va.Add(toBigFloat(b))
    }
    panic("unsupported part type")
}
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

