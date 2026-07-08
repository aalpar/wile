# Coding Style Guide

This document describes the coding conventions used throughout the Wile Scheme interpreter codebase.

Always use sentinel error wrapping patterns (not `fmt.Errorf`) per project conventions. Run `go vet` and `golangci-lint run` before committing.

## Functions

Never write single-line functions. Always spread function bodies across multiple lines, even for simple implementations.

### Early Exit From Conditionals (Keep Indentation Shallow)

**Invert the guard, exit early, and un-indent the primary path.** When an
`if`/`else` splits into an exceptional (or trivial) branch and a main branch,
handle the exceptional branch first with an early `return` — or `continue` /
`break` inside a loop — drop the `else`, and let the main logic continue at the
outer indentation level. Indentation should track genuine logical depth, not the
number of conditions checked along the way.

The same move applies in three places:

1. **Function entry** — guard clauses for preconditions, known failure modes of
   later calls, and trivial/edge cases.
2. **Loop bodies** — `continue` past the cases to skip instead of wrapping the
   body in an `if`.
3. **Any `if cond { long } else { short-exit }`** — invert to
   `if !cond { short-exit }`, then write the long body flat.

**A: Nested `if`/`else` (avoid)** — the happy path is buried, and every check
adds a level of indentation:

```go
func processData(value int, settings map[string]bool) error {
	if value > 0 {
		if settings != nil {
			if settings["enabled"] {
				fmt.Printf("processing value: %d\n", value)
				return nil
			} else {
				return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "processData: settings not enabled")
			}
		} else {
			return werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "processData: settings is nil")
		}
	} else {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "processData: value must be positive")
	}
}
```

**B: Guard clauses, early exit (prefer)** — each failure returns immediately, so
the main logic stays flat and reads top to bottom. Check preconditions and known
failure modes of later calls first; dispose of trivial and edge cases before any
real computation:

```go
func processData(value int, settings map[string]bool) error {
	if value <= 0 {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "processData: value must be positive")
	}
	if settings == nil {
		return werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "processData: settings is nil")
	}
	if !settings["enabled"] {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "processData: settings not enabled")
	}

	// happy path, clear of indentation
	fmt.Printf("processing value: %d\n", value)
	return nil
}
```

Trivial cases exit first, before the real work:

```go
func mult(a, b int) int {
	if a == 0 || b == 0 {
		return 0
	}
	return a * b
}
```

Inside a loop, `continue` past the cases to skip rather than nesting the body
under a filter condition:

```go
// Avoid — body indented under the filter
for _, e := range entries {
	if e.Enabled {
		process(e)
	}
}

// Prefer — skip early, body stays flat
for _, e := range entries {
	if !e.Enabled {
		continue
	}
	process(e)
}
```

#### Advantages

- **Readability**: the primary execution flow (the "happy path") reads linearly.
- **Reduced nesting**: avoids "arrow code," where indentation grows with each
  condition checked.
- **Maintainability**: a validation check can be added or removed without
  re-indenting the main logic.
- **Idiomatic Go**: encouraged in the official
  [Effective Go](https://go.dev/doc/effective_go); the standard library uses this
  pattern pervasively for error handling.

#### When Not To Invert

Early exit is for *asymmetric* branches — one short/exceptional, one primary.
When both arms are genuine, comparable alternatives (a true two-way choice), an
`if`/`else` is honest and inverting it only hides the symmetry. And a function
that opens with a wall of a dozen guards is a signal to split it, not evidence
that guard clauses scale without limit.

## Return Values

| Letter | Usage                                                                                                                                                                                          |
|--------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `q`    | Variable name for value that is eventually returned. Only used when returning single value or returning two values value, the first value being `q` and the second value being an `error` type |

## Receiver Naming

All method receivers use single-letter names:

| Letter | Usage                                                     |
|--------|-----------------------------------------------------------|
| `p`    | All receivers that are not compile related                |
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
| `i`, `j`, `k` | Loop counters |
| `n` | Count, length, or bytes read |
| `l` | Length variable |
| `k` | Length parameter or working value length |
| `v` | Temporary value in type switches |
| `pr` | Pair |
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
| `As` | Value conversion | A type other than the receiver |

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

### Loss-Signal Suffixes

Numeric conversions in Wile fall into a small lattice: some conversions
preserve every signal the underlying representation can produce, some
deliberately discard signals that the caller cannot act on, and some refuse
to discard signals at all. Four suffixes name where a method or function
sits in that lattice. Use them consistently — the name is the contract.

| Suffix | Returns | Caller contract | Use when |
|--------|---------|-----------------|----------|
| `*WithAccuracy` | `(value, big.Accuracy, ...)` | Caller MAY act on the accuracy/loss signal | The signal exists in the source representation and the API is the bridge that surfaces it |
| `*Truncated` | bare `value` | Caller HAS opted out of the signal by name | Downstream code inherently cannot use the signal (`math.Sin` inputs, FNV hash seeds, transcendental coercions where the receiver is already inexact) |
| `*Lossy` | bare `value` | Caller HAS acknowledged that precision loss is acceptable | The function is reached on a hot path where the caller has already proved the precision drop is irrelevant (IEEE 754 Inf/NaN guards, inexact-only arithmetic) |
| `*Lossless` | `(value, error)` | Function REFUSES to drop information | Strict callers (FFI strict mode, conversion validators) need a hard error rather than a silent rounding; returns `werr.ErrLossyConversion` (wrapped) when the source would round |

**Examples**:

```go
// *WithAccuracy — bridges Go big.Accuracy to the caller
func (p *BigFloat) Float64WithAccuracy() (float64, big.Accuracy)
func ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool, error)
func ToComplex128WithAccuracy(n Number) (Complex128Result, error)

// *Truncated — name warns the reader at every call site
func (p *BigFloat) Float64Truncated() float64
func (p *Rational) Float64Truncated() float64

// *Lossy — hot-path lossy reduction, contract documented at the function
func NumberToComplex128Lossy(n Number) complex128

// *Lossless — refuses the lossy path
func ToFloat64Lossless(n Number) (float64, error)
func ToComplex128Lossless(n Number) (complex128, error)
```

**Rationale**: the previous `(*BigFloat).Float64()` was indistinguishable
at the call site from a stdlib `(*big.Float).Float64()`, yet it silently
dropped Go's `big.Accuracy`. Every caller had to read the body to know
whether the discard was deliberate or a bug. Renaming the method to
`Float64Truncated()` and adding `Float64WithAccuracy()` forced every site
to declare its intent — a swap-bug surface eliminated by name.

**Pairing rule**: a `*Truncated` method MUST coexist with a
`*WithAccuracy` companion on the same receiver. The truncated form is the
opt-out; the with-accuracy form is the default. Never offer a `*Truncated`
without its companion — the caller has no escape hatch.

**Sentinels**: `*Lossless` functions return `werr.ErrLossyConversion`
(wrapped) when the conversion would round; the wrap message names the
direction (`"%T rounded %s (lost precision)"`).

### Registry Lookup Naming

Functions that look up entries in a typed registry should name the kind of
entry in the function name, even when the package has only one registry
today. Prefer `LookupNumericSpec(kind)` over a bare `Lookup(kind)`.

**Rationale**: a bare `Lookup` in package `values` reads ambiguously the
moment a second registry appears (`values.Lookup(symKey)` for `*Symbol`?
`values.Lookup(hashtableKey)`?). The bare name optimizes for the current
state of the package, not for what a reader of a single line of code can
infer. Naming the entry type makes the call site self-describing:
`values.LookupNumericSpec(KindInteger)` tells the reader what is being
looked up and what shape comes back, with zero context from the
surrounding code.

The cost — one extra noun at the call site — is paid by the caller, where
the cost is small. The benefit — unambiguous reading — is collected by
every reader of every call site.

**Pattern**:

```go
// Correct
func LookupNumericSpec(kind NumericKind) *NumericTypeSpec
func LookupPromotionEntry(src, dst NumericKind) *PromotionEntry

// Avoid
func Lookup(kind NumericKind) *NumericTypeSpec  // ambiguous at call site
```

Apply the same rule to register-side helpers: `registerNumericSpec`,
not `register`.

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

All Go-side errors use the `werr` package (`werr/werr.go`). Scheme-level error objects are in `values`.

1. **Static errors** - Pre-created sentinel constants for `errors.Is` matching:
   ```go
   var (
       ErrNotANumber     = werr.NewStaticError("not a number")
       ErrDivisionByZero = werr.NewStaticError("division by zero")
       ErrNoSuchBinding  = werr.NewStaticError("no such binding")
   )
   ```

2. **Foreign errors** - New error instances with format string:
   ```go
   werr.NewForeignErrorf("custom error: %s", detail)
   ```

3. **Wrapped errors** - Adding context to existing sentinels:
   ```go
   werr.WrapForeignErrorf(werr.ErrNotANumber, "add: expected number but got %T", arg)
   werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "variable %q not found", name)
   ```

4. **Native errors** - Scheme error objects (R7RS `error` procedure), for
   *user-facing* failures a Scheme `guard` clause would catch:
   ```go
   values.NewErrorObject("something went wrong", irritant1, irritant2)
   // Surfacing a Go failure: keep the sentinel as cause (errors.Is still matches)
   // and the offending value as an irritant (a handler can dispatch on it):
   values.NewErrorObjectWithCause("add: expected a number", werr.ErrNotANumber, arg)
   // read/file: NewReadError / NewFileError / NewErrorObjectWithCauseAndKind carry
   // the kind so read-error? / file-error? match.
   ```
   Irritants and `kind` are Scheme `Value`s, so they attach only at or above the
   `values` layer — at the primitive, not in `werr` (which must not depend on
   `values`). See *Error Message Pattern* under Primitive Implementation Patterns.

### Error Comparison

**Always** use `errors.Is` for sentinel error checks, never `==`:

```go
// Correct
if errors.Is(err, errNeedsBigInt) {
    return fallback()
}

// Avoid
if err == errNeedsBigInt {  // DON'T
    return fallback()
}
```

**Rationale**: `errors.Is` traverses the error wrapping chain, so it works correctly even when errors are wrapped with `WrapForeignErrorf`. Direct `==` comparison breaks silently if the error is ever wrapped.

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
4. Helpers, ordered by least dependant first
5. Accessor methods
6. Operator methods
7. Interface implementation methods

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
var EOFObject Value = eofType{}

type emptyListType struct{}
var EmptyList Value = emptyListType{}  // implements Tuple, NOT *Pair
```

### Type Aliases for Clarity

Use type aliases to distinguish similar types:

```go
type LiteralIndex int                // machine/native_template.go
type LocalIndex [2]int               // environment/local_index.go — [slot, depth] De Bruijn index
type GlobalIndex struct {            // environment/global_environment_frame.go
    Index *values.Symbol
    Env   *GlobalEnvironmentFrame
}
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
        qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
    }
}
```

### Testing Framework

Use `quicktest` (`qt`) with `valuestest.SchemeEquals` custom checker (import `"github.com/aalpar/wile/pkg/values/valuestest"`):

```go
qt.Assert(t, actual, qt.Equals, expected)
qt.Assert(t, actual, valuestest.SchemeEquals, expected)  // For Value comparison
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

Group imports with standard library first, then internal packages, then third-party:

```go
import (
    "context"
    "fmt"
    "io"
    "strings"

    "github.com/aalpar/wile/pkg/environment"
    "github.com/aalpar/wile/pkg/internal/syntax"
    "github.com/aalpar/wile/pkg/values"

    "github.com/jessevdk/go-flags"
)
```

## Comments

Comments explain *why*, not *what*. Non-obvious logic gets context; obvious code gets none.

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

**Promotion lattice** (see `values/promotion.go`): Exact×Exact stays exact (Integer→BigInteger→Rational). Exact×InexactReal promotes to BigFloat (never truncates to Float). Anything×Complex goes to BigComplex (except Float+Complex→Complex).

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
func PrimXxx(mc machine.CallContext) error {
    // 1. Extract arguments
    arg := mc.Arg(0)

    // 2. Type assertion and validation
    typed, ok := arg.(*values.SomeType)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrSomeError, "xxx: expected type but got %T", arg)
    }

    // 3. Computation
    result := compute(typed)

    // 4. Set result and return
    mc.SetValue(result)
    return nil
}
```

### Variadic Primitive Pattern

For primitives accepting variable arguments, use `values.Tuple` (not `*values.Pair`) for rest-arg traversal:

```go
// ParamCount: 2, IsVariadic: true
// mc.Arg(0) = first argument (direct)
// mc.Arg(1) = rest of arguments as Tuple (Pair or EmptyList)

func PrimXxxVariadic(mc machine.CallContext) error {
    first := mc.Arg(0)
    rest := mc.Arg(1)

    if values.IsEmptyList(rest) {
        mc.SetValue(first)
        return nil
    }

    tuple, ok := rest.(values.Tuple)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotAList, "xxx: expected list but got %T", rest)
    }

    result := first
    _, err := tuple.ForEach(mc.Context(), func(_ context.Context, _ int, _ bool, v values.Value) error {
        result = process(result, v)
        return nil
    })
    if err != nil {
        return err
    }
    mc.SetValue(result)
    return nil
}
```

### Comparison Chain Pattern

For variadic comparison primitives (=, <, >, etc.). Note: 2-arg comparisons are now promoted opcodes (Phase 2) and bypass this path entirely.

```go
func compareChain(ctx context.Context, first values.Value, rest values.Tuple, cmp func(a, b values.Number) bool) (bool, error) {
    prev, err := toNumber(first)
    if err != nil {
        return false, err
    }
    result := true
    _, err = rest.ForEach(ctx, func(_ context.Context, _ int, _ bool, v values.Value) error {
        next, err := toNumber(v)
        if err != nil {
            return err
        }
        if !cmp(prev, next) {
            result = false
        }
        prev = next
        return nil
    })
    return result, err
}
```

### Fold Pattern for Associative Operations

For variadic associative operations (+, *, gcd, lcm). Note: 2-arg +, -, *, / are now promoted opcodes (Phases 2-3) and bypass this path.

```go
func foldNumbers(ctx context.Context, identity values.Number, args values.Tuple, op func(a, b values.Number) values.Number) (values.Number, error) {
    result := identity
    _, err := args.ForEach(ctx, func(_ context.Context, _ int, _ bool, v values.Value) error {
        n, err := toNumber(v)
        if err != nil {
            return err
        }
        result = op(result, n)
        return nil
    })
    return result, err
}
```

### Error Message Pattern

Include the primitive name and expected vs actual type:

```go
// Good - includes context
return werr.WrapForeignErrorf(werr.ErrNotANumber, "add: expected number but got %T", arg)

// Good - specific to primitive
return werr.WrapForeignErrorf(werr.ErrBadIndex, "vector-ref: index %d out of bounds for vector of length %d", idx, len)

// Avoid - missing context
return werr.NewForeignErrorf("not a number")
```

Context is necessary but not sufficient. A primitive's argument/type errors are
*user-facing* — a Scheme programmer can `guard` them — and `%T` is lossy there: it
keeps only the Go *type*, leaving a handler nothing to dispatch on. When the
offending value is something a handler would inspect, construct the error object at
the primitive, where the value is still in hand, so it survives as an irritant.
`*values.NativeError` is both a Go `error` and a Scheme condition, so one call does it:

```go
// The "add: expected number" form above has context but is still lossy — arg's
// value never reaches Scheme. Carry the value as an irritant instead:
return values.NewErrorObjectWithCause("add: expected a number", werr.ErrNotANumber, arg)
```

That preserves everything the chain needs:
- the offending value is a real irritant (`error-object-irritants`, dispatchable in a `guard` clause);
- `errors.Is(err, werr.ErrNotANumber)` still matches — the sentinel is the wrapped cause (`Unwrap()`);
- the return value *is* the condition, so it rides the existing `error` signature to the handler with no extra plumbing.

For read/file failures use `NewReadError` / `NewFileError` (or
`NewErrorObjectWithCauseAndKind`) so the kind rides along and `read-error?` /
`file-error?` still match.

Scope and economy:
- Only user-facing failures become irritant-bearing conditions. Internal invariant violations ("impossible" states) stay `werr` or panic — a Scheme programmer should never be able to `guard` them.
- Keep call sites to one line: factor a thin helper (e.g. `typeError(name, want string, got values.Value) *values.NativeError`) so the sentinel/kind/irritant wiring lives in one place.
- When no live value is available to attach, format it into the message with `SchemeString()` (never `%T`) so the error is at least reconstructable from text — the fallback, not the goal.

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
        return nil, werr.WrapForeignErrorf(werr.ErrSomeError, "expected xxx but got %T", v)
    }
    return typed, nil
}
```

### Dispatch Table Arithmetic

Arithmetic is handled via pre-built dispatch tables in `values/promotion.go`. Each type has `[numKinds]func` arrays populated at init time. There are no manual `addParts`/`subtractParts` helpers — the dispatch table closures handle cross-type promotion automatically.

## Miscellaneous

### Context Parameter

When needed, `ctx context.Context` is always the first parameter after receiver:

```go
func parseImportSet(ctx context.Context, expr syntax.SyntaxValue) (*ImportSet, error) { ... }
```

### Temporary Variable Names

Generated temporary variables use the pattern `__T_<base32>`:

```go
func NewTemporaryVariableName() string {
    return "__T_" + base32Encode(counter)
}
```

### Builtin Shadowing

Never use Go builtin function names as local variables or parameters. Use these abbreviations:

| Builtin | Use Instead | Example |
|---------|-------------|---------|
| `real`  | `rel`       | `rel := real(v.Value)` |
| `imag`  | `iam`       | `iam := imag(v.Value)` |
| `copy`  | `cpy`       | `cpy := obj.Copy()` |

This applies to local variables, parameters, and named return values — not struct fields.

### Avoid

- Factory naming (`Create*`, `Make*` for types)
- Hungarian notation
- Excessive abbreviations beyond established patterns
- Documentation comments on trivial methods

## Scheme Docstring Conventions

Wile uses Guile-style docstrings: a string literal as the first expression in a
`define`/`lambda` body (when the body has >1 expression) is treated as
documentation. Retrieved at runtime via `(procedure-documentation proc)`.

### Structure

```scheme
(define (fold kons knil lis1 . lists)
  "Accumulate across LIS1 by applying KONS to each element.\n\nKONS receives the current element and accumulator. For multiple\nlists, KONS receives one element per list plus the accumulator.\nStops at the shortest list.\n\nExamples:\n  (fold + 0 '(1 2 3))      => 6\n  (fold cons '() '(1 2 3))  => (3 2 1)\n\nSee also: `fold-right', `reduce', `unfold'."
  ...)
```

### Rules

| Convention | Format | Example |
|------------|--------|---------|
| **First line** | Standalone summary sentence | `"Return the first element of LIST matching PRED."` |
| **Parameters** | UPPER CASE | `KONS`, `KNIL`, `LIS1` |
| **Cross-references** | Backtick + straight quote | `` `fold-right' `` |
| **Paragraphs** | Separated by `\n\n` | `"Summary.\n\nDetails."` |
| **Examples section** | `Examples:` header, indented code | `"Examples:\n  (foo 1 2)  => 3"` |
| **See also** | `See also:` at end, comma-separated | `` "See also: `bar', `baz'." `` |
| **Pre-formatted** | Indent with 2 spaces after `\n` | `"\n  (code here)"` |
| **Trailing period** | End sentences with periods, including the last line | — |
| **Math** | Self-contained, no assumed knowledge | Explain algebraic concepts inline |

**Note:** Backtick (`` ` ``) is literal inside Scheme double-quoted strings — do NOT
escape it with backslash. `\`` is an invalid R7RS escape sequence and will cause
a tokenizer error.

### Structured Metadata Sections

Docstrings may include metadata sections that the documentation system parses into
structured fields. These enable `,doc`, `,apropos`, and `,topics` to render
Scheme-defined procedures with the same rich output as Go-implemented primitives.

| Section | Format | Purpose |
|---------|--------|---------|
| `Parameters:` | Indented `name : type` lines | Parameter names and types |
| `Returns:` | Single type name on same line | Return type |
| `Category:` | Single category name on same line | Topic grouping for `,topics` |

Sections may appear in any order. Place metadata sections after the prose description
and before `Examples:` / `See also:` (which remain part of the prose text).

Type names must match the `ValueType.String()` vocabulary: `any`, `boolean`, `number`,
`integer`, `real`, `rational`, `complex`, `flonum`, `string`, `character`,
`symbol`, `byte`, `pair`, `list`, `vector`, `bytevector`, `hashtable`, `procedure`,
`port`, `input-port`, `output-port`, `textual-input-port`, `textual-output-port`,
`binary-input-port`, `binary-output-port`. Unknown names degrade gracefully to `any`.

Example with all sections:
```scheme
(define (map f lst)
  "Apply F to each element of LST, returning a list of results.\n\nParameters:\n  f : procedure\n  lst : list\nReturns: list\nCategory: lists\n\nSee also: `for-each', `vector-map'."
  ...)
```

Minimal metadata (category only — makes the procedure visible in `,topics`):
```scheme
(define (not x)
  "Return #t if X is #f, #f otherwise.\n\nCategory: predicates"
  (if x #f #t))
```

### When to Use Each Element

- **Every docstring**: First-line summary + UPPER CASE parameters
- **Non-trivial procedures**: Add a detail paragraph after the summary
- **Procedures with non-obvious behavior**: Add `Examples:` section
- **Procedures with related alternatives**: Add `See also:` line
- **Trivial accessors/wrappers**: First-line summary is sufficient
- **All procedures in bootstrap/stdlib**: Add `Category:` at minimum for `,topics` visibility
- **Procedures with typed contracts**: Add `Parameters:` and `Returns:` sections

### What Gets Docstrings

| Form | Eligible | Notes |
|------|----------|-------|
| `(define (name ...) body ...)` | Yes | Standard case |
| `(define name (lambda (...) body ...))` | Yes | Docstring in lambda body |
| `(define name (case-lambda ...))` | Yes | Docstring in first clause only |
| `define-syntax` | No | No mechanism yet (see TODO.md) |
| `define-record-type` | No | Generated accessors |
| `(define name value)` | No | No body |

