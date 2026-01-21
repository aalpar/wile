# CLAUDE.md

Package `primitives` implements all R7RS Scheme built-in procedures.

## Purpose

237 primitive implementations as Go foreign functions, organized by:
- One file per primitive: `prim_<name>.go`
- Helper modules for shared patterns
- Comprehensive test coverage

## Key Files

| File | Purpose |
|------|---------|
| `state.go` | Global I/O port state, weak-referenced caches |
| `numeric_fold.go` | Variadic arithmetic helpers (+, -, *, /) |
| `numeric_compare.go` | Comparison chain helpers (=, <, >, <=, >=) |
| `numeric_extremum.go` | min/max with exactness contagion and NaN handling |
| `integer_fold.go` | gcd/lcm with BigInteger support |
| `numeric_unary_ops.go` | Unary ops (abs, floor, ceiling, etc.) with BigInteger |
| `char_compare.go` | Character comparison helper (`charCompareVariadic`) |
| `string_compare.go` | String comparison helper (`stringCompareVariadic`) |
| `prim_char_ci_variadic.go` | Case-insensitive char comparisons (char-ci=?, etc.) |
| `prim_string_ci_variadic.go` | Case-insensitive string comparisons (string-ci=?, etc.) |
| `call_with_file.go` | File I/O wrapper with cleanup |
| `eqv.go` | Equality semantics for memv/assv |
| `to_complex128.go` | Complex number conversion helpers for transcendental functions |

## Primitive Implementation Pattern

```go
func PrimXxx(ctx context.Context, mc *machine.MachineContext) error {
    arg := mc.Arg(0)
    typed, ok := arg.(*values.SomeType)
    if !ok {
        return values.WrapForeignErrorf(values.ErrSomeError, "...")
    }
    // ... work ...
    mc.SetValue(result)
    return nil
}
```

## Registration

Primitives listed in `runtime/environment_tiny.go`:
```go
var runtimePrimitives = []PrimitiveSpec{
    {Name: "+", ParamCount: 0, IsVariadic: true, Impl: primitives.PrimAdd},
    // ...
}
```

## Gotchas

- **Variadic args as Pair**: Rest arguments passed as linked list in last parameter
- **Identity elements**: `(+)` returns 0, `(*)` returns 1
- **Append complexity**: Uses vector intermediate for O(n) despite linked lists
- **Continuation escape**: `call/cc` copies continuation, uses sentinel error
- **Arguments via mc.Arg()**: Not via environment frame bindings
- **Weak caching**: Tokenizers/parsers cached per port with weak pointers
- **Pair methods**: Use `pair.Car()` and `pair.Cdr()` - these are methods, not fields
- **Character `#\x` in tests**: Avoid `#\x` in test code as it starts a hex escape sequence; use `#\a` or other letters instead
- **Simple errors**: Use `values.NewForeignError("message")` for validation errors without predefined constants
- **String Unicode escapes**: Use `\xHEX;` format (R7RS) or embed Unicode directly; `\U` escape is not valid Scheme syntax
- **integer->char validation**: Current implementation does not validate Unicode scalar values per R7RS; negative integers, surrogate values (D800-DFFF), and values > 10FFFF should error but currently don't
- **R7RS semantic differences**: See `plans/R7RS_SEMANTIC_DIFFERENCES.md` (at project root) for documented differences between implementation and R7RS specification (char-foldcase, string-foldcase, digit-value, etc.)

## Variadic Registration Patterns

The relationship between `ParamCount` and `IsVariadic` determines how arguments are passed:

| ParamCount | IsVariadic | Arg Access | Example |
|------------|------------|------------|---------|
| 1 | true | `mc.Arg(0)` = all args as Pair | `+`, `*` |
| 2 | true | `mc.Arg(0)` = first arg directly, `mc.Arg(1)` = rest as Pair | `-`, `/`, `char=?` |

For primitives requiring at least 2 arguments (like comparisons), use `ParamCount: 2, IsVariadic: true`.

## Testing

Uses quicktest with `runProgram()` and `runSchemeCode()` helpers. Table-driven tests cover operations, edge cases, and error conditions.

For tests involving exception handling or promises where infinite loops are possible, use `runSchemeCodeWithTimeout()` to prevent resource exhaustion.

**Important**: Do not remove or revert tests that conform to R7RS. If a test fails but correctly reflects R7RS behavior, the implementation must be fixed to conform to R7RS—not the test.

### Test File Organization

This package uses a mix of **thematic consolidation** and **individual test files**. With 237 primitive files, consolidation improves maintainability for related operations, while individual files provide clarity for distinct primitives:

| Test File | Tests For |
|-----------|-----------|
| `prim_arithmetic_test.go` | `+`, `-`, `*`, `/` and related |
| `prim_numeric_predicate_test.go` | `integer?`, `real?`, `number?`, etc. |
| `prim_numeric_compare_test.go` | `=`, `<`, `>`, `<=`, `>=` |
| `prim_char_test.go` | Character operations |
| `prim_string_test.go` | String operations |
| `prim_list_test.go` | List/pair operations |
| `prim_vector_test.go` | Vector operations |
| `prim_trig_test.go` | Transcendental functions (exp, log, sin, cos, tan, asin, acos, atan) |
| `prim_division_test.go` | floor/, truncate/, floor-quotient, etc. |
| `prim_*_extra_test.go` | Additional coverage for specific primitives |

**Equality & Control Flow (individual files):**
| Test File | Tests For |
|-----------|-----------|
| `prim_eq_q_test.go` | `eq?` identity comparison |
| `prim_eqv_q_test.go` | `eqv?` equivalence |
| `prim_equal_q_test.go` | `equal?` deep comparison |
| `prim_apply_test.go` | `apply` |
| `prim_map_test.go` | `map` |
| `prim_for_each_test.go` | `for-each` |
| `prim_values_test.go` | `values` |
| `prim_call_with_values_test.go` | `call-with-values` |
| `prim_dynamic_wind_test.go` | `dynamic-wind` |
| `prim_not_test.go` | `not` |

**I/O Operations (individual files):**
| Test File | Tests For |
|-----------|-----------|
| `prim_call_with_input_file_test.go` | `call-with-input-file` |
| `prim_call_with_output_file_test.go` | `call-with-output-file` |
| `prim_with_input_from_file_test.go` | `with-input-from-file` |
| `prim_with_output_to_file_test.go` | `with-output-to-file` |
| `prim_open_binary_input_file_test.go` | `open-binary-input-file` |
| `prim_open_binary_output_file_test.go` | `open-binary-output-file` |
| `prim_write_simple_test.go` | `write-simple` |
| `prim_write_shared_test.go` | `write-shared` |
| `prim_eof_object_test.go` | `eof-object`, `eof-object?` |
| `prim_bytevector_port_test.go` | bytevector port operations |
| `prim_current_port_test.go` | `current-input-port`, `current-output-port` |
| `prim_close_port_test.go` | `close-port` |
| `prim_io_errors_test.go` | I/O error conditions |

**Exception Handling & Promises (individual files):**
| Test File | Tests For |
|-----------|-----------|
| `prim_exception_test.go` | `with-exception-handler`, `raise`, `raise-continuable`, `error`, `guard`, error-object accessors |
| `prim_promise_test.go` | `promise?`, `make-promise`, `force`, `delay-force` |
| `prim_promise_extra_test.go` | Additional promise tests (memoization, edge cases) |
| `prim_parameter_test.go` | `make-parameter`, `parameterize` |

**Eval, Syntax & Expansion (consolidated files):**
| Test File | Tests For |
|-----------|-----------|
| `prim_eval_env_test.go` | `eval`, `interaction-environment`, `scheme-report-environment`, `null-environment` |
| `prim_env_extra_test.go` | Additional environment tests |
| `prim_identifier_test.go` | `identifier?`, `bound-identifier=?`, `free-identifier=?`, `datum->syntax`, `syntax->datum`, `expand`, `expand-once`, `compile`, `make-compile-time-value` |

**System & Concurrency (individual files):**
| Test File | Tests For |
|-----------|-----------|
| `prim_file_env_test.go` | `file-exists?`, `get-environment-variable`, `get-environment-variables`, `command-line` |
| `prim_delete_load_test.go` | `delete-file`, `load` |
| `prim_misc_test.go` | `features`, `current-second`, `current-jiffy`, `jiffies-per-second` |
| `prim_srfi18_time_test.go` | `current-time`, `time?`, `time->seconds`, `seconds->time` |
| `prim_thread_test.go` | `make-thread`, `thread?`, `thread-name`, `thread-specific`, `thread-start!`, `thread-join!`, etc. |
| `prim_mutex_test.go` | `make-mutex`, `mutex?`, `mutex-lock!`, `mutex-unlock!`, etc. |
| `prim_condvar_test.go` | `make-condition-variable`, `condition-variable?`, `condition-variable-signal!`, etc. |
| `prim_channel_test.go` | `make-channel`, `channel?`, `channel-send!`, `channel-receive`, etc. |
| `prim_sync_test.go` | WaitGroup, RWMutex, Once, Atomic primitives |
| `prim_void_q_test.go` | `void?` predicate |

When adding new primitive tests:
- Check if a thematic test file exists for the category
- If so, add tests there rather than creating a new file
- For distinct primitives (especially I/O and control flow), individual test files are preferred

### Error Testing Pattern

Use `schemeCodeErrorTestCase` for testing error conditions:

```go
func TestXxxErrors(t *testing.T) {
    tcs := []schemeCodeErrorTestCase{
        {name: "wrong type - integer", code: `(xxx 42)`},
        {name: "wrong type - string", code: `(xxx "hello")`},
    }
    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            _, err := runSchemeCode(t, tc.code)
            qt.Assert(t, err, qt.IsNotNil)
        })
    }
}
```

Error tests should cover:
- Wrong argument types (integer when expecting char, string when expecting symbol, etc.)
- Boundary conditions (negative indices, out-of-bounds access)
- Invalid values (per R7RS specification)

## R7RS Numeric Tower

Numbers form a hierarchy where each level is a subset of the one above:

```
number (z)
  └─ complex (z)
       └─ real (x)
            └─ rational (q)
                 └─ integer (n)
```

**Critical**: Each level includes both **exact** and **inexact** representations:
- `7` is an exact integer
- `7.0` is an inexact integer (satisfies `integer?` because `(= 7.0 (round 7.0))`)
- Both are valid arguments where "integer" (n) is required

### Parameter Type Notation

| Symbol | Type | Go Types |
|--------|------|----------|
| **z** | complex (any number) | Integer, BigInteger, Float, BigFloat, Rational, Complex |
| **x** | real | Integer, BigInteger, Float, BigFloat, Rational |
| **q** | rational | Integer, BigInteger, BigFloat, Rational |
| **n** | integer (exact OR inexact) | Integer, BigInteger, Float (if integral), BigFloat (if integral) |
| **k** | exact integer only | Integer, BigInteger |

**Note**: BigFloat (`#m` prefix) is always inexact and always finite (no Inf/NaN support).

## R7RS Arithmetic Primitive Type Requirements

### Accept ANY number (z - complex)

| Procedure | Notes |
|-----------|-------|
| `+`, `-`, `*`, `/` | Basic arithmetic; `(+)` → 0, `(*)` → 1 |
| `sqrt` | Returns complex for negative reals |
| `expt`, `square` | Exponentiation |
| `exp`, `log`, `sin`, `cos`, `tan`, etc. | Transcendental functions |

### Accept REAL numbers only (x)

| Procedure | Notes |
|-----------|-------|
| `max`, `min` | Requires ≥1 arg; inexact contagion applies |
| `abs` | Absolute value |
| `floor`, `ceiling`, `truncate`, `round` | Return integers |
| `rationalize` | `(rationalize x tolerance)` |

### Accept INTEGERS only (n) - includes inexact integers like 7.0

| Procedure | Notes |
|-----------|-------|
| `quotient`, `remainder`, `modulo` | `(quotient 7.0 3.0)` → `2.0` is valid |
| `gcd`, `lcm` | `(gcd)` → 0, `(lcm)` → 1; always non-negative |
| `floor/`, `truncate/`, etc. | Return 2 values |

### Accept EXACT integers only (k)

| Procedure | Notes |
|-----------|-------|
| `exact-integer-sqrt` | Returns 2 exact integers; error on inexact input |

## Exactness Contagion Rules

1. **General rule**: Operations return inexact results when given any inexact arguments
2. **Exception**: May return exact if provably unaffected by inexactness
3. **Specific cases**:
   - `(* 0 x)` → exact 0 (exact zero dominates)
   - `(* 0.0 x)` → inexact 0.0
   - `(* 0 +inf.0)` → may return 0, +nan.0, or error (implementation-defined)
   - `(max 3 2.5)` → `3.0` (inexact, because 2.5 is inexact)
   - `(+ 1 1/2)` → `3/2` (exact)
   - `(+ 1 0.5)` → `1.5` (inexact)

## Helper Functions for R7RS Compliance

### Integer Extraction (`prim_quotient.go`)

```go
// extractInteger extracts an integer value from Integer, BigInteger, or Float (if integral).
// Returns (int64Value, bigIntValue, isInexact, error).
extractInteger(v values.Value, name string) (int64, *big.Int, bool, error)
```

Use for primitives accepting integers (n) - handles exact and inexact integers uniformly.

### Exactness Tracking (`numeric_extremum.go`)

```go
// isInexact returns true if the number is inexact (Float, BigFloat, or Complex)
isInexact(n values.Number) bool

// maybeToInexact converts exact to Float if hasInexact is true
maybeToInexact(n values.Number, hasInexact bool) values.Value
```

### BigFloat Integer Checking (`prim_odd_q.go`, `prim_even_q.go`)

For `odd?` and `even?` with BigFloat, use `v.BigFloatValue().IsInt()` to verify the value
represents an integer before checking parity.

Use for implementing exactness contagion in primitives like max/min.

### Complex Number Conversion (`to_complex128.go`)

```go
// ToComplex128 converts any Scheme number to Go complex128
ToComplex128(v values.Value) (complex128, error)

// ComplexOrFloat returns Float if imaginary part is zero, otherwise Complex
ComplexOrFloat(c complex128) values.Value
```

Use for transcendental functions that accept complex inputs per R7RS.

## Transcendental Functions - Implementation Details

All transcendental functions (`exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`) use Go's `math/cmplx` package and accept complex number inputs per R7RS.

### Branch Cut Conventions

R7RS does **not** mandate specific branch cut conventions for complex functions - these are implementation-defined. This implementation uses Go's `math/cmplx` conventions:

| Function | Go's Branch Cut Convention | Example |
|----------|---------------------------|---------|
| `asin(z)` | Cut along real axis outside [-1, 1] | `(asin 2)` → `1.5708+1.3170i` |
| `acos(z)` | Cut along real axis outside [-1, 1] | `(acos 2)` → `0-1.3170i` |
| `atan(z)` | Cut along imaginary axis outside [-i, i] | `(atan 0+2i)` → `-1.5708+0.5493i` |
| `log(z)` | Cut along negative real axis | `(log -1)` → `0+πi` |

**Note**: Other Scheme implementations may use different branch cuts. Both conventions are mathematically valid - they represent different branches of multivalued complex functions.

### Special Value Handling

R7RS does not specify behavior for infinity/NaN inputs to transcendental functions. This implementation:

| Expression | Result | Notes |
|------------|--------|-------|
| `(sin +inf.0)` | `+nan.0` | Mathematically undefined |
| `(cos +inf.0)` | `+nan.0` | Mathematically undefined |
| `(tan +inf.0)` | `+nan.0` | Mathematically undefined |
| `(log 0)` | `-inf.0` | Limit as x→0⁺ |
| `(log -1)` | `0+πi` | Complex per R7RS |
| `(asin 2)` | Complex | Per R7RS (not NaN) |
| `(exp +inf.0)` | `+inf.0` | Correct limit |
| `(exp -inf.0)` | `0.0` | Correct limit |

### NaN Propagation

When Go's `cmplx` functions return `NaN+NaNi` (e.g., `sin(+inf)`), `ComplexOrFloat` returns `Float(NaN)` rather than `Complex(NaN+NaNi)`. This preserves real-valued semantics for real inputs with undefined results.

### Two-Argument atan (atan2)

The two-argument form `(atan y x)` computes the angle from the positive x-axis to point (x, y). Per R7RS, this form only accepts **real** arguments (not complex). Uses `math.Atan2` directly.

## R7RS Normative Sources

| Source | URL |
|--------|-----|
| R7RS-small PDF | https://small.r7rs.org/attachment/r7rs.pdf |
| R7RS Corrected (HTML) | https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html |
| SRFI-141 (Integer Division) | https://srfi.schemers.org/srfi-141/srfi-141.html |
| Division Spec Wiki | https://small.r7rs.org/wiki/DivisionRiastradh/ |
| R7RS-large Wiki (in progress) | https://codeberg.org/scheme/r7rs/wiki |
