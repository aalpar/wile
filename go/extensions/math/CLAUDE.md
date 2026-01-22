# CLAUDE.md

Package `math` provides transcendental math functions.

## Purpose

- Transcendental functions (exp, log, sin, cos, tan, etc.)
- Rounding operations (floor, ceiling, truncate, round)
- Integer division (floor/, truncate/, quotient, remainder)
- Complex number operations
- Numeric predicates (finite?, infinite?, nan?)

## Key Files

| File | Purpose |
|------|---------|
| `register.go` | Extension registration |
| `prim_transcendental.go` | exp, log, sin, cos, tan, etc. |
| `prim_rounding.go` | floor, ceiling, truncate, round |
| `prim_division.go` | floor/, truncate/, quotients, remainders |
| `prim_complex.go` | make-rectangular, make-polar, etc. |
| `prim_predicates.go` | finite?, infinite?, nan? |

## Primitives (Runtime only)

### Transcendental Functions

| Primitive | Args | Purpose |
|-----------|------|---------|
| `exp` | 1 | Exponential (e^x) |
| `log` | 1-2 | Natural logarithm (or log base b) |
| `sin`, `cos`, `tan` | 1 | Trigonometric functions |
| `asin`, `acos`, `atan` | 1-2 | Inverse trig (atan accepts y, x) |
| `sqrt` | 1 | Square root (complex for negative) |
| `expt` | 2 | Exponentiation (x^y) |
| `square` | 1 | x^2 |

### Rounding

| Primitive | Args | Purpose |
|-----------|------|---------|
| `floor` | 1 | Round toward -∞ |
| `ceiling` | 1 | Round toward +∞ |
| `truncate` | 1 | Round toward 0 |
| `round` | 1 | Round to nearest even |

### Integer Division

| Primitive | Args | Purpose |
|-----------|------|---------|
| `floor/` | 2 | Returns quotient and remainder (floor) |
| `floor-quotient` | 2 | Floor division quotient |
| `floor-remainder` | 2 | Floor division remainder |
| `truncate/` | 2 | Returns quotient and remainder (truncate) |
| `truncate-quotient` | 2 | Truncate division quotient |
| `truncate-remainder` | 2 | Truncate division remainder |

### Complex Numbers

| Primitive | Args | Purpose |
|-----------|------|---------|
| `make-rectangular` | 2 | Create complex from real, imag |
| `make-polar` | 2 | Create complex from magnitude, angle |
| `real-part` | 1 | Extract real component |
| `imag-part` | 1 | Extract imaginary component |
| `magnitude` | 1 | Complex magnitude |
| `angle` | 1 | Complex angle |

### Numeric Predicates

| Primitive | Args | Purpose |
|-----------|------|---------|
| `finite?` | 1 | Check if number is finite |
| `infinite?` | 1 | Check if number is infinite |
| `nan?` | 1 | Check if number is NaN |

### Other

| Primitive | Args | Purpose |
|-----------|------|---------|
| `numerator` | 1 | Rational numerator |
| `denominator` | 1 | Rational denominator |
| `rationalize` | 2 | Find rational within tolerance |
| `exact-integer-sqrt` | 1 | Exact integer square root |
| `number->string` | 1-2 | Convert number to string |
| `string->number` | 1-2 | Parse string as number |

## Gotchas

- **Complex results**: sqrt of negative returns complex, not NaN
- **Branch cuts**: Uses Go's math/cmplx conventions (implementation-defined per R7RS)
- **exact-integer-sqrt**: Requires exact integer input, errors on inexact
