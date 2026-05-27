# values/ — Value Types and Error Handling

## Value Types

`Value` interface: `SchemeString()`, `IsVoid()`, `EqualTo(Value)`. No Kind enum — use type assertions.

- **Numeric** (`Number`): Integer, BigInteger, Float, BigFloat, Rational, Complex, BigComplex
- **Basic**: Boolean, Symbol, String, Character, Byte
- **Collections**: Pair, Vector, ByteVector, Hashtable, EmptyList
- **I/O** (`Port`/`InputPort`/`OutputPort`): BinaryInputPort, CharacterInputPort, TextualWriter
- **Control**: MachineClosure, CaseLambdaClosure, Parameter, ComposableContinuation
- **Advanced**: SyntaxValue, CompileTimeValue, Record, Box, Promise, Channel, Thread, Mutex
- **Errors**: ForeignError, NativeError, StaticError

Interfaces: `Hashable`, `Tuple`, `Indexable`, `Number`, `ComplexNumber`

## Numeric Conversion Helpers (`conversion.go`)

Public helpers that surface Go's `big.Accuracy` three-valued enum at
the Scheme/Go boundary for `float64` / `complex128` extraction:

| Function | Returns | Purpose |
|----------|---------|---------|
| `ToFloat64WithAccuracy(n Number)` | `(float64, big.Accuracy, isReal bool, error)` | Primary helper. Accuracy field is the signal. |
| `ToFloat64Lossless(n Number)` | `(float64, error)` | Strict wrapper. Returns `werr.ErrLossyConversion` (wrapped, names direction) on any loss. |
| `ToComplex128WithAccuracy(n Number)` | `(Complex128Result, error)` | Per-component accuracy via named-field struct. |
| `ToComplex128Lossless(n Number)` | `(complex128, error)` | Strict wrapper. Returns `werr.ErrLossyConversion` (wrapped) if either component non-Exact. |
| `BigAccuracyToSymbol(acc big.Accuracy) *Symbol` | `*Symbol` | Projects `big.Below` / `big.Exact` / `big.Above` to the Scheme symbols `'below` / `'exact` / `'above`. |

The `WithAccuracy` forms return the raw value plus accuracy slots
without erroring on loss; the `Lossless` forms reject loss with
`werr.ErrLossyConversion`. The FFI converter and the math
extension's `inexact-*` primitives both consume these helpers. See
`docs/numeric/tower.md` §"Conversion to Fixed-Precision Go Types".

## In-Place Arithmetic on BigInteger (`numeric_scratch.go`)

Unexported helpers that mutate a destination's `*big.Int` storage in
place, for tight-loop callers that own the destination:

| Helper | Operation |
|--------|-----------|
| `addBigIntInPlace(dest, p, v *BigInteger)` | `dest.value = p.value + v.value` |
| `subBigIntInPlace(dest, p, v *BigInteger)` | `dest.value = p.value - v.value` |
| `mulBigIntInPlace(dest, p, v *BigInteger)` | `dest.value = p.value * v.value` |
| `negateBigIntInPlace(dest, p *BigInteger)` | `dest.value = -p.value` |

All return `dest` for chaining. `dest` may alias `p`, `v`, or both —
the aliasing behaviour is empirically pinned by
`numeric_scratch_test.go` since the `math/big` doc does not promise it
in writing.

**Why unexported.** The public `(*BigInteger).Add` etc. remain immutable
per R7RS Number semantics; in-place mutation would leak observable
state into Scheme. The helpers exist for Go-side consumers (currently
`algebra/graph.CountPathsInDAG`, which calls `(*big.Int).Add` directly
on its own slot array; future weighted-bigint and approximate-counting
DAG kernels will consume the `*BigInteger`-wrapping variants here).

**Allocation profile.** A freshly-constructed `*big.Int` has zero
`[]Word` capacity; the *first* in-place op allocates a backing slice.
Subsequent ops within capacity reuse it. To skip the growth phase when
the working width is known, pre-size with
`(*big.Int).SetBits(make([]big.Word, 0, N))`. Microbenches in
`numeric_scratch_bench_test.go` measure the steady-state cost against
the allocating `(*BigInteger).Add` baseline:

| Bench | ns/op | B/op | allocs/op | vs allocating |
|-------|-------|------|-----------|---------------|
| `BigIntegerAdd` (allocating) | 35 | 88 | 3 | baseline |
| `BigIntAddInPlace` | 5.2 | 0 | 0 | **6.7×** |
| `BigIntegerMultiply` (allocating) | 37 | 88 | 3 | baseline |
| `BigIntMulInPlace` | 6.6 | 0 | 0 | **5.6×** |

## Error Handling

**`errors.Is` and `errors.As` are mandatory.** Never compare errors with `==` or `!=`. Error wrapping means `err == ErrFoo` silently fails when the error is wrapped. Use `errors.Is(err, ErrFoo)` for sentinel errors and `errors.As(err, &target)` for typed errors. This applies to all error comparisons including `io.EOF` and any other sentinel values.

**Two-layer error convention: sentinel + wrap.** Every error returned from production code uses two layers:

1. **Static sentinel** (`werr.NewStaticError`) defined centrally in `werr/werr.go`. The sentinel is a stable identity for programmatic matching via `errors.Is`. Use existing sentinels before adding new ones.

2. **Contextual wrap** (`werr.WrapForeignErrorf`) at each return site. The wrap message describes *where* the error occurred and *what operation* failed, so a human reading the error can locate the failure without a debugger.

Never return a bare sentinel — always wrap it with site-specific context. Never use `errors.New` or `fmt.Errorf` in production code; use a sentinel + wrap instead. `fmt.Errorf` is reserved exclusively for internal use within the error type constructors (`StaticError`, `ForeignError`, `NativeError`). In production code, every error must wrap a sentinel so callers can match with `errors.Is`/`errors.As`. Using `fmt.Errorf` creates opaque errors that defeat programmatic error handling.

```go
// WRONG: bare sentinel, no context
return nil, werr.ErrNotANumber

// WRONG: errors.New, no programmatic matching
return nil, errors.New("not a number")

// WRONG: fmt.Errorf, opaque — callers cannot use errors.Is/errors.As
return nil, fmt.Errorf("not a number: %s", val)

// CORRECT: sentinel for errors.Is + wrap for human context
return nil, werr.WrapForeignErrorf(werr.ErrNotANumber, "makeExact: value is not numeric")
```

**Wrap errors with explanatory context.** Every error that crosses a subsystem boundary must include context about what operation failed. Examples:
- `"parameter: converter error"` — tells you a parameter's converter function failed
- `"bootstrap: expansion error"` — tells you macro expansion failed during bootstrap
- `"parse error"` — tells you the parser couldn't read the input

Never wrap with empty messages (`WrapForeignErrorf(err, "")` produces `": underlying error"`). If the error already has sufficient context, return it as-is.

**Use the project's error types consistently:**

| Type | When to use | Where defined |
|------|------------|---------------|
| `*wile.CompilationError` | Parse, expand, compile failures | `error.go` |
| `*wile.RuntimeError` | Execution failures, Scheme exceptions | `error.go` |
| `*machine.SchemeError` | Internal VM errors with source location | `machine/scheme_error.go` |
| `*werr.ForeignError` | Go primitive failures with stack trace | `werr/werr.go` |
| `*werr.StaticError` | Sentinel errors for `errors.Is` matching | `werr/werr.go` |
