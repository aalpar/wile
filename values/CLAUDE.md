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

Interfaces: `Hashable`, `Tuple`, `Indexable`

## Error Handling

**`errors.Is` and `errors.As` are mandatory.** Never compare errors with `==` or `!=`. Error wrapping means `err == ErrFoo` silently fails when the error is wrapped. Use `errors.Is(err, ErrFoo)` for sentinel errors and `errors.As(err, &target)` for typed errors. This applies to all error comparisons including `io.EOF` and any other sentinel values.

**Two-layer error convention: sentinel + wrap.** Every error returned from production code uses two layers:

1. **Static sentinel** (`values.NewStaticError`) defined centrally in `values/foreign_error.go`. The sentinel is a stable identity for programmatic matching via `errors.Is`. Use existing sentinels before adding new ones.

2. **Contextual wrap** (`values.WrapForeignErrorf`) at each return site. The wrap message describes *where* the error occurred and *what operation* failed, so a human reading the error can locate the failure without a debugger.

Never return a bare sentinel — always wrap it with site-specific context. Never use `errors.New` or `fmt.Errorf` in production code; use a sentinel + wrap instead. `fmt.Errorf` is reserved exclusively for internal use within the error type constructors (`StaticError`, `ForeignError`, `NativeError`). In production code, every error must wrap a sentinel so callers can match with `errors.Is`/`errors.As`. Using `fmt.Errorf` creates opaque errors that defeat programmatic error handling.

```go
// WRONG: bare sentinel, no context
return nil, values.ErrNotANumber

// WRONG: errors.New, no programmatic matching
return nil, errors.New("not a number")

// WRONG: fmt.Errorf, opaque — callers cannot use errors.Is/errors.As
return nil, fmt.Errorf("not a number: %s", val)

// CORRECT: sentinel for errors.Is + wrap for human context
return nil, values.WrapForeignErrorf(values.ErrNotANumber, "makeExact: value is not numeric")
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
| `*values.ForeignError` | Go primitive failures with stack trace | `values/foreign_error.go` |
| `*values.StaticError` | Sentinel errors for `errors.Is` matching | `values/foreign_error.go` |
