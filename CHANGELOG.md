# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).

## [Unreleased]

## [1.2.0] - 2026-02-11

### Added

- Add `NewRational`, `NewComplex`, `NewVector` value constructors to public embedding API
- Add Scheme-level test infrastructure (`scheme-test` executable built from `tests/run-tests.scm`)

### Changed

- Rename `CreateLocalBinding` to `EnsureLocalBinding` on `EnvironmentFrame` and `LocalEnvironmentFrame` — the method has get-or-create semantics (returns existing binding if key exists), and the new name reflects actual behavior (breaking API change for embedders)

### Fixed

- Fix `(list? syntax-obj)` returning `#t` instead of `#f` for syntax objects (R7RS: syntax objects are not lists)
- Fix `EqualTo` comparison for empty syntax lists (previously compared unequal to themselves)
- Fix `ArrayList.ForEach` violating `Tuple` interface contract (previously mutated during iteration)
- Fix 32 missing R7RS library exports across `scheme/char`, `scheme/complex`, `scheme/inexact`, and `scheme/lazy`
- Fix bytevector parser and `NewByteVectorFromIntegers` accepting out-of-range integers (now rejects values outside 0-255)
- Fix flaky `TestMutexAbandoned` test by replacing timing-based synchronization with polling loop

## [1.1.0] - 2026-02-08

### Added

- Enforce cross-thread continuation rejection: continuations captured in one thread now raise `ErrCrossThreadContinuation` when invoked from a different thread, preventing VM state corruption
- Run `dynamic-wind` after-thunks on thread termination (both normal exit and `thread-terminate!`)
- Mark owned mutexes as abandoned when a thread terminates, unblocking waiters per SRFI-18 semantics
- Add thread identity to `MachineContext` so `current-thread` returns the actual thread object inside spawned threads (previously always returned `'primordial`)
- Add `CompilationError` and `RuntimeError` structured error types with `Unwrap()` support for programmatic error handling via `errors.As`
- Add `RuntimeError.Condition` field carrying the Scheme raised value when errors originate from `raise`/`raise-continuable`
- Add 16 value-inspection helpers for embedding: `IsList`, `IsPair`, `IsVector`, `IsSymbol`, `Car`, `Cdr`, `ToSlice`, `ToGoInt`, `ToGoFloat`, `ToGoString`, `ToGoBool`, `ToGoBytes`, `ListLength`, `VectorRef`, `VectorLength`, `VectorToSlice`
- Add context cancellation support: engine operations respect `context.Context` through VM execution and macro expansion
- Add VM performance counters to `MachineContext` for runtime introspection
- Add `ComplexNumber` sub-interface with `RealPart()`, `ImagPart()`, `IsReal()` for interface-based complex number dispatch
- Add `RealNumber` sub-interface with `IsPositive()`, `IsNegative()`, `Sign()` for interface-based real number dispatch
- Add `Abs()`, `ToExact()`, `ToInexact()` methods to the `Number` interface
- Add `IsInteger()`, `IsRational()`, `IsFinite()`, `IsNaN()` predicate methods to the `Number` interface
- Add `EvalWithSource`, `EvalMultipleWithSource`, and `CompileWithSource` methods for source-tracked evaluation — source locations appear in `RuntimeError.Source` and `RuntimeError.StackTrace`
- Add `RegisterFunc` for registering Go functions with natural signatures — supports `int64`, `int`, `float64`, `string`, `bool`, `[]byte`, `Value`, `context.Context`, variadic parameters, and `(T, error)` returns
- Extend `RegisterFunc` with composite type support: `[]T` ↔ Scheme lists, `map[K]V` ↔ hashtables, structs ↔ alists, and `func(...)` callback parameters accepting Scheme procedures (including `make-parameter` objects)
- Add `ErrTypeConversion` sentinel for FFI runtime type mismatch errors
- Wire `ErrExceptionEscape` to carry source location and stack trace from per-operation source tracking

### Changed

- Optimize scope matching hot paths: add size guard early return in `ScopesMatch`, cache `Scopes()` calls in `GetBindingWithScopes`, and add perfect-match early termination in `GetLocalIndexWithScopes`

- Centralize ~190 type assertion sites into `RequireArg[T]` and `RequireType[T]` generic helpers, reducing boilerplate across 22 primitive files
- `Engine.Call` now dispatches case-lambda, parameter objects, and composable continuations in addition to plain closures
- `EmptyList` is now a dedicated singleton type (not `*Pair`), enforcing `(pair? '()) → #f` at the type level
- `String` implements the `Indexable` interface with `Length()`, `Get()`, `Set()` methods

### Removed

- Remove unused `Tower*` dispatch functions from the numeric tower
- Remove unused `*Same` methods from numeric types

### Fixed

- Fix void-returning primitives (`display`, `newline`, `vector-set!`, etc.) silently dropping argument slots when used as function arguments
- Fix `CurrentSource()` not walking the continuation chain when the current template has no source info
- Fix `CaptureStackTrace` using wrong PC for continuation frames (return addresses pointed past the call site)
- Fix `(pair? '())` returning `#t` instead of `#f` (R7RS §6.4: the empty list is not a pair)
- Fix `Engine.Call` and `runCompiled` leaking internal `ErrMachineHalt` sentinel to callers
- Fix parameter converter errors returned without context (now wrapped with "parameter: converter error")
- Use `errors.Is` for all sentinel error comparisons (`io.EOF`, `ErrMachineHalt`) to handle wrapped errors correctly
- Fix empty list `()` in expression position causing "empty application in call form" compiler error (R7RS §4.1.2)
- Fix `(exact-integer? 1+0i)` returning `#f` instead of `#t` for exact complex with zero imaginary part (R7RS §6.2.6)
- Fix `(rational? z)` returning `#f` for real `BigComplex` values
- Fix `(integer? z)` for large inexact floats outside int64 range
- Fix `RegisterFunc` silently producing empty slices/structs when a non-list value is passed where a proper list is expected
- Fix `RegisterFunc` panicking on named scalar types (e.g., `type MyInt int64`) due to `reflect.Call` type mismatch

## [1.0.3] - 2026-02-05

### Fixed

- Use SPDX canonical Apache-2.0 license text for pkg.go.dev license detection

## [1.0.2] - 2026-02-05

### Fixed

- Fix asymmetric precision loss in mixed BigFloat/Complex arithmetic (`BigFloat + Complex` now returns `BigComplex` to preserve arbitrary precision, matching `Complex + BigFloat` behavior)

## [1.0.1] - 2026-02-04

### Added

- Automated release builds with prebuilt binaries for darwin/linux on arm64/amd64

## [1.0.0] - 2026-02-04

### Added

- R7RS-small Scheme interpreter with bytecode compiler and stack-based virtual machine
- Hygienic macros via `syntax-rules` using the sets-of-scopes model (Flatt 2016)
- First-class syntax objects preserving source location and scope information
- First-class continuations with `call/cc` and `dynamic-wind`
- Delimited continuations with prompts and composable capture
- Proper tail-call optimization
- R7RS standard libraries: `scheme/base`, `scheme/char`, `scheme/complex`, `scheme/cxr`, `scheme/eval`, `scheme/file`, `scheme/inexact`, `scheme/lazy`, `scheme/load`, `scheme/read`, `scheme/write`, `scheme/repl`, `scheme/process-context`, `scheme/time`, `scheme/case-lambda`, `scheme/r5rs`
- Full numeric tower: integers, rationals, floats, complex numbers, with exact/inexact distinction
- Arbitrary precision integers (`BigInteger`) with automatic overflow promotion
- R7RS §7.1.1 inexact digit placeholders (`1.2###`) in numeric literals
- Non-decimal base fractions (`#x10/2`, `#o11/2`, `#b101/10`)
- Hashtable primitives with `Hashable` key interface
- Box primitives (`box`, `box?`, `unbox`, `set-box!`)
- Go embedding API via the `wile` package: `Engine`, `Eval`, `Compile`, `Run`, `Define`, `Get`, `Call`, `RegisterPrimitive`
- Value constructors for Go interop: `NewInteger`, `NewFloat`, `NewString`, `NewSymbol`, `NewBoolean`, `NewList`
- Library system with `define-library`, `import`, `export` and configurable search paths
- Interactive REPL with readline support and debug commands
- File execution mode with positional argument and `--file` flag
- SIGQUIT handler for goroutine stack dumps
- Multi-platform builds: `dist/{os}/{arch}/scheme` layout with targets for darwin/linux on arm64/amd64
- Docker build support with `TARGETOS`/`TARGETARCH` platform awareness
- CI builds all four OS/architecture combinations
- R7RS conformance test suite running in CI

[1.2.0]: https://github.com/aalpar/wile/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/aalpar/wile/compare/v1.0.4...v1.1.0
[1.0.4]: https://github.com/aalpar/wile/compare/v1.0.3...v1.0.4
[1.0.3]: https://github.com/aalpar/wile/compare/v1.0.2...v1.0.3
[1.0.2]: https://github.com/aalpar/wile/compare/v1.0.1...v1.0.2
[1.0.1]: https://github.com/aalpar/wile/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/aalpar/wile/releases/tag/v1.0.0

