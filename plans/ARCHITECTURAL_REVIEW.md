# Architectural Code Review

**Date:** 2026-02-12
**Scope:** Full codebase review across values/, machine/, tokenizer/parser/syntax, registry/core/, extensions, and public API.
**Status:** Findings documented. Batch 1 not yet started.

---

## HIGH — Correctness Bugs

### H4. `BigComplex.ToExact()` truncates BigFloat parts to BigInteger

**File:** `values/big_complex.go:571-586`
**Status:** ✅ Fixed

`toExactPart` converts BigFloat to integer by truncation instead of Rational. `(exact 1.5+0i)` produces `1` instead of `3/2`. The non-complex `BigFloat.ToExact()` correctly produces Rational.

**Fix:** Replaced `v.value.Int(nil)` truncation with proper conversion via `big.Rat`. Now converts BigFloat → big.Rat → (Rational | BigInteger) depending on whether the result is an integer. Added comprehensive test `TestBigComplex_ToExactFractionalParts` covering all fractional part combinations.

### H6. `real?` missing `*values.Complex` case

**File:** `registry/core/prim_predicates.go:158-170`
**Status:** ✅ Fixed

`Complex` does not implement `RealNumber`, so `(real? 3.0+0.0i)` returns `#f`. BigComplex IS handled. R7RS section 6.2.6 says it should return `#t`.

**Fix:** Added `case values.ComplexNumber:` that calls `v.IsReal()` to check if imaginary part is zero. This handles both `*values.Complex` and `*values.BigComplex` uniformly via the `ComplexNumber` interface. Added comprehensive regression test `TestRealQ_ComplexRegression` covering both complex types with zero and non-zero imaginary parts.

### H7. `generate-temporaries` panics on non-list argument

**File:** `registry/core/prim_syntax.go:130`
**Status:** ✅ Fixed

Unchecked type assertion `arg.(values.Tuple).Length()`. Non-list input causes Go panic instead of Scheme error.

**Fix:** Added type check before assertion, returns `ErrNotAList` with context.

### H8. `SourceIndexes.NewLine()`/`Tab()` double-count byte position

**File:** `internal/syntax/source_indexes.go:66-71`
**Status:** ✅ Fixed

`readNextRune` calls `Inc(n)` to advance the index, then `NewLine()`/`Tab()` increment it again. Every newline and tab adds an extra phantom byte to the position tracker. All source locations after the first newline in any file are wrong. This affects every error message in the system.

**Fix:** Removed `p.index++` from both `NewLine()` and `Tab()` methods. These methods now only update column and line tracking, since the index has already been advanced by `Inc(n)` in `readNextRune()`. Updated tests to reflect correct behavior. Added comprehensive position tracking tests in tokenizer package to prevent regression.

### H9. String hex escape `\xHHHH;` missing surrogate and max validation

**File:** `internal/tokenizer/tokenizer.go:595`
**Status:** ✅ Fixed

Character literal hex `#\xHHHH` validates against U+10FFFF and surrogate range. String hex escape `"\xHHHH;"` does not. `"\xD800;"` produces an invalid Unicode code point.

**Fix:** Added Unicode code point validation to `readHexEscapeToken()` matching the validation in character hex escapes. Now validates that code points are ≤ U+10FFFF and not in the surrogate range (U+D800-U+DFFF). Added comprehensive test `TestStringHexEscape_H9_Validation` with 38 test cases covering valid code points (ASCII, multi-byte, boundaries, maximum), surrogate errors, and maximum exceeded errors.

---

## HIGH — Thread Safety

### T1. Global mutable I/O state without synchronization

**File:** `internal/extensions/io/state.go:29-54`
**Status:** ✅ Fixed

`Tokenizers` and `Parsers` maps are package-level globals with no locks. `closePort` does `delete()` without synchronization. Concurrent I/O from SRFI-18 threads will crash with Go's concurrent map panic.

**Fix:** Added `sync.RWMutex cacheMu` to protect all map access. Map reads/writes/deletes in `PrimRead`, `PrimReadToken`, `PrimReadSyntax`, and `closePort` are now synchronized. Used full Lock (not RLock) for check-then-write patterns to prevent TOCTOU races. Also added `sync.Mutex stateMu` to protect `InitState`/`ResetState` from concurrent initialization races.

### T2. `GlobalEnvironmentFrame` bindings unprotected

**File:** `environment/global_environment_frame.go`
**Status:** ✅ Fixed

`keys` map and `bindings` slice have no synchronization. `(define x ...)` from one thread + variable lookup from another = data race. `append` on `bindings` can cause corruption.

**Fix:** Added `sync.RWMutex mu` to `GlobalEnvironmentFrame` to protect all map and slice access. All methods (`CreateGlobalBinding`, `GetGlobalIndex`, `GetOwnGlobalBinding`, `SetOwnGlobalValue`, `Copy`, `EqualTo`, `Bindings`, `SetBindings`, `Keys`) now use appropriate locking (RLock for reads, Lock for writes). Updated `EnvironmentFrame` methods (`SetOwnGlobalValue`, `MaybeCreateOwnGlobalBinding`, `resolveGlobal`, `SetGlobalBindingByIndex`) to either delegate to protected `GlobalEnvironmentFrame` methods or acquire locks when accessing fields directly. `EqualTo` uses consistent lock ordering (lower pointer address first) to prevent deadlock. Created comprehensive concurrency test `TestConcurrentGlobalAccess_T2` with 7 test scenarios covering concurrent creates, lookups, updates, mixed operations, copies, equality checks, and parent-chain resolution.

### T3. `with-input-from-file`/`with-output-to-file` race on global port state

**File:** `internal/extensions/files/prim_files.go:190-246`
**Status:** ✅ Fixed

Save/restore of global port parameters is not thread-safe. Should use `parameterize` semantics.

**Fix:** Converted `with-input-from-file` and `with-output-to-file` from Go primitives to Scheme macros in `with_file_macros.scm`. They now expand to:
```scheme
(call-with-input-file filename
  (lambda (port)
    (parameterize ((current-input-port port))
      (thunk))))
```
This uses `parameterize`, which expands to `dynamic-wind`, providing:
- **Continuation safety**: Parameter changes are tracked on the winding stack, integrating properly with `call/cc`
- **Dynamic extent semantics**: Before/after thunks ensure parameters are restored even when continuations escape
- **Code reuse**: Leverages existing `call-with-input-file` and parameter infrastructure

### T4. `PrimMakeThread` captures parent `MachineContext` across goroutine boundary

**File:** `internal/extensions/threads/prim_threads.go:104-143`
**Status:** ✅ Fixed

`mc.NewSubContext()` is called from the child goroutine on the parent's MachineContext. MC is not goroutine-safe.

**Fix:** Added `SubContextParams` struct and two new methods: `CaptureSubContextParams()` captures parent state in the parent goroutine, and `NewThreadSubContext(params, thread)` constructs the sub-context in the child goroutine using only the captured state. This eliminates all cross-goroutine field access. The captured state includes context, top-level environment, parent MC reference, and escape continuation. Thread identity is set via `SetThread(thread)` using the new thread object, not inherited from parent.

### T5. `nextScopeID` counter is not atomic

**File:** `internal/syntax/syntax_value.go:44-51`
**Status:** ✅ Fixed

Plain `uint64` incremented non-atomically. Data race under concurrent macro expansion.

**Fix:** Replaced non-atomic `nextScopeID++` with `atomic.AddUint64(&nextScopeID, 1)` in both `NewScope()` and `NewRebindingScope()`. The atomic increment returns the new value which is directly used for the scope ID. This ensures thread-safe scope creation during concurrent macro expansion.

---

## MEDIUM — Correctness Issues (wrong in edge cases)

### M1. `set!` on locals does not use scope-aware lookup

**File:** `machine/compile_validated.go:597-605`
**Status:** ✅ Fixed

Validation uses `GetBindingWithScopes` but the actual local index lookup uses `GetLocalIndex` (not scope-aware). In hygienic macro-generated code with shadowed locals, this could store to the wrong binding slot.

**Fix:** Changed `CompileValidatedSetBang` to follow the same pattern as `CompileSymbol` (compile_time_continuation.go:115-170): branch on `len(symbolScopes) > 0`, using `GetLocalIndexWithScopes` for scoped symbols and `GetLocalIndex` for unscoped symbols. This ensures hygiene correctness for macro-generated code with shadowed locals.

### M2. Winding stack aliasing in `RestoreWithWindingFrom` and `UnwindTo`

**File:** `machine/machine_context.go:703-742, 632-653`
**Status:** ✅ Fixed

`p.windingStack = sourceStack[:commonDepth]` shares backing array. Subsequent `append` in `RewindTo` can corrupt the original stack. Same issue in `UnwindTo` where sub-context gets a shared slice.

**Fix:** Changed all winding stack slice operations to use three-index slice syntax `[:n:n]` to cap capacity and prevent backing array sharing. This ensures that any subsequent `append()` allocates a new array instead of writing to shared memory. Fixed four locations:
- `UnwindTo()` lines 685, 698
- `RestoreWithWindingFrom()` lines 760, 773

Added regression test `TestWindingStackAliasingBug_M2` that captures a continuation with nested dynamic-wind frames and verifies no corruption during unwind/rewind.

### M3. `NewSubContext` does not inherit exception handlers

**File:** `machine/machine_context.go:454-467, 469-501`
**Status:** ✅ Fixed

Sub-contexts used for `apply`, `call-with-values`, `dynamic-wind` thunks don't see enclosing `with-exception-handler`. R7RS says exception handlers have dynamic extent.

**Fix:** Added `exceptionHandler: p.exceptionHandler` to `NewSubContext()` to automatically inherit the parent's exception handler chain. Updated `SubContextParams` and `NewSubContextFromParams` to include exception handler for cross-goroutine sub-context creation (thread spawning). Removed 3 manual `SetExceptionHandler` calls from `prim_exceptions.go` (now redundant). Added unit tests for exception handler inheritance and Scheme integration tests for `apply`, `call-with-values`, and `dynamic-wind`.

### M4. `SyntaxVector.AddScope` does not propagate scopes to elements

**File:** `internal/syntax/syntax_vector.go:36`
**Status:** ✅ Fixed

Returns `p` unchanged. Macro-introduced code containing vectors with identifiers (e.g., `#(a b c)`) won't get hygiene scopes applied to the symbols inside the vector.

**Fix:** Implemented recursive scope propagation in `SyntaxVector.AddScope()` following the same pattern as `SyntaxPair.AddScope()`. Extended `mapSyntaxTree` helper to handle vectors, added vector case to `AddScopeToSyntax` and `operation_syntax_rules_transform.go` helper. Added 9 unit tests and 3 integration tests verifying basic, nested, and mixed-element scope propagation.

### M5. `BigInteger.Compare` with Float loses precision via float64 conversion

**File:** `values/big_integer.go:377-384`
**Status:** ✅ Fixed

Large BigIntegers converted to float64 before comparison. Two distinct BigIntegers can compare equal to the same Float. Should convert Float to BigFloat instead.

**Fix:** Changed `Compare()` and arithmetic methods (`Add`, `Subtract`, `Multiply`, `Divide`) to promote both operands to BigFloat instead of demoting BigInteger to float64. This preserves precision for integers beyond the float64 mantissa limit (2^53) while maintaining R7RS exactness contagion (exact + inexact → inexact). Updated numeric tower tests to expect BigFloat results instead of Float. Added comprehensive precision tests covering boundary cases (2^53±1), negative values, and arithmetic operations.

### M6. String interning allows mutation of shared strings

**File:** `values/string.go:42-46`
**Status:** Open

`NewString` interns short strings. `string-set!` on an interned string corrupts all references and the intern cache. No immutability flag exists. Related: L8, L9.

### M7. `ConditionVariable.Wait` goroutine leak on timeout

**File:** `values/condition_variable.go:117-145`
**Status:** Open

Timed wait spawns a goroutine with `p.cond.Wait()` that cannot be cancelled after timeout fires. The goroutine blocks until the next signal/broadcast.

### M8. Dead code in `parseComplex` sign-splitting

**File:** `internal/parser/parser.go:1478-1485`
**Status:** Open

The prefix-checking `if` block is dead code — `signPos = i; break` executes regardless of the condition. The heuristic for finding the real/imaginary separator has no effect.

### M9. `string-ci` ordering uses `strings.ToLower` instead of Unicode case folding

**File:** `internal/extensions/all/prim_strings.go:324-350`
**Status:** Open

`string-ci=?` correctly uses `EqualFold`, but `string-ci<?` etc. use `ToLower`. These differ for characters like eszett. R7RS requires `string-foldcase` semantics.

### M10. `read-bytevector` drops partial read at EOF

**File:** `internal/extensions/io/prim_read_write.go:720-727`
**Status:** Open

When `Read` returns `n > 0` AND `err == io.EOF` (valid per Go's io.Reader contract), the code treats it as an error instead of returning the successfully read bytes.

### M11. `read-string` / `read-bytevector` unbounded allocation from user input

**File:** `internal/extensions/io/prim_read_write.go:496-530`
**Status:** Open

`make([]rune, 0, k.Value)` with no upper bound. `(read-string 999999999999)` causes OOM.

---

## LOW — Minor Issues

| # | Location | Issue |
|---|----------|-------|
| L1 | `values/complex.go:333` | `Complex.IsRational` returns false unconditionally |
| L2 | `values/integer.go:59` | Cache comment says -256..255, actual range is -32768..32767 |
| L3 | `values/channel.go:253` | `ChannelSelect` busy-spins without `reflect.Select` |
| L4 | `values/utils.go:253` | `NewTemporaryVariableName` seeds PRNG from `time.Now()` per call |
| L5 | `machine/stack.go:37` | `Pull()` has no bounds check (opaque panic vs `ErrStackUnderflow`) |
| L6 | `machine/compile_validated.go:604` | `set!` emits `LoadVoid` for globals but not locals |
| L7 | `registry/core/prim_arithmetic.go:212` | `abs` accepts complex (R7RS: real only) |
| L8 | `registry/core/prim_strings.go:183` | `list->string` may return interned string |
| L9 | `registry/core/prim_strings.go:194` | `symbol->string` returns mutable string |
| L10 | `internal/extensions/io/prim_read_write.go:485` | `char-ready?`/`u8-ready?` always return `#t` |
| L11 | `internal/extensions/eval/prim_eval.go:35` | `eval` doesn't inherit dynamic context |
| L12 | `engine.go:137` | `EvalMultiple` returns nil (not Void) for empty input |
| L13 | `internal/extensions/gointerop/prim_gointerop.go:417` | `once-do!` swallows thunk errors silently |
| L14 | `internal/extensions/threads/prim_threads.go:291` | `thread-join!` uses `==` not `errors.Is()` |
| L15 | `internal/extensions/threads/prim_threads.go:214` | `thread-sleep!` ignores context cancellation |
| L16 | `internal/extensions/io/prim_read_write.go:271` | `write-char` uses raw local binding instead of `mc.Arg()` |
| L17 | `internal/extensions/math/prim_math.go:310` | `expt` missing BigInteger case in fallback |
| L18 | `internal/extensions/math/prim_math.go:424` | `rationalToInteger` loses precision via float64 |
| L19 | `internal/tokenizer/tokenizer.go:2280` | `isExtendedExponentMarkerForRadix` ignores radix |

---

## Recommended Fix Order

### Batch 1 — Straightforward correctness (no design decisions)

H2, H7, H8, H9, H5, H6, M1, M8, L5

These are unambiguous bugs with clear fixes. No API changes or design tradeoffs.

### Batch 2 — Correctness requiring design thought

H1, H3, H4, M2, M4, M5

These need some care: Pair.Append requires copying semantics, the exactness contagion fix touches all 7 numeric types, winding stack aliasing needs cap-limited slices.

### Batch 3 — Thread safety (systemic, address together)

T1, T2, T3, T4, T5

The thread safety issues are pervasive. Address as a cohesive effort: either document that SRFI-18 threads + shared state is unsupported, or add synchronization across affected subsystems.

### Batch 4 — String mutability (needs design decision)

M6 (plus L8, L9)

Needs an immutability flag on String or a change to the interning strategy.

### Batch 5 — Remaining medium and low items

M3, M7, M9, M10, M11, and remaining LOW items. Fix as encountered or as part of related work.
