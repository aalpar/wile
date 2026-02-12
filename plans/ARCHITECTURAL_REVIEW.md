# Architectural Code Review

**Date:** 2026-02-12
**Scope:** Full codebase review across values/, machine/, tokenizer/parser/syntax, registry/core/, extensions, and public API.
**Status:** Findings documented. Batch 1 not yet started.

---

## HIGH — Correctness Bugs

### H1. `Pair.Append` mutates the receiver instead of copying

**File:** `values/pair.go:104-127`
**Status:** Open

`Append` walks to the last pair and sets `q[1] = vs`, destructively modifying the original list. R7RS section 6.4 says all arguments except the last must be newly allocated. `(define x '(1 2)) (append x '(3))` silently mutates `x`.

### H2. `Float.ToExact()` nil-pointer panic on Inf/NaN

**File:** `values/numeric_tower.go:41`
**Status:** Open

`big.Rat.SetFloat64()` returns nil for infinity and NaN. The next line calls `r.IsInt()` on nil. R7RS says `(exact +inf.0)` should raise an error, not crash Go.

### H3. `Integer.Add`/`Subtract` zero short-circuit breaks exactness contagion

**File:** `values/integer.go:170-175` (and all 7 numeric types)
**Status:** Open

`(+ 0 0.0)` returns exact `0` instead of inexact `0.0`. The zero-optimization returns `p` when `o.IsZero()` without checking exactness. R7RS section 6.2.2 requires inexact contagion: an exact+inexact operation must return inexact.

### H4. `BigComplex.ToExact()` truncates BigFloat parts to BigInteger

**File:** `values/big_complex.go:571-586`
**Status:** Open

`toExactPart` converts BigFloat to integer by truncation instead of Rational. `(exact 1.5+0i)` produces `1` instead of `3/2`. The non-complex `BigFloat.ToExact()` correctly produces Rational.

### H5. `string->utf8` uses byte indices instead of character indices

**File:** `registry/core/prim_byte_vectors.go:247-258`
**Status:** Open

`len(s)` is byte length, `s[start:end]` is byte slicing. R7RS section 6.9 specifies character indices. Multi-byte UTF-8 strings produce wrong results.

### H6. `real?` missing `*values.Complex` case

**File:** `registry/core/prim_predicates.go:158-170`
**Status:** Open

`Complex` does not implement `RealNumber`, so `(real? 3.0+0.0i)` returns `#f`. BigComplex IS handled. R7RS section 6.2.6 says it should return `#t`.

### H7. `generate-temporaries` panics on non-list argument

**File:** `registry/core/prim_syntax.go:130`
**Status:** Open

Unchecked type assertion `arg.(values.Tuple).Length()`. Non-list input causes Go panic instead of Scheme error.

### H8. `SourceIndexes.NewLine()`/`Tab()` double-count byte position

**File:** `internal/syntax/source_indexes.go:66-71`
**Status:** Open

`readNextRune` calls `Inc(n)` to advance the index, then `NewLine()`/`Tab()` increment it again. Every newline and tab adds an extra phantom byte to the position tracker. All source locations after the first newline in any file are wrong. This affects every error message in the system.

### H9. String hex escape `\xHHHH;` missing surrogate and max validation

**File:** `internal/tokenizer/tokenizer.go:595`
**Status:** Open

Character literal hex `#\xHHHH` validates against U+10FFFF and surrogate range. String hex escape `"\xHHHH;"` does not. `"\xD800;"` produces an invalid Unicode code point.

---

## HIGH — Thread Safety

### T1. Global mutable I/O state without synchronization

**File:** `internal/extensions/io/state.go:29-54`
**Status:** Open

`Tokenizers` and `Parsers` maps are package-level globals with no locks. `closePort` does `delete()` without synchronization. Concurrent I/O from SRFI-18 threads will crash with Go's concurrent map panic.

### T2. `GlobalEnvironmentFrame` bindings unprotected

**File:** `environment/global_environment_frame.go`
**Status:** Open

`keys` map and `bindings` slice have no synchronization. `(define x ...)` from one thread + variable lookup from another = data race. `append` on `bindings` can cause corruption.

### T3. `with-input-from-file`/`with-output-to-file` race on global port state

**File:** `internal/extensions/files/prim_files.go:190-246`
**Status:** Open

Save/restore of global port parameters is not thread-safe. Should use `parameterize` semantics.

### T4. `PrimMakeThread` captures parent `MachineContext` across goroutine boundary

**File:** `internal/extensions/threads/prim_threads.go:104-143`
**Status:** Open

`mc.NewSubContext()` is called from the child goroutine on the parent's MachineContext. MC is not goroutine-safe.

### T5. `nextScopeID` counter is not atomic

**File:** `internal/syntax/syntax_value.go:44-51`
**Status:** Open

Plain `uint64` incremented non-atomically. Data race under concurrent macro expansion.

---

## MEDIUM — Correctness Issues (wrong in edge cases)

### M1. `set!` on locals does not use scope-aware lookup

**File:** `machine/compile_validated.go:597-605`
**Status:** Open

Validation uses `GetBindingWithScopes` but the actual local index lookup uses `GetLocalIndex` (not scope-aware). In hygienic macro-generated code with shadowed locals, this could store to the wrong binding slot.

### M2. Winding stack aliasing in `RestoreWithWindingFrom` and `UnwindTo`

**File:** `machine/machine_context.go:703-742, 632-653`
**Status:** Open

`p.windingStack = sourceStack[:commonDepth]` shares backing array. Subsequent `append` in `RewindTo` can corrupt the original stack. Same issue in `UnwindTo` where sub-context gets a shared slice. Fix: use cap-limited slices `[:n:n]`.

### M3. `NewSubContext` does not inherit exception handlers

**File:** `machine/machine_context.go:454-467`
**Status:** Open

Sub-contexts used for `map`, `for-each`, `dynamic-wind` thunks don't see enclosing `with-exception-handler`. R7RS says exception handlers have dynamic extent.

### M4. `SyntaxVector.AddScope` does not propagate scopes to elements

**File:** `internal/syntax/syntax_vector.go:36`
**Status:** Open

Returns `p` unchanged. Macro-introduced code containing vectors with identifiers (e.g., `#(a b c)`) won't get hygiene scopes applied to the symbols inside the vector.

### M5. `BigInteger.Compare` with Float loses precision via float64 conversion

**File:** `values/big_integer.go:377-384`
**Status:** Open

Large BigIntegers converted to float64 before comparison. Two distinct BigIntegers can compare equal to the same Float. Should convert Float to BigFloat instead.

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
