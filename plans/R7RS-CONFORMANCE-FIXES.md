# R7RS Conformance Fixes

**Source:** `plans/R7RS-CONFORMANCE-REVIEW.md`, `plans/R7RS-PORT-IO-CONFORMANCE.md`
**Status:** Complete (all phases shipped)

---

## Completion Summary

All 8 phases shipped across 5 PRs plus 1 open PR for a remaining C3 sub-fix:

| PR | Merged | Phases | Key Findings |
|----|--------|--------|--------------|
| #364 | 2026-03-01 | 1, 2, 3, 4, 5, 8 (partial) | C2, C4, H2, H5, H6, L4, L5, L6, M6, E3 |
| #365 | 2026-03-01 | 4, 5, 8 (remainder) | H2, H3, H4, M1, M2, L1, L2, L3, E2, E3 |
| #366 | 2026-03-01 | 6 | M3 (guard re-raise), M5 (eval multi-value), M4 (not reproducible) |
| #367 | 2026-03-01 | 6, 7 | M3/M5 continued, M7 (vector patterns), E1 (crash prevention) |
| #368 | 2026-03-01 | — (excluded) | H1 (apply tail position — shipped as separate effort) |
| #369 | **Open** | 1 (sub-fix) | C3 BigFloat `.0` suffix |

**Originally excluded items — final status:**
- L7 (`char-ready?`/`u8-ready?` always `#t`) — remains a documented semantic difference
- E1 (datum-label circular literals) — crash prevention shipped in PR #367; shared acyclic datum labels (`#0=(a) (#0#)`) still incorrectly rejected (tracked in `TODO.md`)
- H1 (`apply` tail position) — completed in PR #368 as compile-time special form

**M4 (dynamic-wind double-fire):** Investigated with 5 test variations (simple/nested/re-entry × top-level/let). Not reproducible — all cases show correct identical behavior. Closed as not-a-bug.

**M8 (dotted pair patterns):** Investigated and confirmed working correctly. The conformance review's test case was misleading — `b` captures the rest list, and `(list a b)` places it in evaluation position. Not a bug.

---

## Phasing Strategy

Group fixes by subsystem to minimize context-switching. Each phase is independently
shippable as a single PR. Phases are ordered by impact and ease — quick wins first
to build confidence in the test infrastructure, then deeper fixes.

---

## Phase 1: Display & Write Round-Trip (C2, C3, L6) ✓

**Status:** C2 and L6 complete (PR #364). C3 open (PR #369).
**Theme:** `write` output must be readable by `read`. Three bugs break round-trip.
**Effort:** Low (lookup table + String method tweak + format fix)
**PR scope:** `values/`

### C2: Named character write

**Current code** (`values/character.go:82-84`):
```go
func (p *Character) SchemeString() string {
    return fmt.Sprintf(`#\%c`, p.Value)
}
```
This outputs the raw byte for control characters. `#\newline` becomes `#\` + literal 0x0A.

**Fix:** Add a `var charNames` lookup map and check it before the `Sprintf` fallback.

```go
var charNames = map[rune]string{
    '\a': "alarm",
    '\b': "backspace",
    0x7F: "delete",
    0x1B: "escape",
    '\n': "newline",
    0x00: "null",
    '\r': "return",
    ' ':  "space",
    '\t': "tab",
}

func (p *Character) SchemeString() string {
    if name, ok := charNames[p.Value]; ok {
        return `#\` + name
    }
    return fmt.Sprintf(`#\%c`, p.Value)
}
```

**R7RS §6.6:** These are the ONLY 9 named characters specified. All other printable
characters use `#\x` form; non-printable, non-named characters use `#\xHEX` form.

**Question for Sonnet:** R7RS §6.13.3 also says `write` should use `#\x<hex>` for
characters that are neither graphic nor named. Check if the current code handles
non-graphic, non-named characters correctly (e.g., `#\x01` for SOH). If not, add a
`unicode.IsGraphic(p.Value)` check with hex fallback.

### C3: BigFloat integer display

**Current code** (`values/big_float.go:357-368`):
```go
func (p *BigFloat) SchemeString() string {
    if p.nan {
        return "+nan.0"
    }
    if p.value.IsInf() {
        if p.value.Sign() < 0 {
            return "-inf.0"
        }
        return "+inf.0"
    }
    return p.value.Text('g', -1)
}
```
`p.value.Text('g', -1)` drops `.0` for integer-valued BigFloats. `(+ 1 1.0)` → `"2"`.

**Correct behavior** (`values/float.go:300-318`):
Float's `SchemeString()` uses `strconv.FormatFloat` with `'f'` format, then scans for
a decimal point and appends `.0` if missing. BigFloat must do the same.

**Fix:** After the `p.value.Text('g', -1)` call, check if the string contains a `.`
or `e`/`E`. If neither, append `.0`. This matches the Float pattern:

```go
s := p.value.Text('g', -1)
for i := 0; i < len(s); i++ {
    if s[i] == '.' || s[i] == 'e' || s[i] == 'E' {
        return s
    }
}
return s + ".0"
```

**Note:** `'g'` format already handles scientific notation (e.g., `1e20`), so we only
need to append `.0` when neither `.` nor exponent marker is present.

### L6: Bytevector AND Vector display format

**Current code** (`values/utils.go:33-47`):
```go
func formatIndexable(prefix string, length int, get func(int) Value) string {
    q := &strings.Builder{}
    q.WriteString(prefix)
    if length > 0 {
        q.WriteString(" ")        // BUG: space after prefix
        q.WriteString(get(0).SchemeString())
        for i := 1; i < length; i++ {
            q.WriteString(" ")
            q.WriteString(get(i).SchemeString())
        }
        q.WriteString(" ")        // BUG: space before closing paren
    }
    q.WriteString(")")
    return q.String()
}
```

**Impact:** This function is used by BOTH `ByteVector.SchemeString()` (line 165) and
`Vector.SchemeString()` (line 117). Fixing it changes BOTH formats:
- `#u8( 1 2 3 )` → `#u8(1 2 3)`
- `#( 1 2 3 )` → `#(1 2 3)`

**Fix:** Remove the leading and trailing space writes:
```go
func formatIndexable(prefix string, length int, get func(int) Value) string {
    q := &strings.Builder{}
    q.WriteString(prefix)
    if length > 0 {
        q.WriteString(get(0).SchemeString())
        for i := 1; i < length; i++ {
            q.WriteString(" ")
            q.WriteString(get(i).SchemeString())
        }
    }
    q.WriteString(")")
    return q.String()
}
```

**Warning:** This changes the output format for ALL vectors and bytevectors across the
entire test suite. Run `make test` and expect existing test expectations that match the
old `#( ... )` format to fail. Update them.

### Tests

Add to existing test files (table-driven, per `registry/CLAUDE.md`):

**`values/character_test.go`:**
- All 9 named characters round-trip: `(read (open-input-string (write-to-string #\alarm)))` etc.
- Non-named graphic character: `#\A` → `"#\\A"`

**`values/big_float_test.go`:**
- `(+ 1 1.0)` → SchemeString returns `"2.0"` not `"2"`
- Large integer-valued: `(+ 0.0 1000000)` → `"1e+06"` or `"1000000.0"` (verify which `Text('g', -1)` produces)

**`values/byte_vector_test.go` and `values/vector_test.go`:**
- Update existing SchemeString expectations from `#u8( ... )` to `#u8(...)` etc.

---

## Phase 2: Port I/O Fixes (C5, H6, L4, L5, M6) ✓

**Status:** Complete (PR #364, #365).
**Theme:** Port system conformance — short reads, crash, direction checks, edge cases.
**Effort:** Low–Medium
**PR scope:** `internal/extensions/io/`

### C5: read-bytevector short reads

**Current code** (`internal/extensions/io/prim_read_write.go`):
- `PrimReadBytevector` line 697: `n, err := p.Read(buf)` — single call
- `PrimReadBytevectorBang` line 749: `n, err := p.Read(buf)` — same pattern

A single `p.Read(buf)` on a `bufio.Reader` returns only the internally buffered data
(4096 bytes default). Reading 4000 bytes leaves 96 buffered; the next 4000-byte read
returns only 96.

**Fix:** Replace `p.Read(buf)` with `io.ReadFull(p, buf)` in both functions. Handle
the return:
- `io.ErrUnexpectedEOF` means partial read at actual EOF — return the `n` bytes read
- `io.EOF` means no data at all — return EOF object
- `nil` means full read — return the complete buffer

```go
n, err := io.ReadFull(p, buf)
if err != nil {
    if errors.Is(err, io.ErrUnexpectedEOF) {
        // Partial read at EOF — return what we got
        buf = buf[:n]
    } else if errors.Is(err, io.EOF) {
        mc.SetValue(values.EOFObject)
        return nil
    } else {
        return values.WrapForeignErrorf(values.ErrIOError, "read-bytevector: %v", err)
    }
}
```

Add `"io"` to imports if not present.

**For `PrimReadBytevectorBang`:** Same pattern but writes into the provided bytevector
slice instead of allocating a new one. The partial-read case sets the return value to
the count `n` rather than slicing a buffer.

### H6: current-input-port parameterize crash

**Current code** (`internal/extensions/io/state.go`):
```go
// Line 111
func GetCurrentInputPort() *values.CharacterInputPort {
    // ...
    return currentInputPortParam.Value().(*values.CharacterInputPort)  // Line 116: PANICS
}

// Line 138
func GetCurrentOutputPort() *values.CharacterOutputPort {
    // ...
    return currentOutputPortParam.Value().(*values.CharacterOutputPort)  // Line 143: PANICS
}
```

The type assertions panic when `current-input-port` is parameterized to a
`StringInputPort` (from `open-input-string`).

**Fix:** Change return types to interfaces. The callers need:
- Input: `values.TextualReader` (provides `ReadRune`, `UnreadRune` needed by `read-char`, `peek-char`)
- Output: `values.TextualWriter` (provides `WriteRune` needed by `write-char`)

But `getOptionalOutputPort` (line 40) returns `values.OutputPort`, not `TextualWriter`.
Two-part fix:

**Part 1:** Change `GetCurrentInputPort`:
```go
func GetCurrentInputPort() values.TextualReader {
    if currentInputPortParam == nil {
        return nil
    }
    return currentInputPortParam.Value().(values.TextualReader)
}
```

**Part 2:** Change `GetCurrentOutputPort`:
```go
func GetCurrentOutputPort() values.OutputPort {
    if currentOutputPortParam == nil {
        return nil
    }
    return currentOutputPortParam.Value().(values.OutputPort)
}
```

Use `values.OutputPort` (not `TextualWriter`) because callers assign the result to
`values.OutputPort` variables (lines 264, 548). The M6 fix (below) handles the
textual/binary distinction separately.

**Caller audit (already verified):**
- `getOptionalInputPort` (line 75): assigns to `values.TextualReader` — works
- `getOptionalOutputPort` (line 53): assigns to `values.OutputPort` — works
- `PrimWriteString` (line 266): assigns to `values.OutputPort` — works
- `PrimFlushOutputPort` (line 550): assigns to `values.OutputPort` — works

**Important:** Both `StringInputPort` and `StringOutputPort` implement the required
interfaces (they use `*bytes.Buffer` which provides `ReadRune`/`WriteRune`). Verify
with a quick grep that these types satisfy the interface.

### L4: read-string 0 returns EOF

**Current code** (`internal/extensions/io/prim_read_write.go:476-521`):
The function allocates `chars := make([]rune, 0, k.Value)`, the loop runs zero
iterations when `k==0`, then line 514-517 checks `if len(chars) == 0` and returns
`EOFObject`.

**Fix:** Add early return before the allocation and loop:
```go
if k.Value == 0 {
    mc.SetValue(values.NewString(""))
    return nil
}
```

Insert after the negative-value check (line 483) and before the allocation limit check
(line 487).

### L5: close-input-port / close-output-port direction

**Current code** (`internal/extensions/io/register.go:108,110`):
```go
{Name: "close-input-port", ParamCount: 1, Impl: PrimClosePort, ...},
{Name: "close-output-port", ParamCount: 1, Impl: PrimClosePort, ...},
```

Both map to the same `PrimClosePort` which accepts any `Port`.

**Fix:** Create two new functions in `prim_read_write.go`:

```go
func PrimCloseInputPort(mc *machine.MachineContext) error {
    p, ok := mc.Arg(0).(values.InputPort)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotAnInputPort,
            "close-input-port: expected an input port")
    }
    return p.Close()
}

func PrimCloseOutputPort(mc *machine.MachineContext) error {
    p, ok := mc.Arg(0).(values.OutputPort)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotAnOutputPort,
            "close-output-port: expected an output port")
    }
    // Flush before close for buffered output ports
    if err := p.Flush(); err != nil {
        return values.WrapForeignErrorf(values.ErrIOError, "close-output-port: flush: %v", err)
    }
    return p.Close()
}
```

**Question for Sonnet:** Check if sentinel errors `ErrNotAnInputPort` and
`ErrNotAnOutputPort` exist in `values/foreign_error.go`. If not, add them. Search for
existing port-related sentinels first.

**Question for Sonnet:** Check if `PrimClosePort` does flush-before-close. If so, the
`PrimCloseOutputPort` should match that behavior. Read `PrimClosePort` implementation.

Update `register.go` to point to the new functions.

### M6: Textual output ops accept binary ports

**Current code** (`internal/extensions/io/prim_read_write.go:40-60`):
```go
func getOptionalOutputPort(mc *machine.MachineContext) (values.OutputPort, error) {
    // ...
    p, ok := tuple.Car().(values.OutputPort)  // Line 55: accepts BinaryWriter too
    // ...
}
```

`BinaryWriter` implements `OutputPort`, so binary ports pass the check.

**Fix option A — Change the type assertion:**
```go
p, ok := tuple.Car().(values.TextualWriter)
```
But then the return type must change to `values.TextualWriter`, and all callers need
updating.

**Fix option B — Add explicit rejection (less invasive):**
After the existing `OutputPort` assertion succeeds, add:
```go
if _, isBinary := p.(values.BinaryWriter); isBinary {
    return nil, values.WrapForeignErrorf(values.ErrNotATextualPort,
        "expected a textual output port, got binary port")
}
```

**Recommendation:** Option B is less invasive. The function keeps its `OutputPort` return
type, and callers are unchanged. Only need to verify/add `ErrNotATextualPort` sentinel.

**Note:** There's a parallel `getOptionalInputPort` (line 63-80) that already checks
for `TextualReader` (line 77), so textual input ops correctly reject binary ports.
Only output side needs fixing.

### Tests

Add to `internal/extensions/io/prim_read_write_test.go` (table-driven):

- **C5:** Write 8192 bytes to temp file, `read-bytevector 4000` twice → lengths `(4000 4000)`.
  Also test partial EOF: 100-byte file, `read-bytevector 200` → 100 bytes, then EOF.
- **H6:** `(parameterize ((current-input-port (open-input-string "hi"))) (read-char))` → `#\h`
  Also: `(parameterize ((current-output-port (open-output-string))) (write-char #\A) ...)` → no crash
- **L4:** `(read-string 0 (open-input-string "hello"))` → `""`
- **L5:** `(close-input-port (open-output-string))` → error
  Also: `(close-output-port (open-input-string "x"))` → error
- **M6:** `(write-char #\A (open-output-bytevector))` → error
  Also: `(display "hi" (open-output-bytevector))` → error

---

## Phase 3: Parameterize & Environment (C1, C4) ✓

**Status:** Complete (PR #364). C1 uses `%parameter-raw-set!` in `bootstrap.scm`. C4 returns `NewChildTopLevelEnvironment()`.
**Theme:** Parameter and environment object semantics.
**Effort:** Medium
**PR scope:** `registry/core/bootstrap.scm`, `internal/extensions/eval/`

### C1: Parameterize converter double-applies

**Current macro** (`registry/core/bootstrap.scm:141-152`):
```scheme
(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () body ...)
     (begin body ...))
    ((parameterize ((param val) rest ...) body ...)
     (let ((p param)
           (new val)
           (old (param)))       ;; captures CONVERTED value (e.g., 20)
       (dynamic-wind
         (lambda () (p new))    ;; applies converter: new → converter(new)
         (lambda () (parameterize (rest ...) body ...))
         (lambda () (p old))))));; applies converter AGAIN: 20 → converter(20) = 40
```

The bug: `(p old)` in the after-thunk applies the converter to an already-converted
value. `(make-parameter 10 (lambda (x) (* x 2)))` stores 20. After parameterize, the
restore calls `(p 20)` which converts again to 40.

**Fix approach:** The parameter object needs a way to set its internal value without
applying the converter. There are two options:

**Option A — Scheme-level raw setter (preferred):**
Parameters in Wile are implemented as `machine.Parameter` (Go type). Check how
`(p value)` dispatch works — the parameter object is callable, and `(p)` reads while
`(p val)` writes through the converter.

Add an internal primitive `%parameter-raw-set!` that bypasses the converter:
```scheme
(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () body ...)
     (begin body ...))
    ((parameterize ((param val) rest ...) body ...)
     (let ((p param)
           (new val)
           (old (p)))
       (dynamic-wind
         (lambda () (p new))
         (lambda () (parameterize (rest ...) body ...))
         (lambda () (%parameter-raw-set! p old)))))))
```

**Implementation of `%parameter-raw-set!`:**
1. Find the `Parameter` type in `machine/` — locate where `(p value)` is dispatched
2. The Parameter stores the converted value. `%parameter-raw-set!` calls
   `param.SetValue(raw)` directly (no converter)
3. Register as a runtime primitive in `registry/core/`

**Question for Sonnet:** Search for `Parameter` type in `machine/`. How does parameter
application work? Is `(p value)` handled as a special form by the compiler, or does
it go through a callable interface? Understanding this dispatch is essential.

**Option B — Go-level internal value capture:**
Instead of `(old (param))` capturing the converted value, capture the raw internal
cell. This requires exposing parameter internals to Scheme, which is messier.

**Recommendation:** Option A (raw setter primitive). It's a clean separation.

### C4: scheme-report-environment returns interaction-environment

**Current code** (`internal/extensions/eval/prim_eval.go:231-252`):
```go
func PrimSchemeReportEnvironment(mc *machine.MachineContext) error {
    // ...
    // Return the current top-level environment
    // In a full implementation, this would return a restricted environment
    topLevel := mc.EnvironmentFrame().TopLevelEnv()
    topLevel.Name = "scheme-report-environment"
    mc.SetValue(topLevel)
    return nil
}
```

The comment itself says "In a full implementation, this would return a restricted
environment." It returns the exact same `TopLevelEnv()` as `interaction-environment`.

**`null-environment` already does the right thing** (`prim_eval.go:254-276`):
```go
callerTopLevel := mc.EnvironmentFrame().TopLevelEnv()
newTopLevel := callerTopLevel.NewChildTopLevelEnvironment()
```

**Fix:** Model `scheme-report-environment` after `null-environment`:
```go
callerTopLevel := mc.EnvironmentFrame().TopLevelEnv()
newTopLevel := callerTopLevel.NewChildTopLevelEnvironment()
newTopLevel.Name = "scheme-report-environment"
mc.SetValue(newTopLevel)
```

`NewChildTopLevelEnvironment()` creates a new environment sharing the caller's
symbol interning but with its own bindings. The R7RS bindings are inherited from
the parent, but new `define`s go to the child.

**Open question — Immutability:**
R7RS §6.12 says "The environment might or might not be immutable." This is
implementation-defined. For now, creating a fresh copy (mutable) is sufficient.
If immutability is desired later, add a `frozen` flag to `TopLevelEnvironment` that
blocks `SetOwnGlobalValue`.

**Open question — `environment` procedure:**
R7RS `(environment list ...)` returns an environment with specified library bindings.
Check if Wile implements this. If so, it has the same problem (returns the interaction
environment). The fix is the same: `NewChildTopLevelEnvironment()`.

### Tests

**`registry/core/prim_numeric_conversion_test.go` or new file:**
- C1: `(define p (make-parameter 10 (lambda (x) (* x 2)))) (p)` → `20`
  `(parameterize ((p 5)) (p))` → `10`
  `(p)` → `20` (not `40`)
- C1: Also test with identity converter (should be no-op):
  `(define p (make-parameter 10)) (parameterize ((p 5)) 'ok) (p)` → `10`

**`internal/extensions/eval/prim_eval_test.go`:**
- C4: `(eq? (interaction-environment) (scheme-report-environment 5))` → `#f`
- C4: `(eval 'define-test-var (scheme-report-environment 5))` → error (not found)
- C4: `(eval '(+ 1 2) (scheme-report-environment 5))` → `3` (standard bindings work)

---

## Phase 4: Numeric Precision (H2, H3, H4, M1, M2, L1, L2) ✓

**Status:** Complete (PR #364, #365).
**Theme:** Exactness preservation and IEEE 754 edge cases.
**Effort:** Medium–High
**PR scope:** `values/`, `extensions/math/`

### H4: Division by inexact zero → infinity

R7RS §6.2.6 + IEEE 754: Division by exact zero is an error. Division by inexact zero
returns `+inf.0`/`-inf.0`/`+nan.0`.

**Current behavior:** All division-by-zero paths error unconditionally. The zero check
in `registry/core/prim_arithmetic.go` (integerDivisionOp, lines 97-125) checks `v1 == 0`
without distinguishing exact from inexact.

**Fix approach:** The fix must happen at the Number.Divide dispatch level, not the
integer-specific level. When the divisor is an inexact zero (Float 0.0 or BigFloat 0.0):
- `positive / 0.0` → `+inf.0`
- `negative / 0.0` → `-inf.0`
- `0.0 / 0.0` → `+nan.0`

**Question for Sonnet:** Trace the `/` primitive's dispatch chain. Does `(/ 1 0.0)`
reach `Integer.Divide(Float(0.0))`? Or does it reach `Float.Divide(Float(0.0))`?
Understanding the promotion chain determines where to add the bypass.

For Float, Go's native `float64` division already returns `+Inf`/`-Inf`/`NaN` for
division by zero. The fix may be as simple as removing the zero-check guard for
inexact divisors.

### H2: sqrt exactness for perfect squares

**Current code** (`extensions/math/prim_math.go:228-260ish`):
```go
case *values.Integer:
    if v.Value < 0 {
        mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(float64(v.Value), 0))))
    } else {
        mc.SetValue(values.NewFloat(math.Sqrt(float64(v.Value))))
    }
```

Always returns Float/Complex. No perfect-square detection.

**Fix:** Before the float path, check if the integer is a perfect square:
```go
case *values.Integer:
    if v.Value < 0 {
        absRoot := int64(math.Sqrt(float64(-v.Value)))
        if absRoot*absRoot == -v.Value {
            mc.SetValue(values.NewBigComplex(
                values.NewInteger(0), values.NewInteger(absRoot)))
            return nil
        }
        mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(float64(v.Value), 0))))
    } else {
        root := int64(math.Sqrt(float64(v.Value)))
        if root*root == v.Value {
            mc.SetValue(values.NewInteger(root))
            return nil
        }
        mc.SetValue(values.NewFloat(math.Sqrt(float64(v.Value))))
    }
```

**Warning:** `int64(math.Sqrt(float64(v)))` can lose precision for large integers.
For values > 2^53, use `big.Int.Sqrt()` and verify `root*root == v`.

**Rational case:** `(sqrt 9/4)` → `3/2`. Check numerator and denominator separately:
```go
case *values.Rational:
    num := v.Num()
    denom := v.Denom()
    numRoot := new(big.Int).Sqrt(new(big.Int).Abs(num))
    denomRoot := new(big.Int).Sqrt(denom)
    if new(big.Int).Mul(numRoot, numRoot).Cmp(new(big.Int).Abs(num)) == 0 &&
       new(big.Int).Mul(denomRoot, denomRoot).Cmp(denom) == 0 {
        // Perfect square rational
        if num.Sign() < 0 {
            // Negative rational: result is imaginary
            mc.SetValue(values.NewBigComplex(
                values.NewInteger(0),
                values.NewRationalFromBigInt(numRoot, denomRoot)))
        } else {
            mc.SetValue(values.NewRationalFromBigInt(numRoot, denomRoot))
        }
        return nil
    }
    // Fall through to float
```

### H3: Exact complex division

**Current code:** Complex division at `values/complex.go` uses Go's built-in
`complex128` division (line ~109). BigComplex division at `values/big_complex.go`
uses `toBigFloat` conversion in the general case (lines 161-188), losing exactness.

`(/ 3+4i 1+2i)` should return `11/5-2/5i` (exact).

**Fix:** In BigComplex's divide dispatch, when both parts are exact (BigInteger or
Rational), use rational arithmetic:
```
(a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c²+d²)
```
Compute `ac`, `bd`, `bc`, `ad`, `c²`, `d²` using exact arithmetic (Integer/Rational
operations), then construct the result as exact BigComplex.

**Question for Sonnet:** How does `(/ 3+4i 1+2i)` dispatch? Does `3+4i` parse as
`Complex` (float64-backed) or `BigComplex` (exact-backed)? If it starts as `Complex`,
the fix needs to be in `Complex.Divide` or the value must promote to BigComplex first.
Check the tokenizer/parser for complex literal representation.

### M1: expt with rational exponents

**Current code** (`extensions/math/prim_math.go:228-385`):
The `PrimExpt` function handles integer exponents exactly (lines 242-312) but falls
through to `math.Pow(bf, ef)` for rational exponents (line 383), always returning Float.

**Fix:** For `(expt base rational-exp)` where `rational-exp` has denominator `n`:
1. If `n` is small (2, 3, 4), check if base^(1/n) is exact
2. For n=2: this is sqrt — reuse the H2 perfect-square logic
3. For n=3: check if `root^3 == base`

This is only worth doing for small denominators. For general rational exponents,
the float path is correct.

**Minimum viable fix:** Handle `(expt x 1/2)` by delegating to the sqrt logic from H2.

### M2: real-part/imag-part/magnitude on non-complex

**Current code** (`extensions/math/prim_math.go:549-632`):
```go
// PrimRealPart
case *values.Integer:
    mc.SetValue(values.NewFloat(float64(v.Value)))  // WRONG: loses exactness

// PrimImagPart
case *values.Integer, *values.Float, *values.Rational:
    mc.SetValue(values.NewFloat(0))  // WRONG: should be exact 0

// PrimMagnitude
case *values.Integer:
    mc.SetValue(values.NewFloat(math.Abs(float64(v.Value))))  // WRONG: loses exactness
```

**Fix — `real-part`:** For non-complex reals, return the argument unchanged:
```go
case *values.Integer, *values.BigInteger, *values.Float, *values.BigFloat, *values.Rational:
    mc.SetValue(o)  // Real part of a real is itself
```

**Fix — `imag-part`:** Return exact zero:
```go
case *values.Integer, *values.BigInteger, *values.Float, *values.BigFloat, *values.Rational:
    mc.SetValue(values.NewInteger(0))  // Exact zero
```

**Fix — `magnitude`:** Return `abs(x)` preserving type:
```go
case *values.Integer:
    if v.Value < 0 {
        mc.SetValue(v.Negate())
    } else {
        mc.SetValue(v)
    }
case *values.Rational:
    // Use Abs method if available, otherwise negate if negative
case *values.Float:
    mc.SetValue(values.NewFloat(math.Abs(v.Value)))
// etc.
```

**Question for Sonnet:** Check if `Integer`, `Rational`, `BigInteger` have `Abs()` or
`Negate()` methods. If `Negate()` exists but `Abs()` doesn't, use conditional negate.

### L1: number->string ignores radix for BigInt

**Current code** (`extensions/math/prim_math.go:1224-1271`):
```go
case *values.BigInteger:
    mc.SetValue(values.NewString(v.SchemeString()))  // Line 1265: ignores radix!
```

The `radix` variable is computed at lines 1228-1239 but only used by the `Integer` case
(line 1243: `strconv.FormatInt(v.Value, radix)`). The `BigInteger` case at line 1265
calls `SchemeString()` which always produces decimal.

**Fix:**
```go
case *values.BigInteger:
    mc.SetValue(values.NewString(v.BigInt().Text(radix)))
```

`big.Int.Text(base)` outputs the number in the given base.

**Note:** Other types (`Rational`, `Complex`, `BigComplex`, `BigFloat`) also ignore
radix. R7RS §6.2.7 says "If z is inexact, the radix is 10" — so Float/BigFloat
ignoring radix is correct. For Rational and Complex with non-10 radix, the behavior
is implementation-defined. Fix only BigInteger for now (the reported bug).

### L2: positive?/negative? reject real-valued complex

**Current code** (`registry/core/prim_predicates.go:119-127`):
```go
var PrimPositiveQ = helpers.MakeNumericPredicate[values.RealNumber](
    "positive?", values.ErrNotANumber, values.RealNumber.IsPositive,
)
```

Uses `RealNumber` type constraint. `Complex` and `BigComplex` don't implement
`RealNumber`, so `(positive? 3+0i)` errors.

**Fix:** Before the `RealNumber` type switch, check if the value is a complex with
zero imaginary part and extract the real part:

```go
func PrimPositiveQ(mc *machine.MachineContext) error {
    v := mc.Arg(0)
    // Handle real-valued complex numbers
    if c, ok := v.(values.ComplexNumber); ok {
        if /* imag part is zero */ {
            v = c.RealPart()
        }
    }
    r, ok := v.(values.RealNumber)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotANumber, "positive?: ...")
    }
    mc.SetValue(values.BoolToBoolean(r.IsPositive()))
    return nil
}
```

**Question for Sonnet:** Check if `ComplexNumber` interface has `ImagPart()` and how
to test for zero. Also check `Complex.Value` — for `complex128`, check
`imag(v.Value) == 0`. For `BigComplex`, check `imag.IsZero()`. The helper
`MakeNumericPredicate` can't handle this, so you'll need to inline the logic.

### Tests

All findings have reproduction code in `plans/R7RS-CONFORMANCE-REVIEW.md`. Use those
directly as test cases, plus edge cases:

- H4: `(/ 1 0.0)` → `+inf.0`, `(/ -1 0.0)` → `-inf.0`, `(/ 0.0 0.0)` → `+nan.0`
- H2: `(sqrt 4)` → `2` (exact), `(sqrt 9/4)` → `3/2`, `(sqrt -4)` → `0+2i`
- H2 negative: `(sqrt 5)` → Float (not perfect square)
- M2: `(real-part 3)` → `3`, `(imag-part 3)` → `0`, `(exact? (imag-part 3))` → `#t`
- L1: `(number->string (expt 2 64) 16)` → hex string

---

## Phase 5: String->Number Parsing (H5, L3) ✓

**Status:** Complete (PR #364, #365). L3 (tokenizer prefix ordering) fixed in parser.
**Theme:** Number reader completeness.
**Effort:** Medium
**PR scope:** `extensions/math/prim_math.go`, `internal/tokenizer/`

### H5: string->number complex/special values

**Current code** (`extensions/math/prim_math.go:1274-1403`):
`PrimStringToNumber` uses its own parsing loop (NOT the tokenizer). It handles:
- Integer (decimal, hex, octal, binary)
- Rational (`n/d`)
- Float (with optional exponent)
- Prefix directives (`#e`, `#i`, `#b`, `#o`, `#d`, `#x`) in either order

It does NOT handle:
- Complex literals: `3+4i`, `3-4i`, `+i`, `-i`, `0+i`
- Special values: `+inf.0`, `-inf.0`, `+nan.0`
- Imaginary special values: `+inf.0i`, `-inf.0i`, `+nan.0i`

**Fix approach:** After the existing parsing (which handles the real part), check for:
1. Remaining `+` or `-` followed by number and `i` → complex
2. Input is exactly `+inf.0`, `-inf.0`, `+nan.0` → special Float
3. Input is exactly `+i` or `-i` → pure imaginary unit

The tokenizer already handles all these forms in source code (see the extensive
`TokenizerState` constants in `internal/tokenizer/CLAUDE.local.md`). The implementation
strategy is:

**Option A — Extend the hand-rolled parser in `prim_math.go`:**
Add special-value detection at the start (before integer/float parsing), then add
complex-suffix detection after parsing the real part. This keeps the code paths separate.

**Option B — Delegate to the tokenizer:**
Feed the string to the tokenizer, get back a token, and convert the token to a number.
This reuses the existing, well-tested parsing logic.

**Recommendation:** Option B is more maintainable but risks behavioral differences if
the tokenizer has features not appropriate for `string->number` (e.g., `#z` big integer
prefix). If using Option B, filter the token type to ensure only R7RS-standard number
forms are accepted.

**Question for Sonnet:** Before implementing, test whether the tokenizer-based approach
works: feed `"+inf.0"` to `tokenizer.Tokenize("+inf.0", false)`, check what token
state it produces. If it produces a valid number token, Option B is viable.

### L3: Number prefix ordering (tokenizer only)

**IMPORTANT CORRECTION:** The `string->number` primitive already handles both prefix
orderings correctly. Tests at `registry/core/prim_numeric_conversion_test.go:502-506`
confirm `#e#x2a` and `#x#e2a` both work. The bug is ONLY in the **tokenizer** when
reading source code literals.

**Evidence:** Integration test at `integration/testdata/r7rs-tests.scm:2435-2436`:
```scheme
;; WILE KNOWN LIMITATION: Some prefix combinations and non-decimal fractions not supported
#;(test-numeric-syntax "#x#i10" 16.0 "16.0" "16.")
```

The tokenizer emits `#x` as a `MarkerBase16` token, then tries to parse the
following `#e10` but fails because after setting `radix=16`, the parser/tokenizer
doesn't recognize a second `#` prefix token in that context.

**Fix:** The issue is in how the parser combines prefix marker tokens. After seeing
`MarkerBase16`, it expects a number but gets another marker (`MarkerNumberExact`).

**Question for Sonnet:** Trace the parser's handling of `MarkerBase*` tokens. How does
it combine `#e` + `#x10`? (This works.) How does it handle `#x` + `#e10`? (This fails.)
The parser is in `internal/parser/`. The difference reveals the bug.

### Tests

**`extensions/math/prim_math_test.go`:**
- `(string->number "+inf.0")` → Float +inf
- `(string->number "-inf.0")` → Float -inf
- `(string->number "+nan.0")` → Float NaN (test with `nan?`)
- `(string->number "3+4i")` → Complex 3+4i
- `(string->number "+i")` → Complex 0+1i
- `(string->number "-i")` → Complex 0-1i
- `(string->number "0+i")` → Complex 0+1i
- `(string->number "1+inf.0i")` → Complex with inf imaginary

**Tokenizer tests (`internal/tokenizer/tokenizer_test.go`):**
- `#x#e10` → exact hex 16
- `#x#i10` → inexact hex 16.0

---

## Phase 6: Control & Exception Semantics (M3, M4, M5) ✓

**Status:** Complete (PR #366, #367). M3 uses R7RS §7.3 double `call/cc` pattern. M5 uses `SetValues(sub.GetValues()...)`. M4 not reproducible.
**Theme:** Exception handling and continuation interactions.
**Effort:** Medium–High
**PR scope:** `registry/core/bootstrap.scm`, `machine/`, `internal/extensions/eval/`

### M3: guard re-raise with with-exception-handler

**Current macro** (`registry/core/bootstrap.scm:155-181`):
```scheme
(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) body ...)
     (call/cc
       (lambda (guard-continuation)
         (with-exception-handler
           (lambda (condition)
             (guard-continuation
               (let ((var condition))
                 (guard-aux var clause ...))))
           (lambda () body ...)))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ;; ... clauses ...
    ((guard-aux var)
     (raise var))))    ;; <-- this re-raise happens inside the handler's extent
```

**The problem:** When no clause matches, `guard-aux` falls through to `(raise var)`.
But this executes inside `with-exception-handler`'s handler closure (via
`guard-continuation`). The re-raised exception triggers "handler returned from
non-continuable exception" because `raise` invokes the handler, which returns (since
`guard-continuation` already escaped).

**R7RS §4.2.7 specifies:** "If every <cond clause>'s <test> evaluates to #f, the
exception is re-raised in the dynamic extent of the original `raise` call."

**Fix — R7RS reference implementation pattern:**
The guard must capture a continuation to the point of the original raise, so the
re-raise happens in the original dynamic extent:

```scheme
(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) body ...)
     (call/cc
       (lambda (guard-k)
         (with-exception-handler
           (lambda (condition)
             (call/cc
               (lambda (handler-k)
                 (guard-k
                   (let ((var condition))
                     (guard-aux
                       (lambda () (handler-k (raise var)))  ;; re-raise thunk
                       var clause ...))))))
           (lambda () body ...)))))))
```

The `handler-k` continuation returns to the handler's dynamic extent, where `raise`
is called. This is the standard R7RS approach.

**Warning:** This is one of the trickiest parts of R7RS. The `guard-aux` macro must
also change to accept a re-raise thunk parameter. Test thoroughly.

**Question for Sonnet:** Read R7RS §4.2.7 carefully. The spec gives a sample
implementation. Compare it with the current Wile implementation to understand the gap.

### M4: dynamic-wind double-fires after thunks at top level

Nested `dynamic-wind` + `call/cc` escape fires after-thunks twice at top level but
works correctly inside `let`.

**Root cause hypothesis:** At top level, each expression is compiled and run as a
separate unit. The continuation captured by `call/cc` may interact differently with
the winding stack when restored at top level vs. inside a nested scope.

**Investigation steps for Sonnet:**
1. Create minimal repro at top level and inside `let`
2. Add debug logging to `machine/dynamic_wind.go` — `UnwindTo` and winding frame push/pop
3. Compare the winding stack states between the two cases
4. The frame identity (`DynamicWindFrame.ID`) should prevent double-execution —
   if it doesn't, the stack comparison logic is wrong

**Location:** `machine/dynamic_wind.go` (winding stack), `registry/core/prim_control.go:247-329`
(dynamic-wind primitive), `machine/machine_context.go` (continuation restore).

### M5: eval drops multiple return values

**Current code** (`internal/extensions/eval/prim_eval.go:72-77`):
```go
err = sub.Run()
// ...
mc.SetValue(sub.GetValue())  // Line 77: only captures first value
```

`GetValue()` returns a single `Value`. Multiple return values from `(values 1 2 3)`
are stored differently in the VM.

**Fix approach:**
1. Find how multiple values are stored after `sub.Run()` — look for `GetValues()` or
   a values list on `MachineContext`
2. Transfer all values from the sub-context to the calling context

**Question for Sonnet:** Search for `GetValues`, `MultipleValues`, `values` storage
in `machine/machine_context.go`. How does `call-with-values` retrieve multiple values?
The same mechanism should be used by eval.

### Tests

- M3: The exact reproduction from the review:
  ```scheme
  (with-exception-handler
    (lambda (e) (list 'caught e))
    (lambda ()
      (guard (inner ((symbol? inner) 'sym))
        (raise 42))))
  ```
  Expected: `(caught 42)`
- M4: Top-level vs nested dynamic-wind + call/cc (exact repro needed)
- M5: `(call-with-values (lambda () (eval '(values 1 2 3) (interaction-environment))) list)` → `(1 2 3)`

---

## Phase 7: Vector Patterns in syntax-rules (M7) ✓

**Status:** Complete (PR #367). `ByteCodeVisitCarAsVector` + vector-to-pair-chain conversion. M8 confirmed not-a-bug.
**Theme:** `syntax-rules` vector patterns.
**Effort:** Medium (single new bytecode + 5 type-switch additions)
**PR scope:** `internal/match/`, `machine/compile_syntax_rules.go`

### M8 Resolution: Not a Bug

M8 (dotted pair patterns) was investigated and found to be working correctly.
The conformance review's test case was misleading:

```scheme
(define-syntax rest-test
  (syntax-rules ()
    ((rest-test a . b) (list a b))))
(rest-test 1 2 3)
```

`b` captures `(2 3)` correctly. The error is correct Scheme behavior: `(list 1 (2 3))`
evaluates `(2 3)` as a form, calling `2` as a procedure. The template `(list a b)` places
`b`'s captured syntax in evaluation position. Verified with quote, cons, and dotted templates.

All dotted pair edge cases pass: `(dt . b)`, `(dt a . b)`, `(dt a b . c)`, empty rest `()`.

### M7: Vector patterns with pattern variables

**R7RS §4.3.2** defines vector patterns:
```
<pattern> → #(<pattern> ...)
           | #(<pattern> ... <pattern> <ellipsis> <pattern> ...)
```

**Root cause**: Five functions lack `*syntax.SyntaxVector` handling:

| Function | File | Impact |
|----------|------|--------|
| `collectPatternVariablesWithEllipsis` | `machine/compile_syntax_rules.go` | Variables inside vectors never discovered |
| `collectFreeIdentifiersWithEllipsis` | `machine/compile_syntax_rules.go` | Free ids in vector templates missed |
| `analyzeRecursive` | `internal/match/pattern_analyzer.go` | Analysis returns false for vectors |
| `compileElement` | `internal/match/syntax_compiler.go` | Falls through to literal comparison |
| `findSyntaxVarsRecursive` | `internal/match/syntax_adapter.go` | Template expansion misses vector vars |

Template expansion already handles `SyntaxVector` (syntax_adapter.go:353-370). Only the
pattern compilation and variable discovery paths are missing.

**Reproduction** (confirmed):
```scheme
(define-syntax vec-first
  (syntax-rules ()
    ((vec-first #(x rest ...)) x)))
(vec-first #(1 2 3))
;; Error: syntax-rules: no matching clause for input
```

All vector patterns fail, including empty `#()` and literal-only `#(1 2)`.

### Implementation Strategy: Vector-to-List Conversion

**Add one new bytecode** `ByteCodeVisitCarAsVector` that:
1. Checks the input car is a `*syntax.SyntaxVector`
2. Converts its elements to a temporary `SyntaxPair` chain
3. Pushes that chain onto the syntax stack

All existing pair-based matching (CaptureCar, CompareCar, ellipsis loops, Done) then
works unchanged for the vector's contents. This avoids adding N vector-specific bytecodes.

At compile time, the `SyntaxVector` pattern is converted to a `SyntaxPair` and pushed
onto the compiler stack with the same conversion. The compiler sees it as a regular list
pattern after `ByteCodeVisitCarAsVector`.

### Step-by-Step

**Step 1: Variable discovery** (`machine/compile_syntax_rules.go`)

Add `*syntax.SyntaxVector` case to `collectPatternVariablesWithEllipsis`:
```go
case *syntax.SyntaxVector:
    for _, elem := range p.Values {
        err := collectPatternVariablesWithEllipsis(elem, literalSyntax, false, variables, varSyntax, ellipsis)
        if err != nil {
            return err
        }
    }
```

Add `*syntax.SyntaxVector` case to `collectFreeIdentifiersWithEllipsis`:
```go
case *syntax.SyntaxVector:
    for _, elem := range t.Values {
        collectFreeIdentifiersWithEllipsis(env, elem, patternVars, freeIds, ellipsis)
    }
```

**Step 2: Pattern analysis** (`internal/match/pattern_analyzer.go`)

Add `*syntax.SyntaxVector` case to `analyzeRecursive`:
```go
case *syntax.SyntaxVector:
    hasVars := false
    for _, elem := range t.Values {
        if analyzeRecursive(elem, variables, analysis) {
            hasVars = true
        }
    }
    return hasVars
```

Note: `PatternAnalysis` maps `*values.Pair` pointers, not `SyntaxValue`. Vector
patterns are converted to pair chains at compile time, so the analysis map will use
the converted pair pointers. The analysis for vector elements must be run AFTER
conversion so the pointers align with what the compiler sees.

**Step 3: New bytecode** (`internal/match/bytecode_navigate.go`)

```go
// ByteCodeVisitCarAsVector checks that the car of the current pair is a
// SyntaxVector, converts its elements to a SyntaxPair chain, and pushes
// the chain onto the syntax stack. This enables pair-based matching of
// vector pattern contents.
type ByteCodeVisitCarAsVector struct{}

func (ByteCodeVisitCarAsVector) String() string {
    return "VisitCarAsVector"
}
```

**Step 4: Matcher execution** (`internal/match/match.go`)

Add `ByteCodeVisitCarAsVector` case to the main switch in `MatchSyntaxWithLiterals`:
```go
case ByteCodeVisitCarAsVector:
    if syntax.IsSyntaxEmptyList(p.syntaxStack[lvs-1].pr) {
        return ErrNotAMatch
    }
    car := p.syntaxStack[lvs-1].pr.SyntaxCar()
    vec, ok := car.(*syntax.SyntaxVector)
    if !ok {
        return ErrNotAMatch // Input is not a vector
    }
    // Convert vector elements to a SyntaxPair chain
    chain := vectorToSyntaxPairChain(vec)
    p.syntaxStack = append(p.syntaxStack, syntaxPathEntry{pr: chain})
    lvs = len(p.syntaxStack)
```

Add helper:
```go
// vectorToSyntaxPairChain converts a SyntaxVector's elements into a
// SyntaxPair chain for pair-based matching. Empty vectors produce
// SyntaxEmptyList.
func vectorToSyntaxPairChain(vec *syntax.SyntaxVector) syntax.SyntaxTuple {
    if len(vec.Values) == 0 {
        return syntax.SyntaxEmptyList
    }
    var chain syntax.SyntaxValue = syntax.SyntaxEmptyList
    for i := len(vec.Values) - 1; i >= 0; i-- {
        chain = syntax.NewSyntaxCons(vec.Values[i], chain, vec.SourceContext())
    }
    return chain.(syntax.SyntaxTuple)
}
```

**Step 5: Pattern compiler** (`internal/match/syntax_compiler.go`)

Add `*syntax.SyntaxVector` case to `compileElement` (after the `SyntaxPair` check):
```go
// Handle vector elements
vec, ok := element.(*syntax.SyntaxVector)
if ok {
    return compileVectorElement(vis, stack, vec, element, elementStart)
}
```

Add `compileVectorElement`:
```go
func compileVectorElement(vis *SyntaxCompiler, stack []syntaxCompilerStackEntry,
    vec *syntax.SyntaxVector, element syntax.SyntaxValue, elementStart int,
) ([]syntaxCompilerStackEntry, bool) {
    l := len(stack)

    if len(vec.Values) == 0 {
        // Empty vector pattern #() — verify input car is empty vector
        vis.codes = append(vis.codes, ByteCodeVisitCarAsVector{})
        vis.codes = append(vis.codes, ByteCodeDone{})
        stack[l-1].pr, _ = stack[l-1].pr.SyntaxCdr().(*syntax.SyntaxPair)
        stack[l-1].lastElement = element
        stack[l-1].lastElementStart = elementStart
        return stack, true
    }

    // Non-empty vector — convert to pair chain for pair-based matching
    vis.codes = append(vis.codes, ByteCodeVisitCarAsVector{})
    chain := vectorElementsToPairChain(vec)
    stack[l-1].pr, _ = stack[l-1].pr.SyntaxCdr().(*syntax.SyntaxPair)
    stack[l-1].lastElement = element
    stack[l-1].lastElementStart = elementStart

    // Push converted chain for nested processing
    stack = append(stack, syntaxCompilerStackEntry{
        pr:        chain,
        variables: map[string]struct{}{},
    })
    return stack, true
}

// vectorElementsToPairChain converts a SyntaxVector's elements to a SyntaxPair
// chain for pattern compilation. Used at compile time to reuse pair-based
// compilation for vector pattern contents.
func vectorElementsToPairChain(vec *syntax.SyntaxVector) *syntax.SyntaxPair {
    var chain syntax.SyntaxValue = syntax.SyntaxEmptyList
    for i := len(vec.Values) - 1; i >= 0; i-- {
        chain = syntax.NewSyntaxCons(vec.Values[i], chain, vec.SourceContext())
    }
    return chain.(*syntax.SyntaxPair)
}
```

**Step 6: Template variable discovery** (`internal/match/syntax_adapter.go`)

Add `*syntax.SyntaxVector` case to `findSyntaxVarsRecursive`:
```go
case *syntax.SyntaxVector:
    for _, elem := range t.Values {
        p.findSyntaxVarsRecursive(elem, vars)
    }
```

**Step 7: Pattern analysis for vector contents**

**Problem**: `PatternAnalysis` uses `*syntax.SyntaxPair` pointer identity to map subtrees
to their variable analysis. `compileVectorElement` creates fresh `SyntaxPair` chains from
vector elements. The top-level `AnalyzePattern()` call (before `compile()`) doesn't see
these chains because they don't exist yet.

Without correct analysis, `previousElementHasVariables` returns `false` for elements
preceding `...` inside a vector, and ellipsis compilation breaks — treating `...` as a
literal instead of a repetition.

**Solution**: In `compileVectorElement`, after converting the vector to a pair chain,
run `AnalyzePattern(chain, vis.variables)` and merge the results into `vis.analysis`.
The converted chain pairs are fresh allocations that won't collide with existing map keys.

Add a `Merge` method to `PatternAnalysis`:

```go
// Merge incorporates analysis results from another PatternAnalysis.
// Used when vector patterns are converted to pair chains at compile time,
// creating fresh SyntaxPair nodes that need analysis entries.
func (p *PatternAnalysis) Merge(other *PatternAnalysis) {
    for k, v := range other.containsVariables {
        p.containsVariables[k] = v
    }
    for k, v := range other.variablesInSubtree {
        p.variablesInSubtree[k] = v
    }
}
```

Then in `compileVectorElement`:
```go
localAnalysis := AnalyzePattern(chain, vis.variables)
vis.analysis.Merge(localAnalysis)
```

**Step 2's `analyzeRecursive` vector case** is still needed for the top-level analysis
to correctly report that a vector-containing subtree has variables. The top-level pattern
`(foo #(x y))` is a `SyntaxPair` whose car is `foo` and cdr is `(#(x y))`. When
`analyzeRecursive` hits the `SyntaxVector` at the car of `(#(x y))`, it needs to return
`true` so the parent pair is marked as containing variables. Without this, top-level
analysis would say "no variables" for the entire `(#(x y))` subtree.

The vector case in `analyzeRecursive` (Step 2) handles this. It doesn't need to create
pair chains or store entries in the analysis maps — it just recurses into elements and
returns whether any contain variables. The `Merge` in `compileVectorElement` handles the
pair-chain-specific entries.

### handleByteCodeDone and isPairPattern for vectors

`handleByteCodeDone` pops the syntax stack and advances the parent. When a vector
pattern's content finishes (Done emitted), the logic must handle the fact that the
parent position was advanced by `compileVectorElement` (same as `compilePairElement`).
No changes needed — the stack management is identical to nested pair patterns.

`isPairPattern` (syntax_compiler.go) must also recognize `ByteCodeVisitCarAsVector`
as a "descend" pattern, since `VisitCarAsVector` + `Done` auto-advances the parent
just like `VisitCar` + `Done`. Without this, ellipsis loops containing nested vector
patterns would emit a spurious `VisitCdr`:

```go
func isPairPattern(codes []SyntaxCommand) bool {
    if len(codes) < 2 {
        return false
    }
    _, startsWithVisitCar := codes[0].(ByteCodeVisitCar)
    _, startsWithVisitVector := codes[0].(ByteCodeVisitCarAsVector)
    _, endsWithDone := codes[len(codes)-1].(ByteCodeDone)
    return (startsWithVisitCar || startsWithVisitVector) && endsWithDone
}
```

### Tests

**Go unit tests** (in `internal/match/`):

1. `TestSyntaxCompiler_VectorPattern` — Compile `#(x y)`, verify `ByteCodeVisitCarAsVector`
   emitted followed by `CaptureCar x`, `CaptureCar y`, `Done`, `Done`.
2. `TestMatcher_VectorPattern` — Match `(foo #(1 2))` against `(foo #(x y))`, verify
   bindings `x=1, y=2`.
3. `TestMatcher_VectorPatternEmpty` — Match `(foo #())` against `(foo #())`.
4. `TestMatcher_VectorPatternEllipsis` — Match `(foo #(1 2 3))` against `(foo #(x rest ...))`,
   verify `x=1`, `rest` has ellipsis bindings `[2, 3]`.
5. `TestMatcher_VectorPatternMismatch` — `(foo (1 2 3))` vs `(foo #(x y z))` → no match
   (input is list, not vector).
6. `TestMatcher_VectorPatternLiterals` — `(foo #(1 x))` with literal `1`, verify matching.

**Integration tests** (Scheme):

```scheme
;; Basic vector pattern
(define-syntax vec-first
  (syntax-rules ()
    ((vec-first #(x rest ...)) x)))
(test "vec-first" 1 (vec-first #(1 2 3)))

;; Vector with multiple captures
(define-syntax vec-pair
  (syntax-rules ()
    ((vec-pair #(a b)) (list a b))))
(test "vec-pair" '(1 2) (vec-pair #(1 2)))

;; Empty vector pattern
(define-syntax vec-empty
  (syntax-rules ()
    ((vec-empty #()) 'empty)))
(test "vec-empty" 'empty (vec-empty #()))

;; Vector with literal
(define-syntax vec-tagged
  (syntax-rules (point)
    ((vec-tagged #(point x y)) (list x y))))
(test "vec-tagged" '(3 4) (vec-tagged #(point 3 4)))

;; Vector in template (already works, verify round-trip)
(define-syntax make-vec
  (syntax-rules ()
    ((make-vec x y z) #(x y z))))
(test "make-vec" #(1 2 3) (make-vec 1 2 3))

;; Nested vector pattern
(define-syntax vec-nested
  (syntax-rules ()
    ((vec-nested #(#(a b) c)) (list a b c))))
(test "vec-nested" '(1 2 3) (vec-nested #(#(1 2) 3)))
```

---

## Phase 8: Edge-Case NaN Guards (E2, E3) ✓

**Status:** Complete (PR #364, #365).
**Theme:** NaN comparison edge cases in helpers and cross-type dispatch.
**Effort:** Low
**PR scope:** `registry/helpers/`, `values/`

### E2: helpers.Eqv returns true for BigFloat NaN

**Current code** (`registry/helpers/equality.go:49-53`):
```go
case *values.BigFloat:
    vb, ok := b.(*values.BigFloat)
    if ok {
        return va.BigFloatValue().Cmp(vb.BigFloatValue()) == 0
    }
```

BigFloat NaN is stored as a `nan bool` flag with a zero-valued `*big.Float` in the
`value` field. When `nan` is true, `BigFloatValue()` returns the zero `*big.Float`,
so `Cmp` returns 0 — incorrectly treating NaN == NaN as true.

**Fix:** Add NaN check before comparison:
```go
case *values.BigFloat:
    vb, ok := b.(*values.BigFloat)
    if ok {
        if va.IsNaN() || vb.IsNaN() {
            return false
        }
        return va.BigFloatValue().Cmp(vb.BigFloatValue()) == 0
    }
```

**Question for Sonnet:** Verify that `BigFloat` has an `IsNaN()` method. If not,
check the `nan` field directly (may need to add a method or use the existing one).
The `BigFloat` type definition is in `values/big_float.go`.

### E3: Float.EqualTo panics on NaN vs BigFloat

**Current code** (`values/float.go:283-292`):
```go
func (p *Float) EqualTo(v Value) bool {
    switch other := v.(type) {
    case *Float:
        return p.Value == other.Value
    case *BigFloat:
        vf := new(big.Float).SetFloat64(p.Value)  // Line 288: PANICS if p.Value is NaN
        return vf.Cmp(other.BigFloatValue()) == 0
    }
    return false
}
```

Go's `big.Float.SetFloat64(NaN)` panics because `big.Float` does not represent NaN.

**Fix:** Add NaN guard in the BigFloat case:
```go
case *BigFloat:
    if math.IsNaN(p.Value) || other.IsNaN() {
        return false
    }
    vf := new(big.Float).SetFloat64(p.Value)
    return vf.Cmp(other.BigFloatValue()) == 0
```

Also add an Inf guard — `big.Float.SetFloat64(+Inf)` works in Go, but verify.

### Tests

Add to `values/float_test.go` and `values/big_number_test.go`:

- `Float(NaN).EqualTo(BigFloat(1.0))` → `false` (no panic)
- `Float(NaN).EqualTo(BigFloat(NaN))` → `false` (no panic)
- `BigFloat(NaN).EqualTo(BigFloat(NaN))` via Eqv → `false`
- `Eqv(BigFloat(NaN), BigFloat(NaN))` → `false`
- Cross-type: `Eqv(Float(NaN), BigFloat(1.0))` — shouldn't reach BigFloat case
  (different types), but verify no panic

**Note on Float NaN self-comparison:** `Float.EqualTo` for two Floats uses
`p.Value == other.Value`. IEEE 754 `NaN != NaN`, so this correctly returns `false`.
No fix needed for Float-Float NaN comparison.

---

## Summary

| Phase | Findings | Status | Theme |
|-------|----------|--------|-------|
| 1 | C2, C3, L6 | ✓ (C3 PR #369 open) | Display/write round-trip |
| 2 | C5, H6, L4, L5, M6 | ✓ | Port I/O |
| 3 | C1, C4 | ✓ | Parameters & environments |
| 4 | H2, H3, H4, M1, M2, L1, L2 | ✓ | Numeric precision |
| 5 | H5, L3 | ✓ | Number parsing |
| 6 | M3, M4, M5 | ✓ (M4 not reproducible) | Control & exceptions |
| 7 | M7 | ✓ (M8 not a bug) | Vector patterns |
| 8 | E2, E3 | ✓ | NaN edge cases |

**Total:** 26 findings across 8 phases. 24 fixed, 1 not reproducible (M4), 1 not a bug (M8).
Additionally: H1 (excluded) shipped in PR #368; E1 (excluded) crash prevention shipped in PR #367.

## Corrections from Source Verification

The following corrections were identified during source verification and are
reflected in the phase details above:

1. **L1 root cause confirmed:** `PrimNumberToString` at `prim_math.go:1265` uses
   `v.SchemeString()` for `BigInteger`, ignoring the `radix` variable. Fix: use
   `v.BigInt().Text(radix)`.

2. **L3 scope narrowed:** The `string->number` primitive already handles both prefix
   orderings (tests at `prim_numeric_conversion_test.go:502-506` confirm). The bug
   is ONLY in the tokenizer when reading source code literals like `#x#e10`.

3. **L6 scope widened:** `formatIndexable` in `values/utils.go` is shared by both
   `ByteVector.SchemeString()` and `Vector.SchemeString()`. Fixing it changes both
   formats. Existing test expectations will need updating.

4. **M7 R7RS confirmation:** R7RS §4.3.2 explicitly defines `#(<pattern> ...)` as a
   valid pattern form. The match package simply doesn't implement vector patterns.

5. **H6 caller audit complete:** `GetCurrentInputPort()` is used at `prim_read_write.go:75`
   (assigned to `TextualReader`) and `GetCurrentOutputPort()` at lines 53, 266, 550
   (assigned to `OutputPort`). Interface change is safe.

6. **E2 mechanism clarified:** BigFloat NaN uses a `nan bool` flag with a zero-valued
   `*big.Float`. `BigFloatValue()` returns the zero value, so `Cmp` returns 0.

7. **E3 confirmed:** `new(big.Float).SetFloat64(NaN)` panics in Go. Guard needed at
   `float.go:288`.
