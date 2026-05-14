# Port unification — implementation plan

**Date**: 2026-05-14 (revised after critique pass — fixes
constructor-count drift, `portKind`-type drift, I8 ownership,
SchemeString format, build-order of `ErrInvariantViolation`)
**Design**: `plans/2026-05-14-port-unification-design.md`
**Parent**: `plans/2026-05-13-values-structural-reduction.md` (Phase 2 of
the values SR roadmap).
**Status**: Ready for execution.
**Branch**: `feat/values-sr-phase2-port-unification`.
**PR scope**: One PR, breaking change (v1.x zero-consumer license).

## Pre-flight

Before the first implementation commit:

1. Verify master CI is green: `gh run list --branch master --limit 3`.
2. Run baseline locally to confirm the working tree is clean:
   `make lint && make covercheck`.
3. Capture Gabriel benchmark baseline for R-1 gate:
   `make bench-gabriel > /tmp/port-bench-baseline.txt`.
4. Branch from master: `git switch -c feat/values-sr-phase2-port-unification`.
5. Commit 1 — the plan files: `git add
   plans/2026-05-14-port-unification-design.md
   plans/2026-05-14-port-unification-impl.md && git commit -m "docs:
   port unification design + impl plan (values SR Phase 2)"`.

## Phase 1 — Introduce `*PortObject` and accessors (non-breaking parallel)

**Goal**: `*PortObject` exists alongside the 9 concrete port types;
nothing uses it yet; build and tests stay green.

### 1.1 Export helper interfaces

Edit `values/port_helpers.go`:

- Rename `runeWriter` → `RuneWriter` (exported).
- Rename `byteUnreader` → `ByteUnreader` (exported).
- Rename `runeUnreader` → `RuneUnreader` (exported).
- Rename `flusher` → `Flusher` (exported).
- Update the four `guarded<X>` helper signatures to take the exported
  names.

**Acceptance**: `go build ./values/...` clean. Existing tests pass
(only internal renames; concrete port types in this package call
through the renamed interfaces).

### 1.2 Add `values/port.go`

New file with the `*PortObject` struct, the `StringExtractor`
interface, and constructors. **Do not yet modify** the 9 concrete
port files or any constructor — both `*PortObject` and the old types
coexist.

```go
package values

import (
    "bytes"
    "io"

    "github.com/aalpar/wile/werr"
)

// StringExtractor accepts a buffer that can yield its accumulated
// bytes as a string. *bytes.Buffer satisfies it.
type StringExtractor interface {
    String() string
}

// PortObject is the single concrete representation of an R7RS port.
// Capability presence is encoded as nil-checks on the slot fields.
// Construction always goes through one of the New*Port factories,
// which call Validate before returning.
type PortObject struct {
    portBase

    // Read side.
    rdr io.Reader
    rb  io.ByteReader
    rr  io.RuneReader
    urb ByteUnreader
    urr RuneUnreader

    // Write side.
    wrt io.Writer
    wb  io.ByteWriter
    wr  RuneWriter
    ws  io.StringWriter
    flsh Flusher

    // Output-port extractors. sext and ext are mutually exclusive (I7).
    ext  ByteVectorExtractor
    sext StringExtractor
}

// Compile-time interface satisfaction. Port (interface) is declared
// in values.go and survives; PortObject is the sole implementation.
var (
    _ Value = (*PortObject)(nil)
    _ Port  = (*PortObject)(nil)
)
```

All methods are nil-safe on the receiver (D4):

```go
func (p *PortObject) IsVoid() bool {
    return p == nil
}

func (p *PortObject) IsClosed() bool {
    if p == nil {
        return true
    }
    return p.portBase.IsClosed()
}

// SchemeString preserves the existing format from portBase verbatim:
//   <kind 0xADDR>           (note: NO "#" prefix; verify against
//                            values/port_base.go SchemeString())
// Nil receiver returns "<port nil>" (same bracket shape, no datum).
func (p *PortObject) SchemeString() string {
    if p == nil {
        return "<port nil>"
    }
    return p.portBase.SchemeString()
}

func (p *PortObject) EqualTo(v Value) bool {
    if p == nil {
        return v == nil
    }
    return p.portBase.EqualTo(v)
}

// PortKind returns the Scheme-visible kind tag (e.g.,
// "binary-input-port"). Returns "" for a nil receiver — there is no
// "unknown" port kind in the codebase; the empty string is the
// nil-safe sentinel.
func (p *PortObject) PortKind() string {
    if p == nil {
        return ""
    }
    return p.portBase.kind
}

func (p *PortObject) Close() error {
    if p == nil {
        return nil
    }
    if p.flsh != nil {
        return flushThenClose(p.flsh, &p.portBase)
    }
    return p.portBase.Close()
}
```

The 11 capability accessors all follow the same shape — explicit
multi-line bodies, nil-safe:

```go
func (p *PortObject) AsReader() (io.Reader, bool) {
    if p == nil {
        return nil, false
    }
    return p.rdr, p.rdr != nil
}

func (p *PortObject) AsWriter() (io.Writer, bool) {
    if p == nil {
        return nil, false
    }
    return p.wrt, p.wrt != nil
}

// AsByteReader, AsByteWriter, AsRuneReader, AsRuneWriter,
// AsByteUnreader, AsRuneUnreader, AsStringWriter, AsFlusher,
// AsByteVectorExtractor — same shape; one per slot.
```

`StringContent` is the symmetric accessor to `AsByteVectorExtractor`:

```go
// StringContent returns the accumulated string for string-output
// ports. Returns ("", false) if the port is not string-extractable.
func (p *PortObject) StringContent() (string, bool) {
    if p == nil || p.sext == nil {
        return "", false
    }
    return p.sext.String(), true
}
```

`Validate` enforces the **cross-slot** invariants I1–I7. The
**kind-vs-slot-set** invariant (I8 in the design) is enforced by
construction — every `New*Port` factory writes both `kind` and the
slot set in the same struct literal, so a `*PortObject` with mismatched
kind/slots cannot be produced through the public API. Validate does not
re-check I8 because re-deriving the expected slot set from `kind` would
duplicate the constructor table; a regression test asserts the pairing
at the constructor level (see §2.10 acceptance criteria).

```go
// Validate checks the capability-slot invariants I1–I7. Every
// constructor calls Validate and panics with werr.WrapForeignErrorf on
// failure; embedders constructing PortObject literally may call this
// themselves. I8 (kind matches capability profile) is enforced by
// construction, not by Validate — see the constructor table in the
// design doc.
func (p *PortObject) Validate() error {
    if p == nil {
        return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
            "PortObject.Validate: nil receiver")
    }
    // I1, I2: rb/rr require rdr
    if p.rb != nil && p.rdr == nil {
        return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
            "PortObject.Validate: rb without rdr")
    }
    if p.rr != nil && p.rdr == nil {
        return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
            "PortObject.Validate: rr without rdr")
    }
    // I3: urb/urr require rb/rr
    if p.urb != nil && p.rb == nil {
        return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
            "PortObject.Validate: urb without rb")
    }
    if p.urr != nil && p.rr == nil {
        return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
            "PortObject.Validate: urr without rr")
    }
    // I4: write capabilities require wrt
    if (p.wb != nil || p.wr != nil || p.ws != nil) && p.wrt == nil {
        return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
            "PortObject.Validate: write capability without wrt")
    }
    // I5, I6: extractors require wrt
    if p.ext != nil && p.wrt == nil {
        return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
            "PortObject.Validate: ext without wrt")
    }
    if p.sext != nil && p.wrt == nil {
        return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
            "PortObject.Validate: sext without wrt")
    }
    // I7: sext and ext are mutually exclusive
    if p.sext != nil && p.ext != nil {
        return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
            "PortObject.Validate: sext and ext both set")
    }
    return nil
}
```

**Build-order note**: `werr.ErrInvariantViolation` must be added to
`werr/werr.go` **as part of this Phase 1 commit** (Validate references
it). The Phase 3.1 sentinel work covers only `ErrNotAPort` (introduced
by Phase 2's two-step `get-output-string` extraction). This corrects an
earlier drafting error where both sentinels were grouped in Phase 3.1.

Add `values/port_test.go` with unit tests covering:

- Each accessor returns `(nil-or-zero, false)` on a zero `*PortObject`.
- Each accessor returns `(non-nil, true)` when its slot is populated.
- Each accessor returns `(nil-or-zero, false)` on a nil `*PortObject`
  (nil-safety contract from D4).
- `Close` is idempotent.
- `Close` invokes `flushThenClose` iff `flsh != nil` (D5 encoding).
- `Close` on nil receiver returns nil (no panic).
- `IsVoid` returns true for nil receiver, false otherwise.
- `SchemeString` enumerates all 9 kind strings: `binary-input-port`,
  `binary-output-port`, `character-input-port`, `character-output-port`,
  `string-input-port`, `string-output-port`,
  `bytevector-input-port`, `bytevector-output-port`,
  `bytevector-input-output-port`. (R-5 gate.)
- `SchemeString` on nil returns the documented sentinel form.
- `EqualTo` returns true iff same kind + same datum.
- `Validate` accepts every constructor output; rejects each
  cross-slot invariant violation (**7 negative cases for I1–I7**).
- I8 (kind matches capability profile) is asserted at the constructor
  level. **Phase 1 form** (this commit): one positive test per *intended
  kind* — 9 manually-constructed `*PortObject` literals (one per
  portKind constant) with their expected slot configs, each asserting
  `PortKind() == expectedKind` and `Validate() == nil`. The factories
  cannot be exercised here because they still return concrete types
  until Phase 2. **Phase 2 form** (§2.13 acceptance): same assertion
  re-applied to each of the 15 `New*Port` factories' actual outputs,
  proving each factory writes the kind/slot pair its row in the
  design table specifies. No negative I8 test in either phase — the
  factory is the only writer of `kind`.
- `PortKind` returns the correct tag for each constructor output;
  `PortKind` returns `""` for a nil receiver.

**Acceptance**:
- `go build ./values/...` clean.
- `go test ./values/...` passes (new test file passes, existing
  tests unaffected).
- `make lint` clean.

### 1.3 Commit

`refactor(values): introduce *PortObject + capability accessors (Phase 1
non-breaking)`.

The commit message body cites:
- 11 accessors + `StringContent` + `PortKind` + `Validate`.
- New `StringExtractor` interface in `values/`.
- Four exported helper interfaces (`RuneWriter`, `ByteUnreader`,
  `RuneUnreader`, `Flusher`) — promoted from unexported.
- New `werr.ErrInvariantViolation` sentinel (used by `Validate`).
- Parallel coexistence: `*PortObject` is dead code — nothing
  constructs or consumes it yet.

## Phase 2 — Atomic switch (breaking)

This is the load-bearing phase. It cannot be split into smaller
commits without leaving the build broken between commits, because
constructor signatures and call-site types change together. This
matches design D3.

**Strategy**: build the change in a single working-tree state; commit
once when everything compiles + tests pass. Drive change with TDD
order: rewrite tests first, fix production code to satisfy them.

### 2.1 Switch the constructors

Inventory (verify with `grep '^func New' values/*port*.go` before
editing):

- `binary_input_port.go`: `NewBinaryInputPort`,
  `NewBinaryInputPortFromReader` (2).
- `binary_output_port.go`: `NewBinaryOutputPortFromWriter` (1).
- `character_input_port.go`: `NewCharacterInputPort`,
  `NewCharacterInputPortFromReader` (2).
- `character_output_port.go`: `NewCharacterOutputPortFromWriter` (1).
- `string_input_port.go`: `NewStringInputPortWithBuffer` (1).
- `string_output_port.go`: `NewStringOutputPort`,
  `NewStringOutputPortWithBuffer` (2).
- `byte_vector_input_port.go`: `NewByteVectorInputPortFromReader` (1).
- `byte_vector_output_port.go`: `NewByteVectorOutputPortFromWriter`
  (1).
- `byte_vector_buffered_output_port.go`:
  `NewByteVectorBufferedOutputPort`,
  `NewByteVectorBufferedOutputPortFromBuffer` (2).
- `byte_vector_input_output_port.go`:
  `NewByteVectorInputOutputPort`,
  `NewByteVectorInputOutputPortFromBuffer` (2).

**15 `New*` functions across 10 logical types.** The design table
groups variants of the same type onto one row; the impl must edit
every variant. Variants of the same type produce the **same** kind
tag and slot set — the only difference is what backing they accept.

For each constructor in each file:

- Change each constructor's return type from `*<ConcreteType>` to
  `*PortObject`.
- Construct a `*PortObject` literal with the appropriate capability
  slots per the design's table. Set `flsh: bufioWriter` for the 4
  `bufio.Writer`-backed output ports (per D5: non-nil `flsh` means
  "flush on close"). Set `flsh: nil` for the 2 `*bytes.Buffer`-backed
  output ports. Set `sext: buffer` for the 2 string-output
  constructors. Set `ext: extractor` for the 3 bytevector-extractor
  constructors. Explicitly set `ws: nil` for the 3 binary output
  constructors (R7RS: `write-string` rejects binary ports — see design
  table note).
- Call `Validate()` at the end of each constructor; panic with
  `werr.WrapForeignErrorf` on failure (programmer-error path —
  invariant violation means the constructor is broken).
- Delete the concrete struct type definitions, their methods, their
  `var _ Iface = (*T)(nil)` compile-time assertions, and their
  `Datum()` methods. The file shrinks to just the constructor(s).

After this sub-step, the 10 port files contain only constructors. The
methods that used to live on each (`ReadByte`, `WriteByte`,
`ReadRune`, `WriteRune`, `UnreadByte`, `UnreadRune`, `Write`, `Read`,
`WriteString`, `Flush`, `Close`, `ReadByteVector`) are now reachable
only via `*PortObject` accessors.

(Optional cleanup: collapse all 10 constructor files into one
`values/port_constructors.go`. Worth doing — the per-type file
boundary stops being meaningful when each file is 5–15 lines.)

### 2.2 Delete the obsolete narrow interfaces

Edit `values/values.go`:

- Delete `InputPort`, `OutputPort`, `InputOutputPort`,
  `TextualReader`, `TextualWriter`, `BinaryReader`, `BinaryWriter`
  interface declarations.
- Keep `Port` (marker for "is any port") and `ByteVectorExtractor`
  (capability accessor return type). `Port` (interface) and
  `PortObject` (struct) are distinct names — no collision.

### 2.3 Rewrite the 6 ValueType.Check predicates

Edit `values/value_type.go`. Replace lines ~248–254:

```go
checks[TypePort] = makeCheck[Port]("port")
checks[TypeInputPort] = func(v Value) (bool, ValueType) {
    p, ok := v.(*PortObject)
    return ok && p.rdr != nil, TypeInputPort
}
checks[TypeOutputPort] = func(v Value) (bool, ValueType) {
    p, ok := v.(*PortObject)
    return ok && p.wrt != nil, TypeOutputPort
}
checks[TypeTextualInputPort] = func(v Value) (bool, ValueType) {
    p, ok := v.(*PortObject)
    return ok && p.rr != nil, TypeTextualInputPort
}
checks[TypeTextualOutputPort] = func(v Value) (bool, ValueType) {
    p, ok := v.(*PortObject)
    return ok && p.wr != nil, TypeTextualOutputPort
}
checks[TypeBinaryInputPort] = func(v Value) (bool, ValueType) {
    p, ok := v.(*PortObject)
    return ok && p.rb != nil, TypeBinaryInputPort
}
checks[TypeBinaryOutputPort] = func(v Value) (bool, ValueType) {
    p, ok := v.(*PortObject)
    return ok && p.wb != nil, TypeBinaryOutputPort
}
```

(The repeated closure shape may go through a helper
`makePortCapCheck(slot func(*PortObject) bool, t ValueType)` for
brevity.)

### 2.4 Update `goTypeToValueType` (if applicable)

If Finding 6 from the parent plan has shipped (PR #747) and
`goTypeToValueType` exists, replace the 9 concrete-port entries with
a single `(*PortObject, TypePort)` entry. The narrow ValueTypes
(`TypeInputPort` etc.) cannot be reverse-mapped from a Go type alone
since `*PortObject` covers all of them — leave them out of the
reverse map. Document the asymmetry inline (R-7).

### 2.5 Migrate `internal/extensions/io/state.go`

8 site-changes; all change `values.TextualReader` →
`*values.PortObject` and `values.OutputPort` → `*values.PortObject`.
To preserve textual-vs-binary discrimination at the parameter site
(the old `.(values.TextualReader)` assertion rejected binary ports
*here*), introduce two package-local helpers:

```go
func currentTextualInputPort(v values.Value) (*values.PortObject, error) {
    p, ok := v.(*values.PortObject)
    if !ok {
        return nil, werr.WrapForeignErrorf(werr.ErrNotAnInputPort,
            "current-input-port: value is %T, not a port", v)
    }
    if _, hasRR := p.AsRuneReader(); !hasRR {
        return nil, werr.WrapForeignErrorf(werr.ErrNotATextualPort,
            "current-input-port: port is not textual")
    }
    return p, nil
}

func currentTextualOutputPort(v values.Value) (*values.PortObject, error) {
    p, ok := v.(*values.PortObject)
    if !ok {
        return nil, werr.WrapForeignErrorf(werr.ErrNotAnOutputPort,
            "current-output-port: value is %T, not a port", v)
    }
    if _, hasRW := p.AsRuneWriter(); !hasRW {
        return nil, werr.WrapForeignErrorf(werr.ErrNotATextualPort,
            "current-output-port: port is not textual")
    }
    return p, nil
}
```

`GetCurrentInputPort`, `SetCurrentInputPort`,
`resolveCurrentInputPort` (and their output counterparts) call these
helpers and panic on error. The function names stay the same so
external callers within the io package see no rename churn:

```go
func GetCurrentInputPort() *values.PortObject {
    InitState()
    port, err := currentTextualInputPort(currentInputPortParam.Value())
    if err != nil {
        panic(err)
    }
    return port
}
```

Symmetric edits for `GetCurrentOutputPort`, `SetCurrentOutputPort`,
and `resolveCurrentOutputPort` (the output-side counterparts of the
three input-side functions above).

### 2.6 Migrate `internal/extensions/io/prim_read_write.go`

- Rewrite `extractPort[T any]` as non-generic:
  ```go
  func extractPort(
      o values.Value,
      name string,
      errSentinel *werr.StaticError,
  ) (*values.PortObject, values.Tuple, bool, error)
  ```
  Type assertion `tuple.Car().(T)` becomes
  `tuple.Car().(*values.PortObject)`.
- `getOptionalOutputPort` returns `*values.PortObject`. Rationale:
  the helper's only consumer (the primitive that called it) is the
  authoritative source on which capability is needed; returning the
  port object lets the call site call the exact accessor without the
  helper guessing. Same shape for `getOptionalInputPort`,
  `getRequiredBinaryInputPort`, `getRequiredBinaryOutputPort`.
- `getOptionalTextualOutputPort`'s binary-rejection becomes:
  ```go
  if _, isBinary := port.AsByteWriter(); isBinary {
      return nil, werr.WrapForeignErrorf(werr.ErrNotATextualPort,
          "expected a textual output port, got binary port")
  }
  ```
- Inside each primitive (`PrimReadChar`, `PrimPeekChar`,
  `PrimReadLine`, `PrimReadString`, `PrimRead`, `PrimReadToken`,
  `PrimReadSyntax`): after fetching the port via the helper, fetch
  the needed capability:
  ```go
  rr, ok := port.AsRuneReader()
  if !ok {
      return werr.WrapForeignErrorf(werr.ErrNotATextualPort,
          "<primName>: port is not a textual input port")
  }
  r, _, err := rr.ReadRune()
  ```
  Substitute the actual primitive name for `<primName>` in each site.
- Same pattern for `UnreadRune`: fetch via `AsRuneUnreader()`.

### 2.7 Migrate `internal/extensions/io/prim_write.go`

The single `extractPort[values.OutputPort]` call (~line 174) migrates
with `extractPort` itself. Capability fetch for the writer happens
at the call site via `port.AsWriter()`.

### 2.8 Migrate `internal/extensions/io/prim_ports.go`

12 sites (line numbers approximate — verify against current file):

- `PrimPortQ` (~line 27): `o.(values.Port)` stays — `Port` (the
  interface) survives D1, and `*PortObject` satisfies it.
- `PrimInputPortQ` (~line 31):
  ```go
  func(o values.Value) bool {
      p, ok := o.(*values.PortObject)
      if !ok {
          return false
      }
      _, hasReader := p.AsReader()
      return hasReader
  }
  ```
  The `!ok` short-circuit is required because `p` is nil when the
  assertion fails — calling `AsReader` on the nil result would still
  work (nil-safe per D4) but the explicit short-circuit reads better.
- `PrimOutputPortQ` (~line 35): symmetric — `AsWriter()`.
- `PrimInputPortOpenQ` (~line 44): `RequireArg[values.InputPort]` →
  `RequireArg[*values.PortObject]` (sentinel: `werr.ErrNotAPort`),
  plus a capability check that returns `werr.ErrNotAnInputPort` if
  `AsReader()` returns false. Same for `PrimOutputPortOpenQ`.
- `PrimClosePort` (~line 70): unchanged in spirit
  (`o.(values.Port)` still works via the marker interface).
- `PrimCloseInputPort` (~line 88):
  ```go
  p, ok := o.(*values.PortObject)
  if !ok {
      return werr.WrapForeignErrorf(werr.ErrNotAnInputPort,
          "close-input-port: not a port")
  }
  if _, hasReader := p.AsReader(); !hasReader {
      return werr.WrapForeignErrorf(werr.ErrNotAnInputPort,
          "close-input-port: not an input port")
  }
  // ... close logic
  ```
- `PrimCloseOutputPort` (~line 107): symmetric, with `AsWriter()`
  and `AsFlusher()` for the flush step:
  ```go
  if flsh, ok := p.AsFlusher(); ok {
      _ = flsh.Flush()
  }
  ```
- `PrimGetOutputString` (~line 158): **two-step** extraction:
  ```go
  p, err := helpers.RequireArg[*values.PortObject](mc, 0,
      werr.ErrNotAPort, "get-output-string")
  if err != nil {
      return err
  }
  s, ok := p.StringContent()
  if !ok {
      return werr.WrapForeignErrorf(werr.ErrNotAStringOutputPort,
          "get-output-string: port is not a string output port")
  }
  mc.SetValue(values.NewMutableString(s))
  return nil
  ```
  Two sentinels distinguish "not any port" (`ErrNotAPort`) from
  "wrong flavor" (`ErrNotAStringOutputPort`). `ErrNotAPort` is added
  in Phase 3.1.
- `PrimGetOutputBytevector` (~line 192):
  `RequireArg[values.ByteVectorExtractor]` stays — that interface
  survives D1.
- `PrimTextualPortQ` (~line 207):
  ```go
  p, ok := o.(*values.PortObject)
  if !ok {
      mc.SetValue(values.FalseValue)
      return nil
  }
  _, hasRR := p.AsRuneReader()
  _, hasRW := p.AsRuneWriter()
  mc.SetValue(values.BoolToBoolean(hasRR || hasRW))
  ```
- `PrimBinaryPortQ` (~line 217): symmetric for
  `AsByteReader/AsByteWriter`.

### 2.9 Migrate `internal/extensions/iotest/iotest.go`

Per the design doc § "Downstream call-site changes →
`iotest.go`". The wrapper embeds `*values.PortObject` and overrides
`AsRuneReader` and `AsRuneUnreader`. Internal helper types
`failingRuneReader` and `alwaysFailRuneUnreader` are added to host
the fault-injection logic.

**Acceptance (strict, per R-3)**: existing `integration/iotest_*.scm`
tests pass **without modification**. If they fail, the wrapper is
wrong, not the test. Test edits are forbidden under this gate.

### 2.10 Migrate test files

Discovery (run at the start of Phase 2):

```bash
grep -rln 'values\.\(Binary\|Character\|String\|ByteVector\)\(Input\|Output\|InputOutput\|BufferedOutput\)Port\b\|values\.\(Textual\|Binary\)\(Reader\|Writer\)\b\|values\.\(Input\|Output\|InputOutput\)Port\b' \
    --include='*_test.go' .
```

Treat the resulting file list as authoritative. The expected count is
~14 files across `values/`, `internal/extensions/io/`,
`internal/extensions/iotest/`, `registry/core/`, `werr/`; record the
actual count in the commit message body.

For each test file in the discovery output:

- Constructor calls (`values.NewCharacterInputPortFromReader(...)`) —
  no change needed; constructors return `*PortObject`.
- Type assertions (`_, ok := result.(*values.CharacterInputPort)`) —
  rewrite to `_, ok := result.(*values.PortObject)` plus the
  capability check that distinguishes the port flavor. Document the
  new check inline.
- Compile-time interface assertions (`var _ values.TextualReader =
  ...`) — delete; the interfaces are gone.

The `port_coverage_test.go` file (454 LOC) likely needs the heaviest
revision; its design was specifically to cover the 9 concrete types.
**Decision rule for keeping vs deleting**: keep a test iff its assertion
exercises a behavior not covered by the corresponding test in the new
`port_test.go` — i.e., name + assertion pattern is not subsumed. Tests
that re-prove "this concrete type satisfies this interface" are
subsumed (the interfaces are gone). Tests that exercise specific
fault-injection, edge cases, or kind-specific format strings survive.
Record the per-test decision (keep/delete) in a `git commit -m` body
table at §2.13 time so reviewers can audit.

`values/value_type_test.go` (currently ~227 lines; verify before
editing) has at least one port-related test referencing `TypePort`.
Verify the test still passes under capability-slot inspection; the
predicate semantics are preserved.

**Scope clarification vs R-3**: this section covers **Go** test files.
The R-3 strict-no-edit gate applies only to `integration/iotest_*.scm`
and the R7RS port conformance Scheme tests — those are
behavior-equivalence oracles for the iotest wrapper migration and must
not be modified. Go unit/integration tests in this list are expected
to change shape because the type assertions they perform reference
types that no longer exist.

### 2.11 Update `cmd/typeswitchlint/main.go`

Collapse lines 70–79 (10 entries) to 1 entry: `"*values.PortObject"`.

### 2.12 Update CLAUDE.local.md files

- `values/CLAUDE.local.md` — replace the "Port Types" table (10 rows)
  with a single row + capability table.
- `internal/extensions/io/CLAUDE.local.md` — replace the "Port Type
  Taxonomy" table with a capability description.

### 2.13 Commit

`refactor(values): unify port types into single *PortObject struct
(values SR Phase 2)`.

The commit message body cites:
- LOC delta (precise, measured at commit time).
- The 4 narrow interfaces deleted: `TextualReader`, `TextualWriter`,
  `BinaryReader`, `BinaryWriter`.
- The 3 medium interfaces deleted: `InputPort`, `OutputPort`,
  `InputOutputPort`.
- The 9 concrete types deleted: enumerated.
- 15 `New*Port` functions migrated to return `*PortObject` (per §2.1
  inventory).
- ValueType.Check rewrite (6 predicates: `TypeInputPort` through
  `TypeBinaryOutputPort`; `TypePort` unchanged).
- Downstream files touched: enumerated.
- Per-test keep/delete decisions for `port_coverage_test.go`
  (table, per §2.10 decision rule).

(Phase 1 already added the 11 accessors + `StringContent` + `PortKind`
+ `Validate` + `StringExtractor` interface + `ErrInvariantViolation`
sentinel; this commit consumes them.)

**Acceptance** (each must pass before this commit lands on the branch):
- `go build ./...` clean.
- `go test ./...` passes.
- Integration tests pass:
  - `integration/iotest_*.scm` (R-3, strict — no test edits).
  - `integration/r7rs-conformance/` port tests (textual + binary +
    bytevector families).
  - Other `integration/*` suites that exercise port primitives
    (enumerate at commit time via `ls integration/`).
- `make lint` clean.
- `make covercheck` passes.
- Gabriel benchmarks within 0.5% geomean of baseline across all 16
  canonical benchmarks; median of 3 runs to absorb noise:
  `for i in 1 2 3; do make bench-gabriel > /tmp/port-bench-after-$i.txt; done`
  then geomean-diff (R-1 gate).
- `SchemeString` enumerates all 9 kind strings correctly **with the
  `<kind 0xADDR>` shape (no `#` prefix; preserve `port_base.go`'s
  current format verbatim)** — R-5 gate.
- Constructor I8 contract: one positive test per `New*` factory
  asserts that the produced `*PortObject` has `PortKind()` matching
  the expected `portKind*` constant and that `Validate()` returns
  nil. 15 such assertions total (one per `New*` function in §2.1
  inventory).

If any acceptance fails, fix and amend before committing.

## Phase 3 — Post-migration cleanup (separate commit)

### 3.1 Sentinel audit (`werr.ErrNotAPort` add + existing sentinels)

`werr.ErrInvariantViolation` is added in Phase 1 (see §1.2 build-order
note). This step covers only the additional sentinel that Phase 2
required:

- Add `ErrNotAPort` (used by `PrimGetOutputString` two-step
  extraction and embedder-facing port-shape checks).

Audit the existing port-related sentinels. The seven names below must
all survive the migration (every name must appear at least once in
post-migration `internal/extensions/io/`, and `werr/werr_test.go`
expected-name test must still pass):

```
ErrPortClosed
ErrNotAPort                  (new — added this step)
ErrNotAnInputPort
ErrNotAnOutputPort
ErrNotABytevectorOutputPort
ErrNotAStringOutputPort
ErrNotATextualPort
ErrInvariantViolation        (added in Phase 1)
```

Use `grep -n 'werr\.Err' werr/werr.go` to locate the current
definitions — line numbers drift across commits per project policy
(`memory/MEMORY.md`: "Always locate functions by name or grep, not
hardcoded line numbers").

### 3.2 Update the values package "ADDING A NEW VALUE TYPE" guide

In `values/values.go`, item 5 currently mentions
`values/scheme_writer.go` for cycle-aware writers. Add a note about
`values/port.go` for any future port-shaped type: "If the type has
capability-conditional operations, expose them via
`AsXxxAccessor() (T, bool)` methods following the `*PortObject`
pattern. Document any new slot invariants in `Validate()`."

### 3.3 Update `plans/CLAUDE.md` plans index

Move `2026-05-14-port-unification-design.md` and
`2026-05-14-port-unification-impl.md` from active to **Completed
Plans → Structural Reduction** after the PR merges. Update the
status line on `2026-05-13-values-structural-reduction.md` to "Phases
0, 1, 2 shipped (PRs #747, #748, #XYZ)".

### 3.4 Commit

`docs(values): port unification cleanup — sentinel audit + ADDING
guide update`.

**Acceptance**: lint + tests pass; this commit is documentation +
sentinel additions only.

## Phase 4 — PR + review cycle

### 4.1 Push and open PR

`git push -u origin feat/values-sr-phase2-port-unification`

PR title: `refactor(values): unify port types — Phase 2 of values SR`

PR body cites:
- Parent plan + completed phases (0, 1 with PR numbers).
- LOC and file delta (measured).
- The design decisions D1–D6 with rationale (capability accessors,
  one-PR break, atomic Phase-2, nil-safety, `flsh != nil` encoding,
  `Validate()` invariant enforcement).
- Test plan: `make ci`, R7RS conformance, iotest (strict, no edits),
  bench-gabriel diff, SchemeString format enumeration.

### 4.2 Request reviews

- Copilot: `gh pr edit <N> --add-reviewer copilot-pull-request-reviewer`.
- Crosscheck: `/crosscheck:crosscheck all`.

### 4.3 Aggregate findings

Per `plans/CLAUDE.md` Implementation Completion Workflow § 5–7:
- Bucket findings into Critical / Notable-unambiguous /
  Notable-ambiguous / Clean.
- Fix Critical and Notable-unambiguous in a single
  `fix(values): address Copilot + crosscheck findings on PR #N`
  commit.
- Group Notable-ambiguous as Q-a/Q-b/... questions for user
  resolution.

### 4.4 Hand off

Report final status to user (PR URL, test count delta, lint/CI
state). Wait for merge instruction.

## Risk register

| ID  | Risk                                                                                                                | Detection                                                                  | Mitigation                                                                                 |
|-----|---------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------|--------------------------------------------------------------------------------------------|
| R-1 | Hot-path regression — slot-read + nil-check vs. itab lookup.                                                        | `bench-gabriel` (all 16 canonical benchmarks) diff vs. baseline.           | Gate: ≤ 0.5% geomean regression across all 16 benchmarks; median-of-3 runs to absorb noise (per `memory/feedback-no-taskpolicy-for-benches.md`). If breached, profile to find the hot path and inline. |
| R-2 | R7RS port-semantics regression — a primitive returns wrong type or accepts wrong port flavor.                       | R7RS conformance suite.                                                    | Each migrated primitive's pre-test must still pass post-migration. No exceptions.          |
| R-3 | `iotest` fault-injection semantics drift — accessor-override behavior differs from method-override.                 | `integration/iotest_*.scm` and Go tests.                                   | Tests pass without modification. Forbidden to edit tests; if they fail, fix the wrapper.   |
| R-4 | Tokenizer/Parser cache key drift — map keyed by `values.Value` interface; `*PortObject` pointer identity must hold across interface boxing.  | `prim_read_write_test.go` parser cache tests + a new positive assertion in `port_test.go`: box the same `*PortObject` into `values.Value` twice and verify map-key equality. | Verification, not prediction: Go interface comparison is `(type, value-pointer)` equality (Go spec §"Comparison operators"). Add the boxing assertion to make the property explicit in the test surface.            |
| R-5 | `SchemeString` output format drift — embedders may parse the current `<binary-input-port 0xADDR>` shape (NO `#` prefix; see `port_base.go` `SchemeString`).                              | Format-string enumeration test in `port_test.go` covering all 9 kinds, asserting `<{kind} 0x{hex}>` pattern. | Preserve `portBase.kind` tag and SchemeString format verbatim — nil receiver returns `<port nil>`. |
| R-6 | Hidden external embedder — type-asserting on `*values.<ConcreteType>` from outside this repo.                       | Zero known consumers; v1.x license.                                        | Accept the break per D2.                                                                   |
| R-7 | `goTypeToValueType` reverse-map asymmetry — narrow ValueTypes can't be reverse-mapped from `*PortObject`.           | New comment + sibling test verifying the asymmetry is intentional.         | Document the asymmetry in the ADDING guide.                                                |
| R-8 | `Validate()` overhead at construction — every port constructor calls it.                                            | Microbenchmark of constructor allocation in `port_test.go`.                | Validate is straight-line nil-checks (~30 ns); negligible. Skip via build tag if needed.   |

## Estimated effort

- Reading + design verification: 1 hour (done).
- Phase 1: 2 hours (struct + 12 accessors + Validate + tests +
  `ErrInvariantViolation` sentinel addition).
- Phase 2: 6–8 hours. Most of the effort is in the 15 constructor
  bodies, the 27-site downstream migration, the iotest wrapper rewrite,
  and the ~14 test-file migration.
- Phase 3: 1 hour (sentinel audit, ADDING-guide updates, plans index).
- Phase 4: 2 hours for PR + Copilot + crosscheck + fix cycle.

Total: ~12 hours focused work (range: 10–14h depending on test-file
surprises). Aligns with the design's "1–2 focused-day" estimate.

## Cross-references

- Design: `plans/2026-05-14-port-unification-design.md`.
- Parent SR plan: `plans/2026-05-13-values-structural-reduction.md`.
- Tier A.1 of SR roadmap: `plans/2026-05-07-structural-reduction-roadmap.md`.
- Phase 0 precedent (PR #747), Phase 1 precedent (PR #748).
- Workflow: `plans/CLAUDE.md` § Implementation Completion Workflow.

## Deferred follow-ups

Surfaced during the PR #749 review cycle (Copilot + 5-lens
crosscheck), classified as notable-ambiguous and deliberately
deferred to keep the unification PR focused on structural change.
Each is a real opportunity, not a bug; tracking here so they don't
get lost.

1. **Extractor API symmetry** — `StringContent() (string, bool)`
   returns the resolved string; `AsByteVectorExtractor() (BVE,
   bool)` returns the interface. Converge to one shape — either both
   resolved (`ByteVectorContent() (*ByteVector, bool)`) or both
   interface (`AsStringExtractor() (StringExtractor, bool)`). Caller
   churn: `PrimGetOutputString` and `PrimGetOutputBytevector`. Note
   currently inline at `values/port.go` on `StringContent`.

2. **PrimWriteString textual-port validation** — `write-string` is
   R7RS textual but `PrimWriteString` (`prim_write.go`) uses
   `extractPort` + bare `AsWriter`, accepting binary output ports.
   All six sibling textual-write primitives use
   `getOptionalTextualOutputPort` which rejects binary. The mismatch
   is pre-existing (the old `extractPort[OutputPort]` had the same
   gap), but the refactor was the right opportunity to close it.
   Fix: add a `getOptionalTextualOutputPortWithRest` helper or a
   post-extract `AsByteWriter` check.

3. **Constructor naming `*WithBuffer` vs `*FromBuffer`** —
   `NewStringInputPortWithBuffer` and `NewStringOutputPortWithBuffer`
   use the `WithBuffer` suffix; the symmetric bytevector factories
   use `FromBuffer` (`NewByteVectorBufferedOutputPortFromBuffer`,
   `NewByteVectorInputOutputPortFromBuffer`). Both are pre-existing;
   unification preserved each verbatim per design D2. Normalizing to
   one form is breaking-API churn that should ride a separate PR
   labeled as such.

4. **`getOptionalTextualOutputPort` symmetry with input side** —
   `getOptionalInputPort` returns `(*PortObject, io.RuneReader,
   error)` and pre-extracts the rune-reader capability;
   `getOptionalTextualOutputPort` returns only `(*PortObject,
   error)` and each caller re-extracts via `AsWriter`/`AsRuneWriter`/
   `AsStringWriter`. The asymmetry is principled (the input side has
   exactly one capability to extract; the output side has 3+
   depending on caller) but inconsistent. Either drop the input
   pre-extraction (let `prim_read_write.go` callers do `AsRuneReader`
   themselves) or extend output to pre-extract the textual writer.

5. **I8 enforcement in `Validate`** — currently I8 (kind matches
   capability profile) is "factory contract, not in Validate." A
   9-entry `portKindSlots` map in `Validate` would self-enforce
   future drift. Tradeoff: ~30 LOC + maintenance burden when adding
   a new port kind. Reasonable to defer until a 10th port flavor is
   actually proposed.

6. **`PortObject` literal-construction guard** — the struct is
   exported but slots are unexported; `&PortObject{}` literals pass
   `Validate` vacuously (`kind == ""`, all slots nil). Either
   unexport `PortObject` (callers use `Port` interface) or have
   `Validate` reject `kind == ""`. The first option is a much
   bigger API surface change; the second is a one-line addition.
   Worth filing as a follow-up after the migration settles.

7. **`port_behavior_test.go` table-driven migration** — per
   `registry/CLAUDE.md` "Test Structure: Table-Driven Tests Are
   Mandatory," the new file should consolidate its 15 per-factory
   kind-assertion functions into one table loop. The current shape
   matches the deleted `*_port_test.go` style (also non-table) so
   no convention is broken by *this* PR, but the test file is a
   future candidate for table-driven cleanup.
