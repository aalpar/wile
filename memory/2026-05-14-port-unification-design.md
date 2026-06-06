# Port unification — Phase 2 of `values/` structural reduction

**Date**: 2026-05-14
**Source**: Opportunity 2 of `memory/2026-05-13-values-structural-reduction.md`.
Phase 0 (PR #747 — quick wins) and Phase 1 (PR #748 — mutex state) have
shipped; this is the next phase per the parent plan's recommended phasing.
**Status**: **Implemented in PR #749.** Design (revised 2026-05-14 after
crosscheck disambiguation; re-revised 2026-05-14 after critique pass; updated
2026-05-14 post-implementation with the four mid-flight design refinements
documented in §"Mid-flight design refinements" below).
**Priority**: High. Largest LOC reduction available in `values/` (~900 LOC
across 10 files collapses to ~1 + tests).

## Goal

Replace 9 concrete port types
(`BinaryInputPort`, `BinaryOutputPort`, `CharacterInputPort`,
`CharacterOutputPort`, `StringInputPort`, `StringOutputPort`,
`ByteVectorInputPort`, `ByteVectorOutputPort`,
`ByteVectorBufferedOutputPort`, `ByteVectorInputOutputPort`)
with a single `*PortObject` struct whose capabilities are runtime-decided
**data**, not Go-type-encoded. The Scheme-level type taxonomy (binary
vs textual, input vs output, open vs closed) is preserved via the
existing `portKind*` tag plus capability slots; what changes is that
the *Go* layer stops re-encoding the same taxonomy as 9 near-identical
struct definitions.

(10 constructor files collapse into one set of factory functions. The
codebase currently has 15 `New*` functions across 10 logical port
types — several types provide both a "primary" constructor and a
`*FromReader` / `*WithBuffer` variant. Per-type variants all produce
the same `kind` tag and slot set; only the backing they accept
differs. 9 distinct Scheme-visible port types remain —
`ByteVectorInputOutputPort` combines input + output in one file but
it is still one type.)

## Design decisions (locked-in, post-disambiguation 2026-05-14)

These choices were settled during the design pass and the subsequent
crosscheck disambiguation. They shape every subsequent step.

### D1 — Capability API is **explicit accessors**, not universal methods

`*PortObject` exposes only the always-present surface directly:

```go
// Direct methods on *PortObject (all nil-safe on the receiver):
//   SchemeString() string                  // via portBase, nil-guarded
//   IsVoid() bool                          // returns true if p == nil
//   EqualTo(Value) bool                    // nil-guarded
//   IsClosed() bool                        // nil-guarded
//   Close() error                          // nil-guarded
//   PortKind() string                      // returns the kind tag; "" for nil receiver
//   Validate() error                       // checks the slot invariants
```

Every capability-conditional operation is reached via a typed accessor
returning `(T, bool)`:

```go
func (p *PortObject) AsReader() (io.Reader, bool)
func (p *PortObject) AsWriter() (io.Writer, bool)
func (p *PortObject) AsByteReader() (io.ByteReader, bool)
func (p *PortObject) AsByteWriter() (io.ByteWriter, bool)
func (p *PortObject) AsRuneReader() (io.RuneReader, bool)
func (p *PortObject) AsRuneWriter() (RuneWriter, bool)
func (p *PortObject) AsByteUnreader() (ByteUnreader, bool)
func (p *PortObject) AsRuneUnreader() (RuneUnreader, bool)
func (p *PortObject) AsStringWriter() (io.StringWriter, bool)
func (p *PortObject) AsFlusher() (Flusher, bool)
func (p *PortObject) AsByteVectorExtractor() (ByteVectorExtractor, bool)
func (p *PortObject) StringContent() (string, bool)   // R7RS get-output-string backing
```

The four unexported helpers in `port_helpers.go` (`runeWriter`,
`byteUnreader`, `runeUnreader`, `flusher`) become **exported**
(`RuneWriter`, `ByteUnreader`, `RuneUnreader`, `Flusher`) because they
appear in accessor return types. They are exclusively narrow interface
declarations; promoting them is a one-line rename per declaration.

`StringContent() (string, bool)` is the symmetric accessor to
`ByteVectorExtractor.ReadByteVector()` — it serves the
`get-output-string` primitive, which needs to retrieve the accumulated
string from a string-output-backed `*PortObject`. Without it,
`get-output-string` has no clean way to distinguish "string output"
from "any other writable port".

A new `StringExtractor` interface is added to `values/`:

```go
// StringExtractor is implemented by buffers that can yield their
// accumulated bytes as a string. *bytes.Buffer satisfies it.
type StringExtractor interface {
    String() string
}
```

This makes the string-output slot symmetric with `ByteVectorExtractor`
(both are one-method interfaces hosted in `values/`).

The narrow port interfaces — `InputPort`, `OutputPort`,
`InputOutputPort`, `BinaryReader`, `BinaryWriter`, `TextualReader`,
`TextualWriter` — are **deleted** from the public surface. Callers
that previously used interface assertions (`v.(BinaryReader)`)
migrate to accessor calls.

`Port` (the marker interface with `Close` + `IsClosed`) **stays as the
interface name**. Reason: `Port` is the answer to "is this *any*
port?" — semantically distinct from "is this an integer?" — and only
`*PortObject` satisfies it. `(*PortObject).Close` and
`(*PortObject).IsClosed` provide the implementation; the marker
interface gives type-test sites (`port?`, `close-port`) a stable
name to assert against. The narrower interfaces collapse to slot
inspection on `*PortObject`; the broad `Port` collapses to interface
satisfaction on a single implementer.

The struct is named `PortObject` (not `Port`) to avoid a name
collision with the surviving `Port` interface. This was the chief
crosscheck finding: Go forbids `type Port interface` and `type Port
struct` in the same package.

`ByteVectorExtractor` (the existing 1-method interface) **stays** —
it's referenced as the return type of `AsByteVectorExtractor()`.
Promoting it from a magic interface to a typed accessor return value
gives it a defined home.

**Why this trade-off**: the goal is *type precision*. The status quo has
9 concrete types × the 5-interface hierarchy expressing a 1024-cell
capability space with 10 actual cases. The accessor model collapses
this to one type plus 12 well-defined slots (5 read-side, 5 write-side,
2 extractor) — capability presence becomes a single bool from the
accessor, not a Go-type-level inference. 11 of those slots have an
`AsXxx() (T, bool)` accessor; the 12th (`sext`, the string extractor)
is reached via the symmetrically-named `StringContent() (string, bool)`
helper rather than an accessor that exposes the buffer directly.

**What is sacrificed**: static type evidence at the function-signature
layer. A primitive that "needs a binary output port" no longer says
that in its parameter type; it accepts `*PortObject` and runtime-checks
`AsByteWriter()`. The runtime check has been the actual enforcement
all along (the `TypeBinaryOutputPort.Check` predicate runs at every
foreign-call boundary); the Go signature was duplicating it.

### D2 — Migration is **one PR, one breaking change**

Aliases (`type BinaryInputPort = PortObject`) are *not* introduced. The 9
concrete types are deleted in the same PR that introduces `*PortObject`.
All downstream files (`internal/extensions/io/prim_ports.go`,
`internal/extensions/io/state.go`,
`internal/extensions/iotest/iotest.go`,
`cmd/typeswitchlint/main.go`) are updated in the same PR. v1.x with
zero external consumers permits the break.

**Why this trade-off**: aliases would compile-pass the migration but
mask exactly the capability semantics the unification is meant to
expose. An import that says `*values.BinaryInputPort` reads as
"definitely a binary input port" — but post-aliasing it's just a typed
view of `*values.PortObject` that could have been constructed as
anything. Aliases turn a clean break into a slow tar-pit. One PR is
honest.

### D3 — Phase 2 ships as a single mega-commit

The Phase-2 transformation (constructor signature changes, concrete-type
deletion, ValueType.Check rewrite, downstream call-site migration)
cannot be split into smaller independently-buildable commits without
leaving the build broken between them — constructor return types and
caller type-assertions change together. The earlier 8-commit-sequence
plan is replaced by a 4-commit PR: plan files / Phase 1 (non-breaking
introduce) / Phase 2 (atomic switch) / Phase 3 (docs cleanup).

### D4 — All Value-interface methods are nil-safe on `*PortObject`

`Close`, `SchemeString`, `EqualTo`, `IsClosed`, `Validate`, and every
accessor must nil-guard the receiver. `IsVoid` already returns true
for nil; the other methods must not panic. Generic dispatch sites
(e.g., `mc.SetValue(nil)`, value-table iteration) cannot reasonably
exclude `*PortObject` from receiving nil, so the type must tolerate
it. Methods that would otherwise dereference a slot first check
`p == nil` and return the zero-value/no-op.

### D5 — `flushOnClose` flag is dropped; `flsh != nil` means flush

The earlier draft used both a `flushOnClose bool` and a "no-op
flusher" interface implementation for ports that don't actually
flush. Two encodings of the same information. New encoding: `flsh ==
nil` means "no flush needed; Close should not call flushThenClose";
`flsh != nil` means "flush before close." String-output and
bytevector-buffered-output ports get `flsh: nil`. The 4
`bufio.Writer`-backed output ports get `flsh: bufioWriter`.

### D6 — Invariants are enforced by a public `Validate() error`

The 12 capability slots (5 read-side + 5 write-side + 2 extractor) ×
2-way nil = 4096 combinations; only ~10 are valid (per the constructor
table). A public `Validate() error` checks the **cross-slot** invariants
I1–I7 below. Every constructor calls `Validate()` and panics with a
wrap-error on failure. Embedders building `*PortObject` literally
(rare; not the intended API) can call `Validate()` themselves.

I8 (kind-vs-slots) is enforced **by construction**, not by Validate:
every `New*Port` factory writes both `kind` and the slot set in the
same struct literal, so a mismatched `*PortObject` cannot be produced
through the public API. Re-deriving the expected slot set from `kind`
inside `Validate()` would duplicate the constructor table. A
per-factory positive test (one per `New*` function) asserts the
kind/slot pairing.

Invariants:
- I1: `rb != nil` requires `rdr != nil` (byte-read implies readable).
- I2: `rr != nil` requires `rdr != nil` (rune-read implies readable).
- I3: bidirectional pairing — `rb` requires `urb` and vice versa;
  `rr` requires `urr` and vice versa. **Tightened post-PR-#749
  review** from the original one-direction `urb requires rb / urr
  requires rr` form. Every factory in `port_constructors.go`
  already pairs the slots together, so the tightening turns a
  construction convention into a checked invariant. Discarded `_`
  bools at `peek-char` / `peek-u8` call sites become safer once
  the runtime asserts the pairing.
- I4: `wb`, `wr`, `ws` non-nil require `wrt != nil`.
- I5: `ext != nil` requires `wrt != nil`.
- I6: `sext != nil` requires `wrt != nil`.
- I7: `sext` and `ext` are mutually exclusive (at most one is non-nil).
- I8 (factory contract, not in Validate): `kind` matches the
  capability profile — e.g., `portKindBinaryInput` ⇒ exactly
  `{rdr, rb, urb}` set, none of the write/textual/extractor slots.
  Enforced by the `New*Port` factories; asserted by per-factory tests.

## Mid-flight design refinements

Four design decisions emerged during implementation. They were not
visible from the design pass but became necessary once the code was
written. Each is listed with what triggered it and why the chosen
shape is preferable to the original sketch.

### M1 — Slot-level guarding wrappers (closed-port semantic preservation)

**Trigger**: Once the constructors were written and the existing
`binary_port_test.go` was running through the new accessors, the
`TestBinaryInputPort_Close` test broke: `port.Close()` followed by
`AsByteReader().ReadByte()` returned a real byte instead of
`werr.ErrPortClosed`. The original design left "what happens on a
closed port" implicit — accessors return the raw underlying io
interface, which has no notion of "the port that wraps me was
closed."

**Decision**: Each capability slot stores a small guarding wrapper
struct (`guardedReader`, `guardedByteReader`, `guardedRuneReader`,
`guardedByteUnreader`, `guardedRuneUnreader`, `guardedWriter`,
`guardedByteWriter`, `guardedRuneWriter`, `guardedStringWriter`,
`guardedFlusher`) constructed once at port creation. Each wrapper
holds a `*portBase` and the raw underlying interface; its single
delegated method calls the existing `guardedX` helper which checks
`portBase.closed` first and returns `ErrPortClosed`.

**Why this shape over alternatives**:

- **Per-call wrap at accessor time** would allocate per access (the
  wrapper is interface-boxed). Per-port wrap allocates 10× once at
  construction; accessor returns are zero-allocation.
- **Direct methods on `*PortObject`** (e.g., `(*PortObject).ReadByte`
  with internal guard) was rejected: it contradicts D1 (capability
  via accessors, not via methods on the universal type) and bloats
  the `*PortObject` method set with 10+ direct methods that
  duplicate the slot information.
- **Returning `(T, false)` on closed-port** would conflate "no
  capability" with "closed". The R7RS contract is that operations on
  closed ports *error* with a specific sentinel, not silently degrade
  to "not capable."

**Cost**: ~10 wrapper types in `port_helpers.go` (~150 LOC mechanical
boilerplate), 10 small allocations per port construction. The
wrapper pattern is itself a structural-reduction candidate (called
out in the consistency-lens review as a hand-unrolled loop), but
collapsing it requires either Go method-set generics (not
available) or a different per-method dispatch shape that gives up
the zero-overhead accessor.

### M2 — Iotest factory in `values/` instead of accessor-override embedding

**Trigger**: The original design (D1 + §"Downstream call-site
changes → iotest.go") said the iotest wrapper would compose by
embedding `*PortObject` and overriding the `AsRuneReader` /
`AsRuneUnreader` accessors. Implementation revealed this doesn't
work: production code asserts `v.(*values.PortObject)` directly
(e.g., `extractPort` after the `extractPort[T]` generic was
collapsed). A type-assertion to `*PortObject` on a
`*FailingTextualInputPort` *fails* — Go's type assertion checks the
dynamic type identity, not whether the embedded type matches. So
the wrapper would be rejected before the override could fire.

**Decision**: Add `values.NewStringInputPortWithReaders(rdr, rr,
urr) *PortObject` — a values-package factory that constructs a
real `*PortObject` whose `rr` and `urr` slots are externally
supplied. iotest builds its `countingRuneReader` and
`alwaysFailRuneUnreader`, feeds them to the factory, and gets back
a real `*PortObject` that production code's type assertions accept.
The iotest "wrapper type" disappears entirely.

**Why this shape over alternatives**:

- **Add a `PortAccessor` interface** with all 11 accessor methods
  + change production code to dispatch via this interface instead
  of `*PortObject`: this re-introduces narrow interfaces and
  defeats half of D1's reason-to-exist.
- **Change `extractPort` to use the marker `Port` interface** +
  reach the accessors through the type assertion at the use site:
  same problem; either you assert `*PortObject` and lose
  override-via-embedding, or you build an accessor interface.

The slot-level injection model is structurally consistent with the
overall design — slots-as-data, factories-as-construction.
`NewStringInputPortWithReaders` doesn't violate the abstraction; it
extends the factory family with one that takes pre-built capability
slots instead of buffer-derivation. The factory is named for its
*kind tag* (`portKindStringInput`), not its name in the original
plan ("`NewTextualInputPortWithReaders`"); this keeps name and kind
in sync and avoids the "a textual input port that's actually a
string input port" naming drift.

### M3 — `(*PortObject).Close` idempotent at PortObject level

**Trigger**: The slot-level `flsh` wrapper (M1) checks
`portBase.closed` before flushing. With the design as drafted,
calling `Close()` twice on a `*bufio.Writer`-backed port would:
first call → flush succeeds (port is open) → close marks closed;
second call → flush fails with `ErrPortClosed`. That's a regression
— `Close` is idempotent in the existing concrete types because
`portBase.Close` short-circuits on `closed=true` *before* the flush
helpers run.

**Decision**: `(*PortObject).Close` checks `p.portBase.IsClosed()`
at the top and returns nil without re-entering `flushThenClose`. The
flush wrapper's guard remains useful for direct
`AsFlusher().Flush()` calls on a closed port (which should error).

**Why**: The flush helper and the close helper have different
intended audiences. `flushThenClose` is called from `Close` itself
and assumes the port is open. `AsFlusher().Flush()` is called from
external code (`flush-output-port`) and should reject closed ports.
Putting the idempotency at the `*PortObject.Close` level
distinguishes the two without giving up the wrapper's guard.

### M4 — `PrimGetOutputBytevector` two-step extraction

**Trigger**: The plan said "RequireArg[ByteVectorExtractor] stays —
that interface survives D1." But after migration, `*PortObject`
*does not* satisfy `ByteVectorExtractor` directly — only the slot
wrapper (`bufferedBVExtractor` or `inputOutputBVExtractor`) does.
`RequireArg[ByteVectorExtractor]` rejected every `*PortObject`
input with "expected a bytevector output port but got
\*values.PortObject."

**Decision**: Make `PrimGetOutputBytevector` use the same two-step
extraction shape as `PrimGetOutputString`: first
`RequireArg[*PortObject]` with `ErrNotAPort`, then
`AsByteVectorExtractor()` with `ErrNotABytevectorOutputPort`. Two
sentinels distinguish "not a port" from "wrong port flavor."

**Why this is more consistent**: The String / ByteVector extractor
primitives are R7RS siblings (`get-output-string` and
`get-output-bytevector`); their primitives should have the same
extraction shape. The original plan's asymmetry (one two-step, one
direct interface assertion) was an artifact of which interface
happened to survive D1, not a deliberate design choice. Both being
two-step makes the relationship explicit.

## `*PortObject` shape

```go
package values

// StringExtractor accepts a buffer that can yield its accumulated
// bytes as a string. *bytes.Buffer satisfies it.
type StringExtractor interface {
    String() string
}

type PortObject struct {
    portBase                       // closed, clsr, kind, datum

    // Read side.
    rdr io.Reader                  // present iff readable
    rb  io.ByteReader              // present iff byte-readable
    rr  io.RuneReader              // present iff rune-readable
    urb ByteUnreader               // present iff byte-unreadable
    urr RuneUnreader               // present iff rune-unreadable

    // Write side.
    wrt io.Writer                  // present iff writable
    wb  io.ByteWriter              // present iff byte-writable
    wr  RuneWriter                 // present iff rune-writable
    ws  io.StringWriter            // present iff string-writable
    flsh Flusher                   // present iff a real flush is needed at close

    // Extractors (output ports backed by an in-memory buffer).
    ext  ByteVectorExtractor       // present iff bytevector-extractable
    sext StringExtractor           // present iff string-extractable
}
```

Capability slots are typed by the *narrowest* stdlib interface that
captures the operation (or a wile-local narrow interface where stdlib
has none). Assignment is done once at construction; reads are
nil-checks. The 10 existing constructors collapse to 10 still-named
factory functions, but each is a single struct literal that fills in
the capability slots appropriate to its backing:

| Constructor                                      | Backing         | Slots set                                          |
|--------------------------------------------------|-----------------|----------------------------------------------------|
| `NewBinaryInputPort(*bufio.Reader)`              | `*bufio.Reader` | `rdr, rb, urb`                                     |
| `NewBinaryOutputPortFromWriter(io.Writer)`       | `*bufio.Writer` | `wrt, wb, flsh` (ws: **nil** — binary ⇒ no string) |
| `NewCharacterInputPort(*bufio.Reader)`           | `*bufio.Reader` | `rdr, rr, urr`                                     |
| `NewCharacterOutputPortFromWriter(io.Writer)`    | `*bufio.Writer` | `wrt, wr, ws, flsh`                                |
| `NewStringInputPortWithBuffer(*bytes.Buffer)`    | `*bytes.Buffer` | `rdr, rr, urr`                                     |
| `NewStringOutputPort()` / `*WithBuffer`          | `*bytes.Buffer` | `wrt, wr, ws, sext` (flsh: **nil**)                |
| `NewByteVectorInputPortFromReader(io.Reader)`    | `*bufio.Reader` | `rdr, rb, urb`                                     |
| `NewByteVectorOutputPortFromWriter(io.Writer)`   | `*bufio.Writer` | `wrt, wb, flsh` (ws: **nil** — binary)             |
| `NewByteVectorBufferedOutputPort()`              | `*bytes.Buffer` | `wrt, wb, ext` (flsh: **nil**)                     |
| `NewByteVectorInputOutputPort()` / `*FromBuffer` | `*bytes.Buffer` | `rdr, rb, urb, wrt, wb, ext` (flsh: **nil**)       |

Notes on the table:
- `ws: nil` for binary output ports is **intentional**: `*bufio.Writer`
  implements `io.StringWriter`, but R7RS forbids `write-string` on
  binary ports. Explicit nil keeps `AsStringWriter()` honest.
- `flsh: nil` for `*bytes.Buffer`-backed ports is the new encoding
  per D5: no real flush needed, so `Close` skips `flushThenClose`.
- `sext` (StringExtractor) is set only on string-output ports;
  `ext` (ByteVectorExtractor) only on bytevector-extractor ports.
  Mutual exclusion enforced by `Validate()` (I7).

Constructor names are preserved to minimize call-site churn (the only
embedder-visible names that were both ergonomic and accurate).

## ValueType.Check predicates

The 6 port-related `ValueType` checks switch from `makeCheck[Iface]` to
capability-slot inspection (built at init time as bespoke
`checkFunc`s):

| ValueType                  | New check                                                            |
|----------------------------|----------------------------------------------------------------------|
| `TypePort`                 | `_, ok := v.(*PortObject); return ok`                                |
| `TypeInputPort`            | `p, ok := v.(*PortObject); return ok && p.rdr != nil`                |
| `TypeOutputPort`           | `p, ok := v.(*PortObject); return ok && p.wrt != nil`                |
| `TypeTextualInputPort`     | `p, ok := v.(*PortObject); return ok && p.rr  != nil`                |
| `TypeTextualOutputPort`    | `p, ok := v.(*PortObject); return ok && p.wr  != nil`                |
| `TypeBinaryInputPort`      | `p, ok := v.(*PortObject); return ok && p.rb  != nil`                |
| `TypeBinaryOutputPort`     | `p, ok := v.(*PortObject); return ok && p.wb  != nil`                |

This is the **single source of truth** for "what kind of port is
this?" — replacing the prior duplication between Go interface
satisfaction (compile-time) and `portKind*` tag (runtime).

## Downstream call-site changes

Verification surfaced a wider call-site surface than the original
sketch acknowledged. The complete list, post-verification:

### `internal/extensions/io/prim_ports.go` (12 sites)

- 5 sites assert against `values.Port`, `values.InputPort`,
  `values.OutputPort` (~lines 28, 32, 36, 72, 90, 109, 240) →
  rewritten as `o.(*values.PortObject)` plus per-capability checks
  (`AsReader() != nil` for input, `AsWriter() != nil` for output).
  `values.Port` (the interface) survives D1, so the `o.(values.Port)`
  sites that test "is this *any* port?" still work — `*PortObject`
  satisfies the marker.
- 2 sites use `helpers.RequireArg[values.InputPort/OutputPort]`
  (~lines 45, 58) → become `RequireArg[*values.PortObject]` plus
  capability check.
- 4 sites use `o.(values.TextualReader)`, `values.TextualWriter`,
  `values.BinaryReader`, `values.BinaryWriter` (~lines 209, 210, 219,
  220 — implementations of `textual-port?` and `binary-port?`) →
  become `AsRuneReader() || AsRuneWriter()` and
  `AsByteReader() || AsByteWriter()`.
- 1 site uses `helpers.RequireArg[values.ByteVectorExtractor]`
  (~line 193 — `get-output-bytevector`) → keep using
  `ByteVectorExtractor` interface; it survives D1.
- 1 site uses `helpers.RequireArg[*values.StringOutputPort]`
  (~line 158 — `get-output-string`) → **two-step**: first
  `RequireArg[*values.PortObject]` with sentinel `werr.ErrNotAPort`,
  then `p.StringContent()` with sentinel `werr.ErrNotAStringOutputPort`
  on failure. The `werr.ErrNotAPort` sentinel is new — added in
  Phase 3.1.

(Line numbers are approximate — verify against current file before
editing.)

### `internal/extensions/io/prim_read_write.go` (6 sites)

- `extractPort[T any]` (line 48) is generic over interface type. It
  parameterizes 4 callers (lines 86, 117, 132, 148). Rewrite as
  non-generic `extractPort(o, name, sentinel) (*PortObject, Tuple,
  bool, error)`, with per-caller capability checks at the call site.
- `getOptionalOutputPort` / `getOptionalInputPort` /
  `getRequiredBinaryInputPort` / `getRequiredBinaryOutputPort` return
  `*values.PortObject`. Callers invoke the appropriate accessor at
  the call site — explicit capability check per primitive.
- `getOptionalTextualOutputPort` (line 100) tests
  `p.(values.BinaryWriter)` to reject binary writers. Becomes
  `_, isBinary := p.AsByteWriter()`.

### `internal/extensions/io/prim_write.go` (1 site)

Line 174 uses `extractPort[values.OutputPort]` — migrates with
`extractPort` itself.

### `internal/extensions/io/state.go` (8 sites)

The current-input-port / current-output-port / current-error-port
parameter machinery. Functions `GetCurrentInputPort`,
`SetCurrentInputPort`, `resolveCurrentInputPort` and their output
counterparts type the port as `values.TextualReader` /
`values.OutputPort`. All migrate to `*values.PortObject`.

To preserve textual-vs-binary discrimination at the parameter site
(the prior `.(values.TextualReader)` assertion rejected binary ports
*here*; the new `.(*values.PortObject)` would only fail "not a port
at all"), introduce two package-local helpers:

```go
// currentTextualInputPort asserts *PortObject and verifies the port
// has a RuneReader capability. Returns sentinel-wrapped error
// otherwise.
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

// currentTextualOutputPort is symmetric.
```

`GetCurrentInputPort`, `SetCurrentInputPort`,
`resolveCurrentInputPort` (and their output counterparts) call these
helpers. This centralizes the capability check and preserves the
old gate semantics.

### `internal/extensions/iotest/iotest.go` (1 line + structure)

> **POST-IMPLEMENTATION NOTE**: this section is the *original* sketch.
> The accessor-override-via-embedding approach didn't survive
> implementation — production code asserts `*values.PortObject`
> directly, and Go's type assertion does not dispatch through
> embedded types. See **§Mid-flight design refinements → M2** for
> the shipped design (slot-level injection via the values-package
> factory `NewStringInputPortWithReaders`). The original sketch is
> preserved below for design-history context.

Compile-time assertion `var _ values.TextualReader =
(*FailingTextualInputPort)(nil)` is dropped (interface gone). The
wrapper pattern flips from method-override (current) to
accessor-override:

```go
type FailingTextualInputPort struct {
    *values.PortObject
    failUnread      bool
    failReadAfter   int
    successfulReads int
}

func (p *FailingTextualInputPort) AsRuneReader() (io.RuneReader, bool) {
    inner, ok := p.PortObject.AsRuneReader()
    if !ok {
        return nil, false
    }
    return &failingRuneReader{inner: inner, parent: p}, true
}

func (p *FailingTextualInputPort) AsRuneUnreader() (values.RuneUnreader, bool) {
    if p.failUnread {
        return alwaysFailRuneUnreader{}, true
    }
    return p.PortObject.AsRuneUnreader()
}
```

A separate concern: the wrapper now embeds `*values.PortObject`, not
`*values.StringInputPort`. When `iotest.Builder` constructs the
wrapper, it calls `NewStringInputPortWithBuffer` (which now returns
`*values.PortObject`) and embeds the result. The accessor-override
pattern preserves all non-fault-injected behavior automatically.

The wrapper composes by intercepting the *accessor*, not the *method*.
This is structurally equivalent and arguably cleaner — the fault
injection lives at the same conceptual layer as the capability
declaration.

### Test files (~14 files)

167 test references to concrete port types in non-production files
across `values/`, `internal/extensions/io`, `internal/extensions/iotest`,
`registry/core/`, `werr/`. Most are constructor calls
(`values.NewCharacterOutputPortFromWriter(buf)`) which still work
post-migration since the constructor names are preserved. The ~20
type-assertion sites (e.g., `_, ok := result.(*values.CharacterInputPort)`)
need rewriting to `_, ok := result.(*values.PortObject)` + capability
check.

Integration suites under `integration/` that must pass without
modification: `integration/iotest_*.scm` (R-3 gate, see Risks) and
the R7RS port-conformance Scheme tests (the textual + binary +
bytevector port families exercised by the standard library).

### `cmd/typeswitchlint/main.go`

The 10-entry allow-list (lines 70–79) collapses to 1 entry:
`"*values.PortObject"`. The lint rule still flags type switches that
distinguish ports by Go type (now meaningless); embedders are
pointed at the `PortKind()` method (added as part of Phase 1.2) or
the capability accessors instead.

## What stays as-is

- `portBase` (`port_base.go`) — still embedded; `getPortBase()` /
  `EqualTo` / `SchemeString` are unchanged in shape (now nil-guarded
  via the `*PortObject` wrappers).
- `port_helpers.go` — the `guardedXxx` helpers and `flushThenClose`
  are unchanged. They're already parameterized over stdlib interfaces;
  `*PortObject` calls them via its capability slots.
- `Port` Scheme-level identity (`kind ⨯ datum`) — preserved verbatim,
  with `kind` now the single discriminator for "what kind of port is
  this?" Scheme-side. The Go-level marker interface name `Port`
  also stays.
- All R7RS-mandated semantics (binary ≠ textual, input ≠ output, EOF
  handling, close idempotence, flush-then-close on `bufio.Writer`
  backed ports).

## Migration in one PR (4 commits)

Branch: `feat/values-sr-phase2-port-unification`

1. **Commit 1 (`docs`)**: this design file + the impl plan file.
2. **Commit 2 (`refactor(values)`)**: Phase 1 — introduce `*PortObject`
   struct + 11 accessors + `StringContent` + `PortKind` + `Validate`;
   add `StringExtractor` interface; export the four helper
   interfaces (`RuneWriter`, `ByteUnreader`, `RuneUnreader`,
   `Flusher`). *Do not yet* delete the 9 concrete types — they
   coexist briefly. `*PortObject` is dead code at end of this commit
   (nothing constructs or consumes it yet).
3. **Commit 3 (`refactor(values+io+iotest+typeswitchlint)`)**: Phase 2 —
   atomic switch:
   - Switch all 15 `New*Port*` constructors (10 logical types; see
     impl §2.1 inventory) to return `*PortObject`.
   - Delete the 9 concrete struct types and their methods.
   - Delete the 7 narrow port interfaces (`InputPort`, `OutputPort`,
     `InputOutputPort`, `BinaryReader`, `BinaryWriter`,
     `TextualReader`, `TextualWriter`).
   - Rewrite the 6 ValueType.Check predicates to inspect capability
     slots.
   - Update `goTypeToValueType` (if shipped — collapse 9 entries to
     1 + document the asymmetry).
   - Migrate `prim_ports.go`, `prim_read_write.go`, `prim_write.go`,
     `state.go` (with new `currentTextualInputPort` helpers).
   - Rewrite `iotest.go` wrapper.
   - Collapse `cmd/typeswitchlint/main.go` allow-list to one entry.
   - Migrate test files.
4. **Commit 4 (`docs`)**: Phase 3 — werr sentinel audit (verify the
   7 existing port sentinels + add new `ErrNotAPort`); update the
   `values` "ADDING A NEW VALUE TYPE" guide; update CLAUDE.local.md
   files; update `plans/CLAUDE.md` index.

Phase 1 and Phase 3 are independently buildable. Phase 2 is one atomic
commit — splitting it would leave the build broken between commits
because constructor return types and call-site types change together.

The PR-final state runs `make ci` plus the R7RS-port conformance
sweep and the `integration/iotest_*.scm` regression suite.

## Risks and bench gates

### R-1 — Slot-read replaces interface-satisfaction switch

Hot paths in `io/prim_read_write.go` previously dispatched via
`v.(TextualReader)` (Go interface check); they now dispatch via
`p.AsRuneReader()` (slot read + nil check). Slot read is faster than
itab lookup, so this should be neutral or slightly faster. **Bench
gate**: run `bench-gabriel` before/after on the `boyer`/`peval`/
`tak`/`fib` set — none of these are port-heavy but they exercise the
hot path. **Acceptance**: ≤ 0.5% regression geomean.

### R-2 — Reflection-using callers in the embedder API

`registry/helpers/value_conv.go` and FFI dispatch don't currently
type-assert on concrete port types (verified by the grep that
returned only the 3 downstream files). Risk that a private embedder
*does* type-assert is residual — v1.x with zero consumers is the
license for this risk per `CLAUDE.md` versioning policy.

### R-3 — `iotest` semantic equivalence (strict)

The accessor-overriding pattern in iotest must produce identical
behavior to the method-overriding embedded pattern. **Verification**:
the existing `integration/iotest_*.scm` tests pass *without
modification*. **If they fail, the wrapper is wrong, not the test.**
Test edits are forbidden under this gate.

### R-4 — Tokenizer / parser cache key drift

`internal/extensions/io/state.go` maintains
`map[values.Value]*tokenizer.Tokenizer` and
`map[values.Value]*parser.Parser`. Map keys compare by Go interface
equality `(dtype, dval)`. Replacing 9 concrete types with
`*PortObject` changes the boxed type. Detection: existing parser
cache tests in `prim_read_write_test.go`. Mitigation: `*PortObject`
pointer identity is preserved through the interface box, so within
a single process the cache works correctly.

### R-5 — `SchemeString` format drift

The current external representation, from `values/port_base.go`
`SchemeString()`, is:

```go
fmt.Sprintf("<%s %p>", p.kind, p.datum)
```

i.e., **`<binary-input-port 0xADDR>`** — note the leading `<`, **not**
`#<`. (An earlier draft of this risk section claimed a `#<...>` form,
inherited from a confusion with Scheme datum-label syntax. Corrected
here.) Some embedders may match on this prefix shape; the migration
must preserve the format verbatim. **Verification**: format-string
equality test in `port_test.go` must enumerate all 9 kind strings
explicitly, asserting each renders as `<{kind} 0x{hex}>` (allowing
any hex address).

### R-6 — Hidden external embedder

Type-asserting on `*values.<ConcreteType>` from outside this repo
would break. Zero known consumers; v1.x license. Accept per D2.

### R-7 — `goTypeToValueType` reverse-map asymmetry

Narrow ValueTypes (`TypeInputPort` etc.) cannot be reverse-mapped
from `*PortObject` alone since `*PortObject` covers all of them. Document
the asymmetry in the ADDING guide; verify with a sibling test.

## Estimated size

- LOC delta: −600 to −800 (10 port files at ~80 LOC each → 1 file at
  ~250 LOC + constructors + Validate; minus the 7 narrow interface
  declarations + 6 `makeCheck[Iface]` lines; plus accessor methods
  and the new `StringExtractor`, `PortKind`, `Validate`).
- Diff: ~1500 lines changed.
- Time: ~10–14 focused hours (≈1–2 working days). See
  `2026-05-14-port-unification-impl.md` §"Estimated effort" for the
  per-phase breakdown.
- Reviewer load: medium-high (a few callers, many call sites within
  `values/` itself).

## Cross-references

- Parent plan: `memory/2026-05-13-values-structural-reduction.md`
  (Opportunity 2 + Recommended Phasing).
- Roadmap: `plans/2026-05-07-structural-reduction-roadmap.md`
  (Tier A.1).
- Phase 0: PR #747 (Findings 4, 5, 6 — quick wins).
- Phase 1: PR #748 (Finding 7 — mutex state).
- Precedent for one-PR breaking SR: PR #730 (environment SR — phases
  1–9 in one PR).
