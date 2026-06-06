# `values/` package structural reduction

**Date**: 2026-05-13
**Source**: `/structural-reduction ./values` analysis (Tier A.1 of the
roadmap — `plans/2026-05-07-structural-reduction-roadmap.md`)
**Status**: **Tier A.1 complete — Phases 0–4 shipped** (PRs #747–#752; closeout commit `1518ba1f`). vmCore extraction declined. This file is retained as the analysis of record.
**Priority**: **High** (Tier 5 tech debt; the highest-leverage remaining
SR target — 226 importing files across 32 packages, `Ca=32`).

## Why this scope

`values/` is the most-depended-on package in the codebase. The roadmap
ranks it Tier A.1 because every improvement compounds across 32
direct-importer packages and the entire VM/expander/compiler/extension
pipeline depends on its types and interfaces. The roadmap hypothesized
four findings:

1. Numeric tower tightness (12+ concrete types, 12-item ADDING guide)
2. Port hierarchy collapsibility (8+ port types)
3. Registration mechanism guides (Value, NumericKind)
4. Tuple/Pair migration completeness — confirm no defensive guards remain

This analysis confirms #1, #2, and partially #3, **declines** #4 (the
migration is complete — every remaining `*Pair` site is doing pointer-
identity work for cycle detection, the documented R7RS-mandated case).
It also surfaces three additional findings the roadmap did not predict.

## Scope analyzed

`values/` (61 production files, 11084 LOC; 57 test files):

```
Package layout (production files only):
  Type-bearing files (per Value type):   46
  Numeric infrastructure:                 3  (numeric_kind, numeric_tower, promotion)
  Port infrastructure:                    2  (port_base, port_helpers)
  Value-interface infrastructure:         3  (values, value_type, utils)
  Pair/list infrastructure:               3  (pair, empty_list, pair_block)
  Syntax surface:                         4  (syntax_base, syntax_tuple, syntax_value, syntax_vector)
```

External fan-in: **32 packages, 226 files** (`Ca=32`).
External fan-out: **1 package** — `werr/` (`Ce=1`).
Instability: **I = 1/33 ≈ 0.030** — extremely stable; near the floor.

By the Stable Dependencies Principle (Martin, *Clean Architecture* ch.14),
a package this stable must be very carefully designed because every
structural choice propagates. The instability metric tells us why this
audit matters disproportionately: the blast radius of changes here is
the entire codebase.

## Dependency map

```
                          ┌─────────────────┐
                          │      werr        │  Ca=33, I≈0.03
                          └────────┬────────┘
                                   │ sentinels + WrapForeignErrorf
                                   ▼
   ┌──────────────────────────────────────────────────────────────┐
   │                          values                                │
   │   Ca=32  Ce=1  I≈0.030  (61 files, 11084 LOC)                  │
   │                                                                 │
   │   Interfaces:                Concrete types:                    │
   │     Value, Callable           Integer, Float, Rational, Byte,   │
   │     Hashable, Tuple           BigInteger, BigFloat, BigComplex, │
   │     Number, ComplexNumber,    Complex, Boolean, Character,      │
   │     RealNumber                Symbol, String, Pair, Vector,     │
   │     Port, InputPort,          ByteVector, Hashtable, Record,    │
   │     OutputPort, TextualR/W,   RecordType, Box, Promise,         │
   │     BinaryR/W,                Thread, Mutex, RWMutex, Channel,  │
   │     SyntaxValue, SyntaxTuple  ConditionVariable, Once, Time,    │
   │     TypeConstraint            Process, NativeError, AtomicBox,  │
   │                                AtomicInt64, WaitGroup, CharSet, │
   │                                OpaqueValue, CompileTimeValue,   │
   │                                <9 port types>                   │
   └────────────────────┬───────────────────────────────────────────┘
                        │ Value, Number, Port, Tuple, ...
                        ▼
         internal/syntax, environment, machine, machine/compilation,
         registry, registry/core, registry/helpers, extensions/*,
         repl, cmd/wile, docparse, ... (32 packages)
```

The graph is a clean DAG with `values/` as a near-bottom node. No
violation of the Acyclic Dependencies Principle. No SDP violation —
nothing more stable than `values/` (only `werr/`) depends on anything
less stable.

## Findings

### Finding 1 — `IsVoid()` convention is enforced only by code review

**Status**: Recast after design-intent review (see "Revision history"
below). The original framing — "hand-unrolled loop, delete 51 method
bodies, replace with reflection" — was wrong. The 50 per-type methods
are the *correct* implementation of an intentional API: an
ergonomic, type-specific nil check for `Value` objects that
correctly handles the nil-pointer-in-interface case without forcing
callers to use reflection. Go cannot share method implementations
across distinct types (the Value implementers include non-struct
types like `Pair [2]Value`, `ByteVector []*Byte`, `emptyListType
struct{}`), so the uniform-body shape is forced by the language, not
by a missed abstraction. The free function at `values/utils.go:321`
*depends on* the per-type method to handle nil pointers correctly;
without per-type dispatch, the same behavior would require reflection
on every call.

**Principle**: Semantic documentation — make the contract enforceable
rather than convention-only.

**Where**: 50 files in `values/` (every `Value`-implementing type).
A mechanical count of the body shapes:

| Body              | Count | Where (representative)                  |
|-------------------|-------|-----------------------------------------|
| `return p == nil` | 47    | Every pointer-receiver Value type       |
| `return false`    | 2     | `emptyListType`, `eofType`              |
| `return true`     | 1     | `voidType`                              |

(`SourceIndexes` uses a value receiver and returns `false` — same
documented exception as the singletons.)

**Theory**: The contract "every pointer-receiver `Value` implementer's
`IsVoid` returns `true` for a nil receiver and `false` otherwise" is
**enforced by convention** (Liskov & Guttag's representation-invariant
lens: an invariant that holds across all observable states but is
*not* expressed in the type system). The exceptions (`voidType` →
always true; `emptyListType`/`eofType`/`SourceIndexes` → always false)
are documented but also convention-only.

**Current state**: The convention has held throughout the package's
lifetime. Nothing in the build or test suite would catch a future
author who writes a wrong body, e.g.:

```go
// Imaginary bad impl that the type system permits.
func (p *NewType) IsVoid() bool {
    return p == nil || p.Value == 0   // wrong! 0 is a real value
}
```

A bug like this would silently miscategorize zero-valued instances as
void throughout the codebase — exactly the kind of latent failure the
two-layer error / typing discipline is meant to prevent.

**Problem**: An implicit invariant is one drift-prone author away from
becoming a latent bug. Per the `values/values.go` `Value`-interface
guide, adding a new type *requires* implementing `IsVoid` (item 1),
but the *body* of `IsVoid` is documented only in prose. The cost of
codifying the convention as a test is small; the payoff is catching
exactly the kind of drift the convention is meant to prevent.

**Proposed direction**: Add a reflection-based convention test that
enumerates all `Value` implementers and asserts the documented
behavior, with an explicit allow-list for the three documented
exceptions:

```go
// values/value_isvoid_convention_test.go
func TestIsVoidConventionForAllValueTypes(t *testing.T) {
    // Exceptions — must match the documented set exactly.
    exceptions := map[string]bool{
        "voidType":      true,  // singleton: always void
        "emptyListType": false, // singleton: never void
        "eofType":       false, // singleton: never void
        "SourceIndexes": false, // value receiver: nil not applicable
    }
    for _, exemplar := range allValueExemplars {
        rt := reflect.TypeOf(exemplar)
        typeName := rt.Elem().Name()
        // Construct a typed nil pointer.
        nilPtr := reflect.New(rt.Elem()).Elem().Interface().(Value)
        got := nilPtr.IsVoid()
        want, isException := exceptions[typeName]
        if !isException {
            want = true // default convention: nil receiver → void
        }
        if got != want {
            t.Errorf("(*%s)(nil).IsVoid() = %v, want %v", typeName, got, want)
        }
    }
    // Roster completeness: every Value type appears in allValueExemplars.
    // Enforced by a separate roster-walk test against the package's
    // declared types (via reflection over package types — out of scope
    // for the convention test itself but recommended as a sibling test).
}
```

The test depends on a canonical roster `allValueExemplars` — one nil
pointer per concrete `Value` type. The roster doubles as documentation
of the closed set of Value types: a single grep-able location instead
of a 50-file scan. Adding a new Value type means adding one exemplar;
forgetting to do so is caught by a roster-completeness sibling test
that walks `reflect`-discovered types in the package.

**Trade-offs**:
- **Pro**: Zero production code change. No interface modification.
  No performance impact. Convention becomes a checked invariant.
- **Pro**: Roster `allValueExemplars` provides the closed-world enumeration
  the package currently lacks. Useful for any future cross-type audit
  (e.g., Finding 6's `goTypeToValueType` reverse map could read from
  the same roster).
- **Con**: Maintenance cost — the roster must be updated when adding a
  new Value type. Mitigated by the completeness sibling test, which
  fails loudly if the roster is incomplete.
- **Neutral**: Doesn't reduce LOC; the goal is *correctness enforcement*,
  not duplication elimination.

**Impact**: Small — converts a convention into a checked invariant.
Optional; the convention has held without it.

**Estimated size**: XS (~50 LOC of test code; no production changes).

**Revision history**:
- 2026-05-13 v1: Original finding proposed deleting 51 method bodies
  and replacing with reflection in a free function. Retracted after
  design-intent review: the methods exist to provide an ergonomic
  type-specific nil check that doesn't require callers to use
  reflection. Go's inability to share method implementations across
  unrelated types makes the uniform body shape forced, not
  accidental. The original framing applied Strachey's substitution
  principle to methods as if they were functions — incorrect: the
  dispatch through the interface is the entire point of the API.
- 2026-05-13 v2 (current): Recast as a convention-test finding.
  Production code untouched; the goal is to make the existing
  invariant enforceable.

---

### Finding 2 — Port hierarchy: 9 concrete types differ only in capability mix

**Principle**: Composability / Type precision
**Where**: `values/binary_input_port.go`, `binary_output_port.go`,
`character_input_port.go`, `character_output_port.go`,
`string_input_port.go`, `string_output_port.go`,
`byte_vector_input_port.go`, `byte_vector_output_port.go`,
`byte_vector_buffered_output_port.go`,
`byte_vector_input_output_port.go`
**Theory**: This is **factoring out the common structure** in the
algebraic sense. Nine port types share an identical skeleton:

```
type XPort struct {
    portBase
    buf <BufferType>
}

func NewXPort(...) *XPort {
    q := &XPort{buf: ...}
    q.kind = portKindX
    q.datum = q.buf
    return q
}

func (p *XPort) <RWop>(...) (...) {
    return guarded<RWop>(&p.portBase, p.buf, ...)
}
```

The variations are exactly:

| Type                              | BufferType         | Capabilities                                |
|-----------------------------------|--------------------|---------------------------------------------|
| `BinaryInputPort`                 | `*bufio.Reader`    | R + RB + URB + Close                        |
| `BinaryOutputPort`                | `*bufio.Writer`    | W + WB + Flush + Close + flushOnClose       |
| `CharacterInputPort`              | `*bufio.Reader`    | R + RR + URR + Close                        |
| `CharacterOutputPort`             | `*bufio.Writer`    | W + WR + WS + Flush + Close + flushOnClose  |
| `StringInputPort`                 | `*bytes.Buffer`    | R + RR + URR + Flush(no-op) + String        |
| `StringOutputPort`                | `*bytes.Buffer`    | W + WR + WS + Flush(no-op) + String         |
| `ByteVectorInputPort`             | `*bufio.Reader`    | R + RB + URB + Close                        |
| `ByteVectorOutputPort`            | `*bufio.Writer`    | W + WB + Flush + Close + flushOnClose       |
| `ByteVectorBufferedOutputPort`    | `*bytes.Buffer`    | W + WB + Flush(no-op) + Extractor           |
| `ByteVectorInputOutputPort`       | `*bytes.Buffer`    | R + W + RB + WB + URB + Flush + Extractor   |

Where R/W = Read/Write, RB/WB = ReadByte/WriteByte, RR/WR =
ReadRune/WriteRune, URB/URR = UnreadByte/UnreadRune, WS = WriteString.

The product space is 2^10 ≈ 1024 representable combinations of
capability subsets; only 10 are actually used. **Type precision ≈
10/1024 ≈ 1%.** By type algebra, this is a sum-of-products where the
sum has 10 cases out of a 1024-case universe — and the cases are not
mutually exclusive (most ports share several capabilities).

Two further hints in the existing code:
1. `port_base.go:36` — `portKindBytevectorOutput` is shared between
   `ByteVectorOutputPort` and `ByteVectorBufferedOutputPort`. The
   comment reads: *"Two concrete types share portKindBytevectorOutput
   because they represent the same Scheme type."* This is the codebase
   already conceding that the Scheme-level identity is `kind ⨯ datum`,
   not the Go type.
2. `port_helpers.go` already defines `guarded<RW>op(b *portBase, r
   io.Reader, ...)` helpers that are parameterized over the narrowest
   stdlib interface (`io.Reader`, `io.ByteReader`, `io.RuneReader`, …).
   The dispatch machinery is already capability-shaped; only the
   *types* are not.

**Current state**: ~900 LOC spread across 10 files, each a near-copy of
the same skeleton with slight variation.

**Problem**: Adding an 11th port type (say, a network socket port) means
copying the skeleton again, picking a kind string, choosing which
`guardedXxx` helpers to expose. The capability matrix is a runtime
property of the underlying I/O object, not a static property requiring
a new Go type. This is the **Parnas (1972) inversion**: the decomposition
is by *mechanism* (which Go interface the buffer implements) rather
than by *design decision* (what capability set does this Scheme port
expose). The mechanisms differ; the design decisions are identical.

**Proposed direction**: A single `Port` struct parameterized by an
optional capability slot for each protocol:

```go
type Port struct {
    portBase                       // closed, kind, datum, EqualTo, SchemeString
    rdr  io.Reader                 // nil if no read capability
    wrt  io.Writer                 // nil if no write capability
    rb   io.ByteReader             // nil if no byte-read; assert from rdr at construction
    wb   io.ByteWriter             // …
    rr   io.RuneReader
    wr   runeWriter
    urb  byteUnreader
    urr  runeUnreader
    flsh flusher
    ext  byteVectorExtractor       // nil unless this is an extractor variant
    flushOnClose bool
}
```

Type predicates (`InputPort`, `OutputPort`, `BinaryReader`, …) become
runtime checks against the relevant slot:

```go
func (p *Port) AsBinaryReader() (BinaryReader, bool) {
    return p, p.rb != nil
}
```

Or alternatively, keep the existing interfaces and have `*Port`
implement only the *checked* method names — meaning `ReadByte` panics
if `rb` is nil, or returns `ErrUnsupported`. The cleaner option is the
explicit `As<Capability>()` accessor.

**Reuse sites**: The Scheme-level type check funnel
(`TypeBinaryInputPort.Check`, `TypeTextualOutputPort.Check`, …) collapses
from 7 interface-asserting checks to 1 struct-with-capability-check
lookup.

**Trade-offs**:
- **Pro**: 10 files (~900 LOC) collapse to 1 + tests. Adding a new port
  is a constructor call, not a type definition. The capability matrix
  becomes data, not code.
- **Pro**: `ValueType.Check` for the 6 port-type constraints simplifies
  to capability-slot inspection — one closure replaces six.
- **Con**: `*Port` becomes a wide struct (~12 fields). Embedders that
  type-assert on concrete port types (`*BinaryInputPort`) break — but
  searching the public API surface (`wile/`, `repl/`, `extensions/*`)
  shows the only assertions are on the *interfaces*, not the concrete
  types. The `Datum()` method on each concrete type returns a type-
  specific value — those callers do break and need to switch to the
  capability accessor.
- **Con**: Major surface change. Likely a 2-week project. Bench-gated.

**Impact**: Highest-leverage finding in this analysis. Eliminates the
largest hand-unrolled loop in `values/`. Reduces port-related cognitive
load to a single file.

**Estimated size**: L (multi-week, multi-PR; benchmark-gated).

---

### Finding 3 — Numeric tower 12-place "ADDING A NEW NUMERIC TYPE" guide

**Principle**: Dependency / Parnas information hiding
**Where**: `values/numeric_kind.go:5-22` (the guide comment); call sites
across `values/promotion.go`, `values/numeric_tower.go`,
`registry/helpers/value_conv.go`, `extensions/math/prim_conversion.go`,
`extensions/math/prim_complex.go`, `ffi.go`,
`internal/parser/parser_number.go`, `registry/helpers/equality.go`, and
two test files.
**Theory**: When the guide says "edit these 12 places", that *is* the
missing abstraction. Parnas (1972) frames it precisely: a module should
hide a design decision likely to change. "What concrete numeric types
does Wile support?" is exactly such a decision (R7RS implementations
commonly add `fixnum`, `flonum`, `quaternion`, decimal-floats, etc.).
The fact that 12 unrelated files must be coordinated to make a coherent
change means the design decision is *not* hidden — it leaks across 12
package boundaries.

**Current state**: 7 `NumericKind` constants in `values/numeric_kind.go`.
For each new numeric type, the 12-item update list reads:

```
 1. values/numeric_kind.go              — add KindXxx constant
 2. values/xxx.go                       — new type file, dispatch tables, init
 3. values/promotion.go                 — promotionTable, promoter rows,
                                          NumberToFloat64, NumberToComplex128
 4. values/numeric_tower.go             — Simplify, ExactnessOf
 5. values/numeric_dispatch_test.go     — TestAllDispatchEntriesPopulated
 6. registry/helpers/value_conv.go      — ToComplex128, ToFloat64
 7. extensions/math/prim_conversion.go  — exact->inexact, number->string
 8. extensions/math/prim_complex.go     — make-rectangular, make-polar
 9. wile-goast/goast/mapper.go          — EXTERNAL REPO, numberToAST
10. ffi.go                              — schemeToReflectValue
11. internal/parser/parser_number.go    — if parseable from source
12. registry/helpers/equality.go        — Eqv if special semantics
```

Items 1–5 are local to `values/` and are already well-organized — the
dispatch-table generators (`makeArithmeticDispatch`, `makeLessThan
Dispatch`, etc.) centralize the actual algorithm; the per-type init()
functions just wire up the same-type fast path. The `init()` cost is
covered by `TestAllDispatchEntriesPopulated`.

The leakage is items **6–12**: a numeric type's identity, name, parse
syntax, FFI mapping, and equality behavior live in *six* other packages
including one external repo (`wile-goast`).

**Problem**: There is no central record of "what is a numeric type in
Wile". The information is distributed across:

- `NumericKind` (a flat enum, no associated data)
- `promotionTable[K]` (which kind to promote toward)
- `promoter[K]` (how to actually convert)
- `Simplify` switch (when to demote)
- `ExactnessOf` switch (exact vs. inexact membership)
- `TypeInteger.Check` / `TypeFlonum.Check` (Go-type unions)
- `SchemeTypeName` switch (Go type → Scheme name)
- `parser_number.go` (literal syntax recognition)
- `value_conv.go` (Go→Scheme conversion)
- FFI's `schemeToReflectValue` (Scheme→Go reflection)

Every site re-encodes the same set of seven facts. If a new type is
added and any one of the twelve sites is missed, the result is a silent
runtime failure mode — usually "type assertion failed" or a missing
dispatch entry — that escapes the type system.

**Proposed direction**: A `NumericTypeSpec` record, keyed by
`NumericKind`:

```go
// values/numeric_registry.go (new file)
type NumericTypeSpec struct {
    Kind         NumericKind
    SchemeName   string                  // "integer", "flonum", ...
    Exact        bool                    // for ExactnessOf
    IsInteger    bool                    // R7RS §6.2.6 integer?
    IsRational   bool                    // R7RS §6.2.6 rational?
    SimplifyTo   func(Number) Number     // descent in the tower
    ToFloat64    func(Number) float64    // promotion to float64 (where defined)
    ToComplex128 func(Number) complex128 // promotion to complex128 (where defined)
    Parse        func(string) (Number, error) // nil if not parseable
    FromGoValue  func(reflect.Value) Number   // nil if not FFI-receivable
    ToGoValue    func(Number) reflect.Value   // nil if not FFI-emittable
}

var numericTypeRegistry [numKinds]NumericTypeSpec
```

Each type file's `init()` registers a single spec. The 12 leakage sites
collapse to one registry lookup. External consumers (`registry/helpers`,
`extensions/math`, `ffi.go`) read the registry instead of duplicating
the case analysis.

**Trade-offs**:
- **Pro**: Adding a new numeric type drops from 12 sites to 2 (the type
  file itself, plus the registry entry — which lives *with* the type
  file). The 12-item guide collapses to "implement Number; register
  spec; done."
- **Pro**: Cross-package consistency becomes a type-system property:
  every consumer reads from the same registry, so additions are
  automatically reflected everywhere.
- **Pro**: External repos (`wile-goast`) can iterate the registry
  instead of hardcoding a switch — fewer EXTERNAL REPO update items.
- **Con**: Introduces an indirection on hot paths. `ToFloat64` and
  `ToComplex128` are called inside arithmetic dispatch closures — the
  current direct switches are fast. The registry lookup is `[numKinds]`
  indexed (cache-friendly), but the function-pointer call may
  prevent inlining. Bench-gate.
- **Con**: Some FFI/parsing operations don't fit the per-type-spec
  shape cleanly. May need a hybrid where dispatch helpers stay where
  they are and the registry is only consulted in non-hot paths.

**Impact**: Largest leakage finding. Most disruptive to fix but pays
back across the entire numeric subsystem and into two external repos.

**Estimated size**: L (multi-phase; the registry shape needs to be
designed before any migration).

---

### Finding 4 — `TypeExactInteger` is a literal alias of `TypeInteger`

**Principle**: State tightness / boolean blindness inversion
**Where**: `values/value_type.go:61`, with 18 call sites in
`registry/core/`, `extensions/`, `internal/extensions/all/`
**Theory**: Two names for one concept is a normalization violation
(database theory: redundant attributes create update anomalies; in code
they create consistency obligations the type system does not enforce).
The two names are not synonyms — they are *aliases*: the code

```go
checks[TypeExactInteger] = checks[TypeInteger]
```

binds them to the same function pointer at init time.

**Current state**:

| Constant            | `typeNames`        | `typeDescriptions`  | `checks`             |
|---------------------|--------------------|---------------------|----------------------|
| `TypeInteger`       | `"integer"`        | `"exact integer"`   | `*Integer\|*BigInteger` switch |
| `TypeExactInteger`  | `"exact-integer"`  | `"exact integer"`   | same as above        |

The display name differs by hyphen. The description is byte-identical.
The check is pointer-identical.

**Problem**: All Wile integers are exact. `TypeInteger` already means
"exact integer" (the `Float`/`BigFloat` types — inexact — go under
`TypeFlonum`). The `TypeExactInteger` alias was likely added to make
docstrings read naturally where R7RS specifies "exact integer" (e.g.,
`(make-string n)` where `n` must be an exact-integer index). But this
is **boolean blindness** in reverse: a tag that carries no information
beyond the existing `TypeInteger`. Two ValueType constants pointing at
the same check is a redundant attribute — adding a third would compound
the problem.

**Proposed direction**: Delete `TypeExactInteger`. Migrate the 18 call
sites to `TypeInteger`. If the user-facing display name matters
(docstrings rendering "exact-integer" instead of "integer"), provide a
formatter that maps `TypeInteger` → "exact-integer" in contexts where
R7RS-style spec-text is wanted.

**Trade-offs**:
- **Pro**: One fewer ValueType. The 27-entry enum shrinks to 26. The
  init() iteration is one slot lighter.
- **Pro**: Future authors can't accidentally pick the "wrong" one.
- **Con**: 18 call-site renames across `registry/core/`,
  `extensions/`, `internal/extensions/all/`. Mechanical but touches
  multiple packages. If docstrings depend on the "exact-integer" name,
  need to verify rendering doesn't regress.

**Impact**: Small but visible. Cleanest single-PR finding.

**Estimated size**: S.

---

### Finding 5 — `makeCheck[T]` and `makeInterfaceCheck[T]` are byte-identical

**Principle**: Composability / Substitution principle
**Where**: `values/value_type.go:312-322` and `:325-336`
**Theory**: Two functions that produce identical bytecode for every
input are one function. The current code admits this in a comment:

```go
// makeInterfaceCheck creates a checkFunc for an interface type T.
// The implementation is identical to makeCheck — both use Go type assertions —
// but keeping them separate documents the intent: concrete type vs interface.
```

The substitution principle (Strachey, 1967) is unambiguous: if `f` and
`g` produce identical results for identical inputs and have no observable
side effects, they are the same function. Documenting intent through
duplication is the wrong tool — comments are the right tool.

**Current state**: 11 lines × 2 functions = 22 lines that are 100%
identical except for the function name.

**Problem**: A reader who sees both `makeCheck` and `makeInterfaceCheck`
in the same file is led to believe they differ semantically. They don't.
The comment then *un*-tells them. This is a documentation anti-pattern:
the redundant code is the source of confusion the comment then resolves.

**Proposed direction**: Delete one. Keep `makeCheck[T]`. Where the
"interface" intent is load-bearing, use a `// interface check` comment
at the call site, or a wrapper:

```go
// init()
checks[TypeNumber]  = makeCheck[Number]("number")        // interface
checks[TypeReal]    = makeCheck[RealNumber]("real")      // interface
checks[TypeBoolean] = makeCheck[*Boolean]("boolean")     // concrete
```

**Trade-offs**:
- **Pro**: −11 LOC, −1 generic, +clarity.
- **Con**: Loses the at-a-glance "this is an interface" cue from the
  function name. Mitigated by per-call comments where it matters (only
  matters for the 6 interface checks, since 17 are concrete-type).

**Impact**: Trivial. Could ride along with Finding 4 in one PR.

**Estimated size**: XS.

---

### Finding 6 — `SchemeTypeName` duplicates the `ValueType` name registry

**Principle**: Dependency minimization / Equivalent representations
**Where**: `values/value_type.go:263-307` vs. `values/value_type.go:85-114`
(`typeNames`).
**Theory**: Two representations of the same mapping is a
**normalization** opportunity. Database theory: redundant maps create
update anomalies. The information-theoretic content of both tables is
identical:

```
typeNames        : ValueType → string
SchemeTypeName   : Go runtime type → string
```

Both encode "what's the Scheme-facing name of a value's type". They
differ only in the input domain. The bridge `Go runtime type → ValueType`
is the missing piece that would let one table serve both.

**Current state**: `SchemeTypeName(v)` is a 45-line switch:

```go
switch v.(type) {
case *Boolean:        return "boolean"
case *Integer, *BigInteger: return "integer"
case *Rational:       return "rational"
...
case *Pair:           return "pair"
case *Vector:         return "vector"
...
default:
    switch {
    case IsEmptyList(v): return "empty-list"
    case IsList(v):      return "list"
    default:             return fmt.Sprintf("%T", v)
    }
}
```

Meanwhile `typeNames[TypeInteger]` returns `"integer"`, etc.

**Problem**: When a new Value type is added, the 7-item ADDING guide
in `values/values.go` lists `scheme_writer.go` as a possible update site
but does **not** mention `SchemeTypeName`. The switch silently falls
through to `fmt.Sprintf("%T", v)` which produces things like
`*values.SomeNewType` — debugging-only output that leaks the Go type
name into Scheme error messages. The author has no signal that the
switch needs updating; the test suite passes; users see ugly errors
months later.

**Proposed direction**: Build a reverse map `goTypeToValueType` at init
time:

```go
var goTypeToValueType map[reflect.Type]ValueType

func init() {
    goTypeToValueType = make(map[reflect.Type]ValueType, TypeCount)
    register := func(t ValueType, exemplar Value) {
        goTypeToValueType[reflect.TypeOf(exemplar)] = t
    }
    register(TypeBoolean,   (*Boolean)(nil))
    register(TypeInteger,   (*Integer)(nil))
    register(TypeInteger,   (*BigInteger)(nil))      // both → integer
    register(TypeFlonum,    (*Float)(nil))
    register(TypeFlonum,    (*BigFloat)(nil))
    ...
}

func SchemeTypeName(v Value) string {
    if v == nil || IsVoid(v) {
        return "void"
    }
    if t, ok := goTypeToValueType[reflect.TypeOf(v)]; ok {
        return t.String()
    }
    // Interface cases (port, list, empty-list) handled via interface
    // assertions as today, but only as fallback.
    ...
}
```

Then `SchemeTypeName` and `typeNames` share their authoritative table.
Adding a new ValueType requires one registration; `SchemeTypeName` picks
it up automatically.

**Trade-offs**:
- **Pro**: Eliminates the silent fall-through. New types get correct
  names without touching `SchemeTypeName`.
- **Pro**: Establishes a `Go-type → ValueType` bridge that Finding 3's
  numeric registry can reuse.
- **Con**: Reflection on every call to `SchemeTypeName`. This function
  is used in error messages, not on hot paths, so the cost is acceptable.
  But verify before/after via `go test -bench`.

**Impact**: Eliminates a silent failure mode. Pairs naturally with
Finding 3's numeric registry — both want the Go-type → ValueType
reverse map.

**Estimated size**: S.

---

### Finding 7 — Mutex `state × owner` product space has implicit invariants

**Principle**: State tightness / representation invariants
**Where**: `values/mutex.go:42-49` (`MutexState` enum) and `:68-78`
(`Mutex` struct with `state` + `owner` fields)
**Theory**: Liskov & Guttag's **representation invariant** lens. The
`Mutex` struct has

```go
state MutexState  // 4 values
owner *Thread     // nil | *Thread
```

representable space: `4 × (1 + |threads|)`. The valid space is governed
by implicit invariants:

```
INV(m): state = MutexUnlocked       ⇒ owner = nil
INV(m): state = MutexLockedOwned    ⇒ owner ≠ nil
INV(m): state = MutexLockedNotOwned ⇒ owner = nil
INV(m): state = MutexAbandoned      ⇒ owner = nil
```

Three of four states force `owner = nil`; only `LockedOwned` carries a
thread. The type does not encode this — every site that reads `state`
must also know which states permit `owner != nil`.

A separate issue: `MutexState.String()` collapses `MutexUnlocked` and
`MutexLockedNotOwned` to the same string `"not-owned"` because R7RS
SRFI-18 specifies the Scheme-level `mutex-state` returns
`'not-owned` for both. The Go-side states are distinct (one is
acquirable without blocking, the other is held); the Scheme-side names
collide.

**Current state**: 4 states + 1 nullable owner; 8 representable
combinations; 4 valid. Type precision = 4/8 = 50%.

**Problem**: Adding a fifth state (e.g., "locked-recursive" for a
hypothetical recursive mutex variant) requires re-auditing every site
that reads `state` to confirm the new state's `owner` semantics. The
invariants are documented only in code comments, not in the type.

**Proposed direction**: A sum type for state:

```go
type mutexStateTag uint8
const (
    msUnlocked mutexStateTag = iota
    msLocked        // covers both owned and not-owned: owner field tells which
    msAbandoned
)

type Mutex struct {
    ...
    stateTag mutexStateTag
    owner    *Thread  // meaningful only when stateTag == msLocked
    ...
}
```

This collapses `LockedOwned`/`LockedNotOwned` into one `Locked` tag
where the owner field is the actual carrier of identity. Three states
instead of four; the `MutexState.String()` collision becomes a
function of `stateTag = msLocked ∧ owner = nil`, which is exactly what
SRFI-18's `'not-owned` symbol captures.

**Trade-offs**:
- **Pro**: −1 state, type precision 4/8 → 3/6 = 50% (same, but smaller
  absolute space — easier to audit). The invariant
  `msUnlocked ⇒ owner = nil` and `msAbandoned ⇒ owner = nil` remain
  implicit, but the *Locked* case now correctly says "owner is the
  identity" instead of leaving it implicit.
- **Pro**: `StateValue()` simplifies — three cases instead of four,
  with the `LockedNotOwned → SymbolMutexNotOwned` redundancy gone.
- **Con**: Touches a concurrency primitive — needs careful review of
  lock/unlock paths to confirm semantics preserved. SRFI-18 conformance
  test suite must continue to pass.

**Impact**: Small but isolates a concurrency invariant in the type
system. Lower priority than the IsVoid / Port findings.

**Estimated size**: S (well-scoped; concurrency review required).

---

### Finding 8 — `Datum()` method scattered across 23 types with no shared contract

**Principle**: Composability — phantom interface
**Where**: 23 types in `values/`, each returning a different concrete
Go type (`int64`, `float64`, `complex128`, `string`, `rune`, `bool`,
`*bytes.Buffer`, `*bufio.Reader`, `[2]Value`, `[]Value`, `[]*Byte`,
`io.Reader`, …)
**Theory**: A method that appears on 23 types but is not part of any
interface, where each instance returns a different type, is **not** a
polymorphic operation — it is 23 unrelated methods that happen to share
a name. In type-theory terms, there is no parametric or ad-hoc
polymorphism: every site is monomorphic. The shared name is at best a
naming convention, at worst a false signal of generality.

**Current state**: 23 `Datum()` methods, 23 different return types. No
interface mentions `Datum()`. Most call sites are inside test code or
inside `values/` itself.

**Problem**: A reader sees `Datum()` on many types and infers a shared
contract. There isn't one. To call `Datum()` on a `Value`, you must
type-assert to the concrete type first, at which point `Datum()` is
just an accessor for an exported field (in most cases `p.Value`,
`p.buf`, `p.rdr`, …). The method is a thin wrapper around field
access; the wrapping adds zero information.

**Proposed direction**: Two options:

(a) **Promote `Datum()` to an interface** with `any` return type:

```go
type Datumable interface {
    Datum() any
}
```

Then callers receive a typed interface and can use `Datum()` polymorphically.
This trades type safety for uniformity — likely the wrong trade.

(b) **Delete `Datum()` where it duplicates field access**. Use direct
field access on the concrete type, since callers always type-assert
anyway. Keep `Datum()` only on the port types where the underlying buffer
type is *not* a public field. For numeric types, just expose `Value` field
directly (already done).

Reading the call sites suggests (b) is the right answer: 18 of the 23
`Datum()` calls are in `values/` test code and tutorial-style integration
tests. Production callers are rare. Most types already export their
underlying value as a public field (`Integer.Value`, `Float.Value`,
`String.Value`).

**Trade-offs**:
- **Pro (b)**: Removes 23 methods that add no information beyond field
  access. Authors learn the concrete-type-then-field-access pattern,
  which is consistent with how every other Go codebase works.
- **Con (b)**: Test fixtures need touch-ups. Ports may need a public
  field rename (e.g., `Port.Buf` instead of `Port.Datum()`).

**Impact**: Cleanup; not load-bearing. Could be folded into the Port
unification (Finding 2) since ports are 9 of the 23 `Datum()` sites.

**Estimated size**: S.

---

### Finding 9 — Two `Value` types are not pointers: `Pair [2]Value`, `ByteVector []*Byte`

**Principle**: State tightness / representation choice
**Where**: `values/pair.go:75`, `values/byte_vector.go:22`
**Theory**: This is more an observation than a problem. Most Value types
are `*StructName`; two types are array/slice value types: `Pair` is
`[2]Value` (used by pointer everywhere as `*Pair`), and `ByteVector`
is `[]*Byte` (used by pointer as `*ByteVector`).

The Pair representation has a subtle consequence: `(*Pair)(nil)` is a
typed nil pointer, while the underlying `[2]Value` is value-typed. The
nil-receiver convention for `IsVoid` works because Go permits methods
on `*Pair` with a nil receiver. This is consistent with all other
pointer-receiver types — *no asymmetry*.

The ByteVector slice representation means `(*ByteVector)(nil)` is the
"void" form, while `*ByteVector(&[]*Byte{})` is the empty-but-present
form. The void/empty distinction is meaningful here (consistent with
R7RS bytevector semantics).

**Current state**: Two `non-struct` Value types exist alongside ~30
struct types. The mixing is intentional and load-bearing for Pair's
cycle-detection (`*Pair` pointer identity is the only way to compare
cells for `set-car!`/`set-cdr!` aliasing).

**Problem**: None, structurally. Documenting this in case Finding 1's
convention-test enumerates non-struct types — both `*Pair` and
`*ByteVector` are reflectable as nil pointers, so the proposed
exemplar-roster construction (`reflect.New(rt.Elem()).Elem().Interface()`)
works uniformly across struct and non-struct receivers.

**Proposed direction**: No action. Recorded so future readers don't
mistake the mixing for accidental.

**Impact**: Informational.

**Estimated size**: 0.

---

## Opportunities (sort-package style)

### Opportunity 1: `IsVoid` convention test + `allValueExemplars` roster

**Status**: Recast — the original "delete 51 methods, replace with
reflection" framing was retracted (see Finding 1 revision history).
The opportunity here is *additive*: codify the existing convention
and gain a reusable closed-world roster of Value types.

**Replaces**: An implicit convention enforced only by code review.

**Core operation**: At test time, walk every concrete `Value`
implementer, construct a typed nil pointer, and assert
`(*T)(nil).IsVoid()` matches the documented contract (true for
pointer-receiver types by default; false for the three documented
singletons; false for `SourceIndexes` which uses a value receiver).

**Algebraic structure**: A **closed-set predicate test** — assert a
property holds across all members of a documented enumeration. The
roster is the enumeration; the predicate is the convention.

**Proposed shape**: A new `values/exemplars_test.go` declares a
`allValueExemplars []Value` slice containing one entry per concrete
`Value` implementer — typed nil pointers for pointer-receiver types,
zero-value structs for the value-receiver singletons (`voidType{}`,
`eofType{}`, `emptyListType{}`, `SourceIndexes{}`). A companion test
walks the package's exported types via reflection and fails if any
concrete `Value` implementer is missing from the roster.

The convention test (`TestIsVoidConvention`) iterates the roster,
asserting each exemplar's `IsVoid()` return matches an exceptions
map that lists the four documented deviations from the default
"nil receiver → true" rule.

**Reuse sites**: The roster is also a natural input for Opportunity 4
(`goTypeToValueType` reverse map) and for any future cross-type audit
(e.g., "every Value type has a Hashable-or-not classification",
"every Value type with a Datum() returns a documented Go type").

---

### Opportunity 2: Unified `Port` struct with capability accessors

**Replaces**: 9 concrete port types (`BinaryInputPort`,
`BinaryOutputPort`, `CharacterInputPort`, `CharacterOutputPort`,
`StringInputPort`, `StringOutputPort`, `ByteVectorInputPort`,
`ByteVectorOutputPort`, `ByteVectorBufferedOutputPort`,
`ByteVectorInputOutputPort`) — ~900 LOC across 10 files.

**Core operation**: Carry an `(io.Reader, io.Writer, ...)` capability
tuple plus a `portBase` and a Scheme kind tag.

**Algebraic structure**: The capability set is a **product** of optional
slots — each slot is a `T | nil` interface. The 9 port types are 9
specific factorizations of this product. By introducing the product
explicitly, the factorizations collapse to constructor arguments.

**Proposed shape**:

```go
type Port struct {
    portBase
    rdr io.Reader; rb io.ByteReader; rr io.RuneReader
    urb byteUnreader; urr runeUnreader
    wrt io.Writer; wb io.ByteWriter; wr runeWriter
    flsh flusher
    ws  io.StringWriter
    ext byteVectorExtractor
    flushOnClose bool
}

func NewBinaryInputPort(rdr *bufio.Reader) *Port { ... }
func NewByteVectorInputOutputPort(buf *bytes.Buffer) *Port { ... }
// ...etc
```

**Reuse sites**: Anywhere a port is constructed or type-checked. The 6
port-related `ValueType` checks (`TypeBinaryInputPort`, etc.) become
capability inspections of the single `*Port` type.

---

### Opportunity 3: `NumericTypeSpec` registry — single source of numeric facts

**Replaces**: The 12-item "ADDING A NEW NUMERIC TYPE" guide. Centralizes
seven distinct cross-package facts into one record per kind.

**Core operation**: Map `NumericKind → NumericTypeSpec` where each spec
carries name, exactness, integer/rational predicates, simplification
target, conversion adapters, parser, and FFI adapters.

**Algebraic structure**: This is a **typeclass-style table-driven
dispatch** — each numeric kind is a row in a horizontal table, each
operation is a column. The current code has columns scattered across
12 files; the registry collapses them into one structure.

**Proposed shape**:

```go
type NumericTypeSpec struct {
    Kind         NumericKind
    SchemeName   string
    Exact        bool
    IsInteger    bool
    IsRational   bool
    SimplifyTo   func(Number) Number
    ToFloat64    func(Number) float64
    ToComplex128 func(Number) complex128
    Parse        func(string) (Number, bool, error)
    FromGoValue  func(reflect.Value) (Number, bool)
    ToGoValue    func(Number) (reflect.Value, bool)
}

var numericRegistry [numKinds]NumericTypeSpec
```

**Reuse sites**: `Simplify`, `ExactnessOf`, `NumberToFloat64`,
`NumberToComplex128`, `registry/helpers/value_conv.go`,
`extensions/math/prim_conversion.go`,
`extensions/math/prim_complex.go`, `ffi.go`,
`internal/parser/parser_number.go`,
`registry/helpers/equality.go`, and the external `wile-goast` repo's
`numberToAST`.

---

### Opportunity 4: `goTypeToValueType` reverse map — bridge from Go runtime to ValueType

**Replaces**: The 45-line `SchemeTypeName` switch and the silent
fall-through to `fmt.Sprintf("%T", v)`.

**Core operation**: Look up the canonical `ValueType` for a runtime
`Value`, falling back to interface checks for the truly polymorphic
cases (`InputPort`, `OutputPort`, `Tuple`, `Callable`).

**Algebraic structure**: An **inverse function** for the
`ValueType → exemplar Go type` map that already exists implicitly
in `checks`. Building it explicitly removes the duplication.

**Proposed shape**: Built once at init time alongside `typeNames` and
`checks`. Consulted by `SchemeTypeName`. The bridge is also reusable
by `NumericTypeSpec` (Opportunity 3) for `FromGoValue`.

**Reuse sites**: `SchemeTypeName`, plus the numeric registry's FFI
adapters, plus any future "what ValueType is this Go value?" question.

---

## What's already done well (preserve)

- **`Value` interface is small** (3 methods today: `SchemeString`,
  `IsVoid`, `EqualTo`). Each method pays its rent — `IsVoid` in
  particular is the ergonomic, type-specific nil check that lets
  callers avoid reflection (see Finding 1).
- **`Number` interface is well-factored** into `Number`, `ComplexNumber`,
  `RealNumber` — three levels of capability matching R7RS §6.2.6.
- **Dispatch table generators** (`makeArithmeticDispatch`,
  `makeLessThanDispatch`, etc.) are an excellent application of
  factoring out common structure: 294 closures generated from 5
  generators.
- **Compile-time interface assertions** (`var _ Value = (*T)(nil)`)
  in every type file catch interface drift at build time.
- **`init()` panic checks in `value_type.go`** verify that every
  `ValueType` has a name, description, and check function. State-space
  completeness is enforced; new ValueType additions fail loudly.
- **`Pair`/`emptyListType` migration** (referenced by `internal/`
  Finding 1) is **complete in `values/`**. Every remaining `*Pair`
  type assertion is a load-bearing pointer-identity check for cycle
  detection (`scheme_writer.go`, `utils.go:140`) or for direct mutation
  (`SetCar`/`SetCdr` in pair.go). No defensive guards remain. The
  parallel migration story holds on both sides.
- **R7RS-aligned partitioning**: every type maps cleanly to a Scheme
  spec section. The Go decomposition mirrors the Scheme decomposition.

These properties should be preserved through any refactor.

---

## Closing summary

**State-space summary**:
- `values/` defines 13+ public interfaces and ~50 concrete Value types.
- Port subtypes have a representable state space of 2^10 ≈ 1024
  capability combinations; only 10 are actually used (type precision ≈ 1%).
- `MutexState` has 4 values with implicit owner-coupling — 8
  representable, 4 valid (precision 50%).
- The numeric tower has 7 kinds × 7 operations × 7 dispatch outcomes =
  294 dispatch closures, generated from 5 generic generators. Type
  precision here is good — the LUB structure is enforced.
- `IsVoid()` is implemented 50 times for 50 Value types — uniform
  implementations of a convention currently enforced by code review.

**Dependency count**: 1 outgoing dependency (`werr/`), 32 packages depend
on `values/`. Instability **I ≈ 0.03** — near-minimal. The
package is doing its job as a stable foundation, but every structural
choice has high blast radius.

**Top 2 highest-impact changes**:

| # | Change                                       | States eliminated | Dependencies removed | Reuse sites gained | Estimated size |
|---|----------------------------------------------|-------------------|----------------------|--------------------|----------------|
| 1 | **Opportunity 2 — `Port` unification**       | ~900 LOC, 9 types | None internal; 6 ValueType checks simplify | All port construction & checking | L |
| 2 | **Opportunity 3 — `NumericTypeSpec` registry** | 7×6 = 42 leakage sites | Cross-package: 6 files in registry/extensions, 1 external repo | All numeric-type-aware code | L |

Opportunity 1 (the `IsVoid` convention test) is small and additive but
not load-bearing; it is independent and could ride along with any
cleanup PR.

The four quick wins (Findings 4, 5, 6, 7) can be bundled into one PR
as cleanup. The two major opportunities are independent and should be
sequenced by risk:

1. **Port unification** first — largest LOC reduction, isolated to
   `values/` plus a few extension callers.
2. **Numeric registry** second — touches the most external code,
   including the external `wile-goast` repo. Design pass before code.

---

## Recommended phasing

**Phase 0 — Quick wins (single PR)**:
- Finding 4: Delete `TypeExactInteger`, migrate 18 call sites.
- Finding 5: Delete `makeInterfaceCheck`, fold into `makeCheck`.
- Finding 6: Build `goTypeToValueType` reverse map; rewrite
  `SchemeTypeName`.
- Estimated: ~200 LOC, 1 PR.

**Phase 1 — Mutex state sum-type (Finding 7)**:
- Collapse `LockedOwned`/`LockedNotOwned` into a single `msLocked` tag
  with owner-as-discriminator.
- Run SRFI-18 conformance test suite before/after.
- Estimated: ~50 LOC delta, 1 PR.

**Phase 2 — Port unification (Opportunity 2)**:
- Design pass first — document the capability-matrix design in a
  follow-on plan file.
- Migrate one port type at a time; each migration is a single PR.
- Keep type aliases for backward compatibility through the transition:
  `type BinaryInputPort = Port`.
- Estimated: 4-6 PRs over 2-3 weeks. Bench-gated on each PR.

**Phase 3 — Numeric registry (Opportunity 3)**:
- Design pass first — `plans/2026-05-??-numeric-registry-design.md`.
- Identify which hot-path dispatch lookups must stay direct vs. which
  can move to registry indirection.
- Migrate consumers one at a time; the external `wile-goast` repo is a
  separate PR there.
- Estimated: 3-4 PRs over 2 weeks. Bench-gated.

**Phase 4 — Cleanup (Findings 8, 9)**:
- Delete `Datum()` methods where they duplicate field access. Update
  test fixtures.
- Estimated: ~100 LOC, 1 PR.

**Optional, independent — `IsVoid` convention test (Opportunity 1)**:
- Add `allValueExemplars` roster + `TestIsVoidConvention` +
  `TestExemplarRosterIsComplete` in `values/exemplars_test.go`.
- No production code change.
- Can ride along with any of the cleanup phases above.
- Estimated: ~80 LOC of test code, +0 LOC production, 0 PR if bundled.

---

## Cross-references

- `plans/2026-05-07-structural-reduction-roadmap.md` — Tier A.1
  (this plan completes the analysis side of A.1).
- `memory/2026-05-09-environment-structural-reduction.md` — Tier A.2
  precedent for the SR plan format. Closed via PR #730.
- `plans/2026-05-06-machine-structural-reduction.md` — Tier A.0;
  Finding 7 stages 1-2 closed via PRs #742-#745.
- `plans/2026-05-07-internal-structural-reduction.md` — Tier A.0;
  phases 1-5 closed.
- `memory/MEMORY.md` — Architecture Quick Reference.

After this plan ships its implementation, Tier A.1 is complete. Tier
A.3 (`registry/`) remains as the last Tier A target before moving to
Tier B (`wile/` root, `repl/`).
