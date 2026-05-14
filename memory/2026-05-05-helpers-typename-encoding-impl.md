# Helpers TypeName Encoding — Implementation Plan

**Date**: 2026-05-05
**Status**: ✅ **SHIPPED** — PR #725 (`feat/helpers-typename-encoding`), merge commit `79e7ce05`.
**Scope**: Drop the redundant `typeName` parameter from `registry/helpers` argument-extraction helpers by encoding the type phrase on the type-mismatch sentinel itself.

## Motivation

`helpers.RequireArg`, `RequireType`, `OptionalArg`, `VariadicArgs`, and the
`SequenceLength/Ref/Set` family currently take both a sentinel error
(`werr.ErrNotAString`) *and* a redundant human-readable type phrase
(`"a string"`). Across ~157 call sites in 37 files, the pair is 1:1 — every
`ErrNotAString` is paired with `"a string"`. The single exception is
`ErrInvalidArgument → "a namespace"` (11 sites), which leans on a generic
sentinel because no `ErrNotANamespace` exists yet.

The redundancy is unenforced (a typo like `werr.ErrNotAString, "an integer"`
compiles cleanly) and produces five-arg helper signatures that shadow what
should be four-arg calls. It also drives doc rot — `registry/helpers/CLAUDE.md`
already documents the four-arg shape that doesn't yet exist.

## Design Decisions

**Encoding (Option A)** — add an `expectedType` field to `werr.StaticError`,
plus a `NewTypeSentinel(noun)` constructor and `TypeName() string` method.
Existing `NewStaticError` callers are unaffected; type sentinels opt in by
switching to the new constructor.

`NewTypeSentinel` takes a *bare* noun ("string", "integer", "char-set"); it
auto-prefixes "a"/"an" via an `articleFor` helper using the conventional
letter rule (vowel letter → "an", else → "a"). Inputs already starting
with "a " or "an " pass through verbatim, providing an escape hatch for
phonetic exceptions like "once" (pronounced /wuns/, takes "a"). This keeps
the API to a single constructor while handling irregular cases without a
separate function.

**Namespace cases (Path 1)** — introduce `werr.ErrNotANamespace` as a real
type sentinel and migrate the 11 `ErrInvalidArgument → "a namespace"` call
sites. Helpers have **no** override path; every `RequireArg`-shaped call
takes a real type sentinel.

Rejected alternatives (recorded for posterity):
- Option B (separate `*TypeSentinel` wrapper type) — buys compile-time
  enforcement but changes the type of `ErrNotAX` vars; rejected as larger
  blast radius for marginal safety gain in an internal API.
- Option C (parse "not " prefix from message) — couples message text to
  behavior, fragile to message edits, and cannot represent the namespace
  case at all.
- Path 2 (parallel `RequireArgAs` override variant) — keeps a generic
  escape hatch but preserves the redundancy at the few sites that use it;
  Path 1 is uniform.

## Scope

### Files modified

| Layer | Files | Purpose |
|---|---|---|
| `werr/werr.go` | 1 | Add field + constructor + method; convert ~22 `ErrNotAX` to `NewTypeSentinel`; add `ErrNotANamespace` |
| `registry/helpers/args.go` | 1 | Drop `typeName` from `RequireArg`, `RequireType`, `OptionalArg` |
| `registry/helpers/variadic.go` | 1 | Drop `typeName` from `VariadicArgs` |
| `registry/helpers/sequence.go` | 1 | Drop `typeName` from `SequenceLength/Ref/Set` |
| `registry/helpers/*_test.go` | ~3 | Update helper unit tests to four-arg shape |
| Call sites | 37 | Mechanical signature update |
| `registry/helpers/CLAUDE.md` (or .local.md) | 1 | Sync with new signatures |
| `registry/CLAUDE.md` | 1 | Update optional-argument decision tree |

### Inventory of call-site directories

```
extensions/charsets/        1 file
extensions/eval/            1 file
extensions/files/           2 files
extensions/gointerop/       1 file
extensions/introspection/   1 file  (also has 1 namespace site)
extensions/process/         1 file
extensions/threads/         1 file
internal/extensions/all/    3 files
internal/extensions/envvars/1 file
internal/extensions/io/     4 files
internal/extensions/namespace/ 1 file (10 namespace sites)
registry/core/             20 files
```

Total: 37 production files + helper tests.

### Sentinel migration in `werr/werr.go`

Sentinels constructed via `NewTypeSentinel` (bare noun; article auto-derived):

```
ErrNotABoolean              "boolean"          → "a boolean"
ErrNotAnInputPort           "input port"       → "an input port"
ErrNotAnOutputPort          "output port"      → "an output port"
ErrNotABox                  "box"              → "a box"
ErrNotAnOpaqueValue         "opaque value"     → "an opaque value"
ErrNotAByte                 "byte"             → "a byte"
ErrNotAByteInputPort        "byte input port"  → "a byte input port"
ErrNotAByteOutputPort       "byte output port" → "a byte output port"
ErrNotATextualPort          "textual port"     → "a textual port"
ErrNotAPrimitive            "primitive"        → "a primitive"
ErrNotANumber               "number"           → "a number"
ErrNotAReal                 "real number"      → "a real number"
ErrNotAList                 "list"             → "a list"
ErrNotAMachineContext       "machine context"  → "a machine context"
ErrNotAPair                 "pair"             → "a pair"
ErrNotACons                 "cons"             → "a cons"
ErrNotACharacter            "character"        → "a character"
ErrNotACharSet              "char-set"         → "a char-set"
ErrNotASyntaxValue          "syntax value"     → "a syntax value"
ErrNotASyntaxPair           "syntax pair"      → "a syntax pair"
ErrNotASyntaxSymbol         "syntax symbol"    → "a syntax symbol"
ErrNotASyntaxList           "syntax list"      → "a syntax list"
ErrNotASyntaxObject         "syntax object"    → "a syntax object"
ErrNotASymbol               "symbol"           → "a symbol"
ErrNotAClosure              "closure"          → "a closure"
ErrNotAnInteger             "integer"          → "an integer"
ErrNotALocalEnvironmentFrame "local environment frame" → "a local environment frame"
ErrNotAMachineTemplate      "machine template" → "a machine template"
ErrNotAString               "string"           → "a string"
ErrNotANamespace            "namespace"        → "a namespace"   (NEW)
ErrNotAVector               "vector"           → "a vector"
ErrNotAByteVector           "bytevector"       → "a bytevector"
ErrNotAProcedure            "procedure"        → "a procedure"
ErrNotAParameter            "parameter"        → "a parameter"
ErrNotAStringOutputPort     "string output port"     → "a string output port"
ErrNotABytevectorOutputPort "bytevector output port" → "a bytevector output port"
ErrNotANativeError          "error object"     → "an error object"
ErrNotARecord               "record"           → "a record"
ErrNotARecordType           "record type"      → "a record type"
ErrNotAThread               "thread"           → "a thread"
ErrNotAMutex                "mutex"            → "a mutex"
ErrNotAConditionVariable    "condition variable" → "a condition variable"
ErrNotATime                 "time"             → "a time"
ErrNotAChannel              "channel"          → "a channel"
ErrNotAWaitGroup            "wait-group"       → "a wait-group"
ErrNotARWMutex              "rw-mutex"         → "a rw-mutex"
ErrNotAOnce                 "a once"           → "a once"        (PASS-THROUGH: /wuns/)
ErrNotAnAtomic              "atomic"           → "an atomic"
ErrNotAHashtable            "hashtable"        → "a hashtable"
ErrNotAMatch                "match"            → "a match"
ErrNotAPromptTag            "prompt tag"       → "a prompt tag"
ErrNotAContinuationMarkSet  "continuation mark set" → "a continuation mark set"
ErrNotAContinuation         "continuation"     → "a continuation"
ErrNotAnErrorContext        "error context"    → "an error context"
ErrNotAProcess              "process"          → "a process"
```

`ErrNotAOnce` uses pass-through because the letter rule would emit "an once"
but the word is pronounced with a /w/ consonant sound.

Sentinels that stay as `NewStaticError` (not type sentinels): `ErrStopIteration`,
`ErrNoSuchBinding`, `ErrCannotCompare`, `ErrDivisionByZero`, `ErrFileNotFound`,
all the `ErrXxxFailed` errors, etc.

## Phases

### Phase 1 — `werr` encoding

Add to `werr/werr.go`:

```go
type StaticError struct {
    message      string
    expectedType string  // "" for non-type sentinels
}

func NewTypeSentinel(noun string) *StaticError {
    var typeName string
    switch {
    case strings.HasPrefix(noun, "a "), strings.HasPrefix(noun, "an "):
        typeName = noun // pass-through for irregulars
    default:
        typeName = articleFor(noun) + " " + noun
    }
    return &StaticError{
        message:      "not " + typeName,
        expectedType: typeName,
    }
}

func articleFor(noun string) string {
    if len(noun) == 0 {
        return "a"
    }
    switch noun[0] {
    case 'a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U':
        return "an"
    }
    return "a"
}

func (p *StaticError) TypeName() string {
    return p.expectedType
}
```

Convert the ~50 `NewStaticError("not ...")` declarations listed above to
`NewTypeSentinel("...")` with the bare noun (article stripped). Verify
message strings are byte-identical afterward (so `Error()` output is
unchanged).

Add `ErrNotANamespace = NewTypeSentinel("a namespace")` in the existing block.

Add a guard test in `werr/werr_test.go` (or wherever the existing test
file lives) that pins the type-sentinel inventory:

```go
func TestTypeSentinelsCarryTypeName(t *testing.T) {
    typeSentinels := []*StaticError{
        ErrNotABoolean, ErrNotAnInputPort, ErrNotAnOutputPort,
        ErrNotABox, ErrNotAnOpaqueValue, ErrNotAByte,
        ErrNotAByteInputPort, ErrNotAByteOutputPort, ErrNotATextualPort,
        ErrNotANumber, ErrNotAReal, ErrNotAList, ErrNotAPair,
        ErrNotACharacter, ErrNotACharSet, ErrNotASymbol, ErrNotAClosure,
        ErrNotAnInteger, ErrNotAString, ErrNotAVector, ErrNotAByteVector,
        ErrNotAProcedure, ErrNotAParameter, ErrNotARecord, ErrNotARecordType,
        ErrNotAThread, ErrNotAMutex, ErrNotAConditionVariable,
        ErrNotATime, ErrNotAChannel, ErrNotAOnce, ErrNotAnAtomic,
        ErrNotAHashtable, ErrNotAPromptTag, ErrNotAContinuationMarkSet,
        ErrNotAContinuation, ErrNotAWaitGroup, ErrNotANamespace,
    }
    for _, s := range typeSentinels {
        if s.TypeName() == "" {
            t.Errorf("sentinel %q missing TypeName — was NewTypeSentinel skipped?",
                s.Error())
        }
    }
}
```

This catches the "forgot to switch a sentinel from `NewStaticError` to
`NewTypeSentinel`" failure mode at test time rather than via degraded
runtime error messages. The list is the authoritative inventory; adding
a new type sentinel later means appending here too.

**Verification**: `go test ./werr/...` passes; `make lint` clean.

### Phase 2 — helpers signature change

In `registry/helpers/args.go`, change:

```go
func RequireArg[T any](mc CallContext, index int, sentinel error, typeName, name string) (T, error)
```

to:

```go
func RequireArg[T any](mc CallContext, index int, sentinel error, name string) (T, error)
```

Inside, look up the type phrase via:

```go
typeName := ""
var se *werr.StaticError
if errors.As(sentinel, &se) {
    typeName = se.TypeName()
}
```

Apply the same change to `RequireType`, `OptionalArg`, `VariadicArgs`,
`SequenceLength`, `SequenceRef`, `SequenceSet`.

Update the helpers' own unit tests (`registry/helpers/*_test.go`) to the
four-arg shape. The tests construct sentinels directly; ensure they use
`NewTypeSentinel` where the test exercises type-mismatch error formatting.

**Verification**: `go test ./registry/helpers/...` passes. The package
compiles; downstream callers do **not** yet — that lands in Phase 3.

### Phase 3 — call-site migration

For each of the 37 production files, drop the `typeName` argument from
every `helpers.RequireArg`/`RequireType`/`OptionalArg`/`VariadicArgs`/
`SequenceLength`/`SequenceRef`/`SequenceSet` call.

This is mechanical: a regex of the form
`helpers\.(RequireArg|...)\[([^]]+)\]\(([^,]+),([^,]+),([^,]+)werr\.([A-Za-z]+),\s*"[^"]+",\s*("[^"]+|name)\)`
maps to dropping the typeName group. Recommended approach: write a small
script or use `gofmt`-aware AST rewrite (e.g., `gofmt -r`).

Sample rewrite rules:

```
gofmt -r 'helpers.RequireArg[a](b, c, d, e, f) -> helpers.RequireArg[a](b, c, d, f)'
gofmt -r 'helpers.RequireType[a](b, c, d, e) -> helpers.RequireType[a](b, c, e)'
gofmt -r 'helpers.OptionalArg[a](b, c, d, e, f) -> helpers.OptionalArg[a](b, c, d, f)'
gofmt -r 'helpers.VariadicArgs[a](b, c, d, e, f) -> helpers.VariadicArgs[a](b, c, d, f)'
gofmt -r 'helpers.SequenceLength[a](b, c, d, e) -> helpers.SequenceLength[a](b, c, e)'
```
*(verify exact identifier patterns match; gofmt's pattern variables match
single expressions so the rewrites should be safe)*

**Verification**: `go build ./...` succeeds; `make lint` clean. No
`go vet` warnings about unused imports or arguments.

### Phase 4 — Namespace sentinel migration

Migrate the 11 `werr.ErrInvalidArgument` → `werr.ErrNotANamespace` sites
in `extensions/introspection/prim_introspection.go` (1 site) and
`internal/extensions/namespace/prim_namespace.go` (10 sites).

**Verification**: `go test ./extensions/introspection/... ./internal/extensions/namespace/...` passes. Spot-check that error
messages still read sensibly.

### Phase 5 — docs sync

Update:

- `registry/helpers/CLAUDE.md` (or `CLAUDE.local.md`) — function tables
  reflect four-arg signatures; remove stale references to `typeName` parameter.
- `registry/CLAUDE.md` — the "Optional Argument Patterns" decision tree
  example loses its `typeName` argument.
- This plan file — append a "Shipped" note with PR link when merged.

## Verification

After all phases land:

```bash
make lint && make covercheck
```

Both must pass before PR. Then:

```bash
make ci   # local CI per feedback memory
```

per the `feedback_run_make_ci_before_pr.md` policy.

## Risks

- **Atomic-PR scope**: Phases 2–4 must land in a single PR because the
  helpers signature change breaks all callers simultaneously. Phases 1 and 5
  are independent and could land separately, but bundling them keeps the
  changeset coherent.
- **Sentinel-drift regressions**: The new behavior depends on every type
  sentinel being declared via `NewTypeSentinel`. If a sentinel listed above
  is missed, `RequireArg(..., werr.ErrNotAFoo, ...)` with that sentinel
  produces an empty type phrase ("expected  but got *Bar"). Mitigation:
  the `TestTypeSentinelsCarryTypeName` guard test in Phase 1 fails loudly
  if any inventory entry is left as `NewStaticError`. The inventory list
  above remains the human-readable truth source.
- **Wrong-sentinel passed to helper**: Option A doesn't catch
  `helpers.RequireArg(..., werr.ErrFileNotFound, ...)` at compile time. It
  produces a degraded error message. Mitigation: this isn't worse than today
  and could be addressed later with a ruleguard rule that flags type-sentinel
  arguments to `RequireArg` and friends.
- **External consumers of `*StaticError`**: The struct gains a field but no
  type rename. `errors.Is`, `errors.As`, and `Error()` all behave identically.
  No external API change.

## Out of scope

- The "variadic chain/fold generics" cleanup (High #2 in the
  staff-engineer assessment) — separate plan.
- The `ErrInvalidArgument` cases that are *not* type-mismatch (e.g.,
  range violations, malformed input). Those keep `ErrInvalidArgument`.
