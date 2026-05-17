# Crosscheck Findings — `fix/write-string-textual-validation`

**Date**: 2026-05-17
**Branch**: `fix/write-string-textual-validation`
**Commit**: `086970d9` — `fix(io): reject binary output ports in write-string`
**Lenses run**: 5 — `code`, `errors`, `types`, `tests`, `consistency` (all Opus)
**Source**: `/crosscheck:crosscheck all fix/write-string-textual-validation`

## Context

The PR closes follow-up #2 from `plans/2026-05-14-port-unification-impl.md`:
`write-string` was the only R7RS textual-write primitive that accepted
binary output ports. The fix inlines an `AsByteWriter` check in
`PrimWriteString` rather than routing through the existing
`getOptionalTextualOutputPort` helper, because `write-string` also
needs the rest-tuple for `start`/`end` parsing.

## Headline finding

### Five-lens convergence — `internal/extensions/io/prim_write.go:174-184`

All five orthogonal lenses flagged the same site: `PrimWriteString`
hand-rolls a duplicate of `getOptionalTextualOutputPort`
(`prim_read_write.go:108-119`). Convergence across five mandates is
the strongest validity signal the workflow produces.

**Lens votes**:

| Lens | Severity | Posture |
|------|----------|---------|
| code-reviewer | Important (88) | CONVENTION |
| silent-failure-hunter | HIGH | CONVENTION |
| type-design-analyzer | — | CONVENTION + SYMPTOM |
| pr-test-analyzer | — | CONVENTION (structural) |
| consistency-checker | Notable | CONVENTION (accepted as documented divergence) |

**Evidence**:

- Six sibling write primitives (`PrimWrite:50`, `PrimWriteChar:69`,
  `PrimDisplay:87`, `PrimNewline:102`, `PrimWriteSimple:120`,
  `PrimWriteShared:140`) all call `getOptionalTextualOutputPort`.
  `PrimWriteString` is the lone outlier.
- The package's own `internal/extensions/io/CLAUDE.local.md` "Port
  Type Taxonomy" table names `getOptionalTextualOutputPort` as the
  canonical helper for "AsWriter present + AsByteWriter absent →
  textual" with sentinel `ErrNotATextualPort`.
- CLAUDE.md "Refactoring §1-5" mandates routing the same capability
  check through one site.

**Why I chose inline (recorded for future readers)**:

`getOptionalTextualOutputPort` returns `(*PortObject, error)`.
`PrimWriteString` needs the rest-tuple for `ParseSubrange` on
`start`/`end`. The plan offered (a) extract
`getOptionalTextualOutputPortWithRest` or (b) inline. I chose (b)
to avoid creating a single-caller helper.

**Recommended fix**:

Factor `getOptionalTextualOutputPort` into two pieces — a
`requireTextualOutput(p *PortObject) error` helper that operates on
an already-resolved port, plus the existing port-resolution flow.
Then both `getOptionalTextualOutputPort` and `PrimWriteString` can
call `requireTextualOutput` and the predicate lives in one place.

```go
// prim_read_write.go
func requireTextualOutput(p *values.PortObject) error {
    _, isBinary := p.AsByteWriter()
    if isBinary {
        return werr.WrapForeignErrorf(werr.ErrNotATextualPort,
            "expected a textual output port, got binary port")
    }
    return nil
}
```

`PrimWriteString` becomes:
```go
port, tuple, found, err := extractPort(...)
// ... resolve port ...
err = requireTextualOutput(port)
if err != nil { return err }
```

Estimated effort: ~15 LOC, one PR.

## Convention-grounded findings (ship-ready)

### 1. Missing sentinel assertion in test — `internal/extensions/io/prim_read_write_test.go:1126-1130`

[CONVENTION] | flagged by pr-test-analyzer (rated 8/10)

The new "binary port" test case uses `evalExpectError`
(`prim_ports_test.go:47-55`) which only asserts `err != nil`. The
sentinel identity `werr.ErrNotATextualPort` is not checked.

**Prior art**: `prim_read_write_test.go:1189` uses
`qt.Assert(t, errors.Is(err, werr.ErrNotAByte), qt.IsTrue)`. This
is the established pattern in the same file.

**Why it matters**: The whole point of choosing `ErrNotATextualPort`
over the previously-returned `ErrNotAnOutputPort` is that callers
can distinguish them programmatically. A regression that flipped the
sentinel back would pass the current test silently.

**Fix**: Add a focused subtest:
```go
_, err := engine.EvalMultiple(context.Background(),
    `(write-string "hello" (open-output-bytevector))`)
qt.Assert(t, errors.Is(err, werr.ErrNotATextualPort), qt.IsTrue)
```

## SYMPTOM findings — verified, no action required

These were flagged as ⚠ verify-mechanism. Verification against the
actual code dissolved both prescriptions.

### 2. Hoisted `AsByteWriter` check on no-port path — `prim_write.go:171-184`

[SYMPTOM] | flagged by silent-failure-hunter (HIGH), code-reviewer
(Important 80), type-design-analyzer

**Concern**: The `AsByteWriter` check at line 180 is hoisted out of
the `else` branch and runs unconditionally — including for the
default-port resolution at line 172. A binary current-output-port
(or a future port with both `wb` and `wr` slots) could trigger an
unexpected rejection or a misleading error message.

**Verification** (`state.go:215-224` + `state.go:128-142`):

`resolveCurrentOutputPort` routes through `currentTextualOutputPort`
which **already** asserts `AsRuneWriter`. If `current-output-port`
is parameterized to a binary port, `resolveCurrentOutputPort` panics
with `ErrNotATextualPort` (caught by `OperationForeignFunctionCall`'s
recover, converted to a Scheme exception) **before** control reaches
the hoisted check.

The "wb + wr both present" future-bug trap doesn't exist: the
`values/CLAUDE.local.md` kind/slot table shows no current port kind
has both, and the invariant is enforced by every factory in
`values/port_constructors.go`.

**Verdict**: Hoisted check is dead code on the no-port path. Not a
correctness bug. Moving it inside `else` is optional structural
cleanup, not required.

### 3. `AsWriter !ok` collapses closed-port and non-output failure modes — `prim_write.go:174-178`

[SYMPTOM] | flagged by silent-failure-hunter (MEDIUM)

**Concern**: If `AsWriter` returns `!ok` for both closed ports and
non-output ports, the diagnostic `ErrNotAnOutputPort` would
incorrectly classify a closed output port as a type error.

**Verification** (`port.go:177-182` + `port_helpers.go:93-99`):

`AsWriter` returns `ok = p.wrt != nil` — it tests **slot presence**,
not close state. A closed output port returns `ok=true` from
`AsWriter`. Closed-port rejection happens at write-time via the slot
wrapper: `port.Write → guardedWriter.Write → guardedWrite →
b.guardClosed → ErrPortClosed`.

The `AsWriter !ok` path only fires when `p.wrt == nil` (non-output
port). `ErrNotAnOutputPort` is the correct sentinel for that case.

**Verdict**: Not a real issue. The slot-wrapper architecture (PR #749
Phase 2) separates "do you have the capability?" (slot presence) from
"can you use it right now?" (close state). The two failure modes are
handled by orthogonal code paths.

## Notable, deferred

### 4. Binary-rejection message uses literal `"binary port"` instead of `port.PortKind()` — `prim_write.go:182-183`

[CONVENTION] | flagged by silent-failure-hunter, consistency-checker

The first inline message at line 177 (after this PR's diagnostic
improvement) uses `port.PortKind()`. The new binary-rejection message
at line 183 uses the literal `"binary port"`. Asymmetric.

The helper at `prim_read_write.go:116` has the same literal, so the
diff is faithful to the helper. Fixing here alone would diverge from
the helper. Fix both or neither.

**Action**: Defer. If the helper-extraction fix (headline finding) is
done, address both sites in the same PR.

### 5. Test coverage gaps — `prim_read_write_test.go`

[CONVENTION + SYMPTOM] | flagged by pr-test-analyzer

Two flavors not exercised by the new "binary port" test:

- **`bytevector-input-output` port**: No Scheme-level constructor
  registered (`register.go`). Would need a Go-level test mirroring
  `TestDefaultInputPortRead:1401` using `values.NewByteVectorInputOutputPort()`.
  The fix's logic (`AsByteWriter`-present → reject) does cover this
  flavor, but nothing asserts it.

- **Parameterized default path**: A test where `current-output-port`
  is parameterized to a binary port via `parameterize`. Given the
  verification of SYMPTOM #2 above, this path is already handled at
  `resolveCurrentOutputPort`, but a regression test would lock in
  the intent.

**Action**: Worth one PR; not blocking.

### 6. First-class textual/binary predicate on `*PortObject` — `values/port.go` (absence)

[CONVENTION] | flagged by type-design-analyzer

The package has `PortKind() string` but no `IsTextualOutput() bool` /
`IsBinaryOutput() bool` predicate. Every call site that wants to
discriminate has to reconstruct "binary = has byte-writer slot"
from raw accessors.

**Action**: Defer. Only justified if a third call site appears.

## Items addressed in original PR (no follow-up needed)

- ✓ Diagnostic improvement to the AsWriter-failure path now includes
  `port.PortKind()` (matches sibling helper convention at
  `prim_read_write.go:99,145`).
- ✓ Compound-if split in `values/value_isvoid_convention_test.go:147`
  per `noCompoundIf` ruleguard (drive-by fix for pre-existing
  violation from commit `e93448c4`).
- ✓ Test case naming and table placement match siblings.
- ✓ VERSION bump matches pre-commit hook pattern.

## Recommended sequencing

1. **In this PR** (small, ship-ready): Add the sentinel-assert test
   (finding 1).
2. **Follow-up PR**: Extract `requireTextualOutput` helper (headline
   finding), which also closes finding 4 (literal-"binary port"
   asymmetry) in the same pass.
3. **Optional follow-up**: Test coverage for bytevector-input-output
   (finding 5).
4. **Defer indefinitely**: First-class predicate on `*PortObject`
   (finding 6) — only if a third call site appears.

## Cross-references

- Parent plan: `plans/2026-05-14-port-unification-impl.md` (follow-up #2)
- Parent SR plan: `plans/2026-05-13-values-structural-reduction.md`
- Five-lens grounding-posture protocol:
  `/Users/aalpar/.claude/plugins/marketplaces/local/plugins/crosscheck/skills/crosscheck/SKILL.md`
