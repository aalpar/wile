# Finding 2 — Tail/non-tail opcode collapse via `instr.Arg` sign bit

Implementation plan for Finding 2 of
`plans/2026-05-06-machine-structural-reduction.md`.

Parent finding: **Composability — Tail/non-tail opcode duplication: 28
(now 34) cases that differ in one bit**
(`plans/2026-05-06-machine-structural-reduction.md:138-192`).

## Decision

Per the parent plan: **collapse the 17 promoted-op tail/non-tail opcode
pairs (34 cases) to 17 single opcodes by encoding tail-ness in the high
bit of `instr.Arg`.** This is the parent plan's "decidable by measurement"
recommendation.

The parent plan was written when the count was 14 promoted ops (28
cases); the count is now 17 (34 cases) after additions of `OpMul`,
`OpDiv`, `OpCons` and their tail variants. The proposed encoding scales
identically.

**Scope:** promoted opcodes only (`OpEqQ`/`OpEqQTail` through
`OpDiv`/`OpDivTail`). The structurally-similar
`OpCallForeignCached`/`OpCallForeignCachedTail` pair is **out of scope**
— it was not analyzed by the parent plan, and including it would
contaminate the bench signal this PR is gating on. Defer to a future PR
if the verdict on Finding 2 is favorable.

## Gate

**This PR is bench-gated.** Per the parent plan's stated thresholds:

- Geomean delta within **±0.5%** vs `master`
- Per-bench delta within **±0.3%** vs `master`

Three prior memory records (`memory/callstack-optimization-attempt.md`,
`memory/promoted-ops-table-revert.md`,
`memory/maxcalldepth-nullable-revert.md`) show that **hot-path dispatch
changes have historically regressed**, including a 1.5% geo-mean
regression on the structurally-similar promoted-ops table-driven dispatch
attempt. The parent plan distinguishes table-driven dispatch (loading
function pointers from an array — rejected) from encoding-driven
dispatch (keeping the per-case switch, branching internally on
Arg sign — this proposal). The two have different cost models, but
the historical precedent demands measurement.

**If the bench gate fails**, the work is reverted as a documented
"considered and declined" entry on the parent plan, mirroring the
treatment of Finding 1 and Finding 4(b).

## Scope of changes

| File | Change |
|------|--------|
| `machine/opcode.go` | Delete 17 `*Tail` opcode constants; delete corresponding 17 `opcodeTable` entries; opcode count drops by 17. Add a new `OperandPromotedCachedBinding` operand kind (signals "Arg high bit = tail flag, low 31 bits = binding index"). Add encoding/decoding helpers. |
| `machine/machine_context.go` | Collapse 34 `Run()` cases to 17; each decodes the tail flag and binding index from `instr.Arg` and dispatches accordingly. |
| `machine/call_promoted.go` | `execPromoted` takes the encoded `Arg` (not separate `tail` parameter); decodes internally. `promotedOpForName` returns a single op + arity (tail-ness moves to encoding). |
| `machine/peephole.go` | Two emission sites: non-tail emits `{Op: promotedOp, Arg: bindingIdx}`; tail emits `{Op: promotedOp, Arg: encodeTail(bindingIdx)}`. |
| `machine/peephole_test.go` | Three test sites previously asserted `OpEqQTail` / `OpVectorQTail`; rewrite to assert the single op plus the tail-flag in Arg. |
| `machine/disassemble.go` | Promoted-op decoding shows binding index + tail status (e.g., `EqQ.tail bindingIdx=5`). |
| `call_promoted.go` guide comment | "ADDING A NEW PROMOTED OP" goes from 3-file edit (opcode.go, machine_context.go, call_promoted.go) to 2-file edit (drop the second case in machine_context.go; drop the second opcode in opcode.go). |

## Encoding

```go
// promotedTailBit is the high bit of instr.Arg, used to encode tail-call
// position for promoted opcodes. Non-promoted ops with OperandCachedBinding
// (e.g. OpCallForeignCached) do not use this bit; they have explicit tail
// variants. See plans/2026-05-11-machine-sr-finding2-impl.md.
const promotedTailBit int32 = -1 << 31 // = math.MinInt32

func encodePromotedArg(bindingIdx int32, tail bool) int32 {
    if tail {
        return bindingIdx | promotedTailBit
    }
    return bindingIdx
}

func decodePromotedArg(arg int32) (bindingIdx int32, tail bool) {
    return arg &^ promotedTailBit, arg < 0
}
```

Binding indexes are non-negative by construction (peephole-assigned), so
the high bit is free. The post-collapse maximum binding index is
`2^31 - 1` = ~2.1B, well above any realistic template.

## Phases

1. **Plan + branch.** Commit this plan file.
2. **Encoding helpers + operand kind.** Add `OperandPromotedCachedBinding`,
   `promotedTailBit`, `encodePromotedArg`, `decodePromotedArg` to
   `opcode.go`. Build verifies.
3. **`execPromoted` signature change.** Drop the `tail bool` parameter;
   take the full instruction's Arg and decode internally. Update
   call sites in `machine_context.go` to pass `instr` instead of
   `instr` + `false`/`true`.
4. **Collapse Run() switch.** Delete 17 `*Tail` cases. Each surviving
   case now decodes tail from `instr.Arg`.
5. **Remove `*Tail` opcode constants + table entries.** Drop the 17
   tail entries from the `OpCode` const block and `opcodeTable`. Adjust
   `opCount` implicitly.
6. **Update `promotedOpForName`.** Return single op + arity; remove the
   tail-op return value. Update peephole emission to wrap tail-emit
   with `encodePromotedArg(bindingIdx, true)`.
7. **Update `peephole_test.go`.** Three sites need both opcode and Arg
   verification. Add helper that decodes `(op, idx, tail)` from
   `Instruction` for cleaner assertions.
8. **Update disassembler.** Promoted-op operand-kind decode should
   render as `bindingIdx + ".tail"` or similar.
9. **Update guide comments.** `call_promoted.go:15-36` "ADDING A NEW
   PROMOTED OP" — 3 edit sites → 2 edit sites.
10. **Verify.** `make lint && make covercheck && make ci` clean.
11. **Bench gate.** Head-to-head against master per the gate. Decide
    based on numbers.

## Risk

- **Hot-path regression.** This is the largest risk and the entire
  reason for the bench gate. Encoding-driven dispatch keeps the
  jump-table form (same case cardinality regime as today's
  non-promoted-op switch — 17 cases is comparable to the 30+ inlined
  cases already in the same switch). But the historical pattern is
  clear: every hot-path change at this layer needs measurement.

- **Disassembler breakage.** `--disasm` output will change for tail
  variants. Acceptable: disassembly is a debug feature, not a public
  API contract. Update the expected-output golden test if one exists.

- **Peephole regression.** Two emission sites must both encode
  correctly. Asymmetry (one site encodes, the other doesn't) would
  silently produce broken bytecode. Mitigated by the existing
  `peephole_test.go` coverage; extend tests if necessary.

## Commit cadence

Per `feedback_commit_cadence.md` (progressive commits):

1. `docs(plans): finding 2 impl plan for tail/non-tail opcode collapse`
2. `feat(machine): add OperandPromotedCachedBinding + tail-flag encoding`
3. `refactor(machine): collapse promoted-op tail/non-tail Run() cases`
4. `refactor(machine): drop *Tail opcode constants and table entries`
5. `test(machine): update peephole tests for collapsed promoted ops`
6. `docs(machine): update ADDING A NEW PROMOTED OP edit-site list`

Each commit builds and passes its own tests independently.
