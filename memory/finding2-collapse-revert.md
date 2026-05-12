# Finding 2 — Promoted-Op Collapse: Bench Data and Revert Rationale

Finding 2 of `plans/2026-05-06-machine-structural-reduction.md` was prototyped
on `feat/machine-sr-finding2` (closed PR #737) and **declined** after the
project's bench gate failed decisively. This note captures the full data
and the cost-model lesson so the experiment is not re-run blindly.

## The proposal

Collapse the 17 promoted-op tail/non-tail opcode pairs (34 opcodes, 34
dispatch cases in `Run()`) to 17 single opcodes by encoding tail-ness in
the high bit of `Instruction.Arg`. Helpers:

```go
const promotedTailBit int32 = -1 << 31

func encodePromotedArg(idx int32, tail bool) int32  // peephole-side
func decodePromotedArg(arg int32) (idx int32, tail bool) // Run-side
```

Each surviving `case OpEqQ:` handled both tail and non-tail positions;
`execPromoted` lost its `tail bool` parameter and decoded the flag from
`instr.Arg`.

## The hypothesis

From the parent plan:
> The benchmark rejected *table-driven dispatch* (loading function
> pointers from an array), not *encoding-driven dispatch*. The two are
> different. ... The `tail bool` parameter to `execPromoted` already
> exists — only the case label and the `bool` literal change.

The claim was that the surviving 17-case `switch` would still compile to
a jump table (same cardinality regime), and the per-call decode would
cost one predictable branch.

## The bench gate (head-to-head vs master)

**Methodology**: 10-run interleaved, separate binary paths, fresh build
of both binaries with `--version` SHA verified to match HEAD before
each run. (An earlier run produced misleading "no regression" results
because `make build` had silently reused a cached binary from before
the collapse commit — the "branch" binary was actually the plan-commit
binary, identical to master modulo the plan file.)

`taskpolicy -c utility` was tried initially but rejected per the
project methodology in `memory/finding5-bench-methodology.md`: `-c
utility` clamps to E-cores, which is the **opposite** of what the
bench discipline calls for (pin at max frequency). Final numbers used
no wrapper.

Min-of-10 per benchmark:

| bench | master min (s) | branch min (s) | Δ% |
|---|---:|---:|---:|
| tak       | 0.1131 | 0.1147 | +1.4% |
| takl      | 1.0217 | 1.0558 | +3.3% |
| ctak      | 1.3320 | 1.3417 | +0.7% |
| cpstak    | 0.1827 | 0.1861 | +1.9% |
| fib       | 0.3640 | 0.3793 | +4.2% |
| triangl   | 0.0381 | 0.0402 | +5.5% |
| sum       | 0.0301 | 0.0303 | +0.7% |
| sumfp     | 1.0135 | 1.0597 | +4.6% |
| diviter   | 2.2745 | 2.3564 | +3.6% |
| divrec    | 0.8586 | 0.8692 | +1.2% |
| deriv     | 0.1045 | 0.1054 | +0.9% |
| ackermann | 0.4587 | 0.4614 | +0.6% |
| sieve     | 0.0844 | 0.0876 | +3.8% |
| nqueens   | 1.6034 | 1.6491 | +2.9% |
| primes    | 0.2234 | 0.2310 | +3.4% |
| peval     | 0.0784 | 0.0800 | +2.0% |

**Geomean: +2.5%** (5× over the ±0.5% gate). **Sign distribution: 16
slower, 0 faster.** The all-positive direction across every benchmark
is the strongest signal: even when individual per-bench deltas sit
near each benchmark's measurement noise, the uniform direction
indicates a real underlying effect, not random variance.

## Cost model — why the hypothesis failed

The parent plan distinguished table-driven dispatch (rejected, 1.5%
regression) from encoding-driven dispatch (this proposal) on the
assumption that the dispatch *structure* was what mattered. The actual
mechanism that paid the bill was different: **loss of compiler
specialization at the call site**.

Pre-collapse, each promoted-op case looked like:

```go
case OpEqQ:
    mc, err = execPromoted(mc, instr, "eq?", 2, /*tail=*/false, inlineEq)
case OpEqQTail:
    mc, err = execPromoted(mc, instr, "eq?", 2, /*tail=*/true,  inlineEq)
```

The literal `false`/`true` arguments let the Go compiler:

- Constant-fold the `if tail { ... } else { ... }` branch inside
  `execPromoted` at each call site (or specialize via inlining).
- Eliminate the tail/non-tail epilogue branch entirely per call site.
- Treat the two cases as effectively unrolled into two distinct
  functions at the IR level.

Post-collapse, the single case looked like:

```go
case OpEqQ:
    mc, err = execPromoted(mc, instr, "eq?", 2, inlineEq)
```

…where `execPromoted` itself decoded `bindingIdx, tail := decodePromotedArg(instr.Arg)`
and branched on the decoded `tail`. The compiler can no longer
specialize: every call site shares the same `execPromoted` body, which
must run the runtime decode and branch.

The decode is small (2-3 ALU ops). The branch is per-call-site
predictable, so misprediction cost is near zero. The cost that actually
showed up was the loss of code locality: the pre-collapse form let the
compiler generate two specialized inlined bodies; the post-collapse
form forces a single shared body that the compiler can no longer
specialize per call site. The net effect is **roughly +0.5%–5% per
benchmark**, scaling with how dispatch-bound the benchmark is.

## The lesson (file under "hand-unrolled loops with literal control-flow args")

When a hand-unrolled `switch` has cases that differ only in a literal
argument to a callee (a `bool` flag, a small int), the unroll is doing
more than structural sugaring — it's giving the compiler N specialized
call sites it can fold the literal into. Substituting "encoded payload"
for "compile-time literal" looks structurally equivalent but defeats
the specialization. The cost model is **compiler IR**, not opcode-table
cardinality.

When the analyzed pattern *looks like* a hand-unrolled loop, check the
literal-folding angle before claiming it's a clean reduction:

- If the callee branches on a per-case literal, the unroll is
  load-bearing for the optimizer.
- A "decode-from-payload" replacement is functionally equivalent but
  semantically more general — and the optimizer can't recover the
  per-call specialization without inlining hints (`//go:noinline`-free
  small leaf functions don't reliably help here).
- Bench is the only honest signal.

## Future direction

This finding is closed. If anyone re-opens it, they should:

1. Read this file first.
2. Build the prototype with `//go:nosplit` and/or aggressive inline
   hints on `execPromoted` to see if specialization can be coaxed back.
3. Measure with at least 10 interleaved pinned runs per binary.
4. Apply the parent-plan gate (geomean ±0.5%, per-bench ±0.3%) strictly.

Closing PR #737 leaves no code on master. The impl plan
(`plans/2026-05-11-machine-sr-finding2-impl.md` on the closed branch)
was deleted in this revert; this memory file is the canonical record.
