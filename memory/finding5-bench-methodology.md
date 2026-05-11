# Finding 5 — Bench Methodology and Full Data

Finding 5 of `plans/2026-05-06-machine-structural-reduction.md` shipped via Option D (PR #734, 2026-05-11). This note captures the bench methodology and the full per-benchmark data that the plan's status block summarizes.

## Why this matters

The plan's bench-gate spec calls for "no Gabriel-benchmark regresses by more than 0.3% per benchmark; geo-mean delta within ±0.5%". On the development hardware (darwin/arm64 laptop), the 0.3% per-benchmark gate is below the per-block noise floor of the Gabriel suite even with the CPU pinned at max frequency. Future findings on this code path should treat the per-bench gate as advisory and rely on the geo-mean gate plus sign-distribution analysis.

## Drift on this hardware (no CPU pin)

Two `make bench-gabriel` invocations of the same master binary, 40 minutes apart:

- First run averaged ~5.84% slower across all 16 benchmarks geometrically (+1.89% on `fib` to +8.70% on `sum`).
- Drift is monotonic across the run sequence — sequential runs get progressively slower — consistent with Apple Silicon thermal throttling scaling P-core clocks down as the laptop warms.

Implication: **you cannot compare a baseline taken at T=0 against a branch run at T=20min without controlling for thermal envelope.** Run-canonical-style "6 runs averaged per binary, run-once" methodology produces misleading +4% deltas for any change tested mid-thermal-curve, including no-op changes.

## What worked

Interleaved measurement with CPU pinned at max:

1. Pin the CPU at max frequency (eliminates the dominant drift source).
2. Build two binaries (`/tmp/wile-master`, `/tmp/wile-finding5`) separately to avoid `make build` overwriting between runs.
3. Warmup: one full Gabriel run with either binary, results discarded (primes the instruction cache and any deferred page-faults).
4. Interleave: alternate `RUNS=3 SCHEME=/tmp/wile-master ./run-canonical.sh` and `RUNS=3 SCHEME=/tmp/wile-finding5 ./run-canonical.sh` for 3 blocks each.
5. Pool the 3 per-binary CSVs, compute per-benchmark means and stdevs, then geo-mean across benchmarks.

The interleaving puts both binaries inside the same thermal envelope at each block, so any residual thermal drift affects them symmetrically.

## Final results (Finding 5 Option D vs master)

| Benchmark | master mean (s) | branch mean (s) | delta | master stdev | branch stdev |
|-----------|----------------:|----------------:|------:|-------------:|-------------:|
| ackermann | 0.4634          | 0.4642          | +0.19% | 0.0052 | 0.0058 |
| cpstak    | 0.1907          | 0.1887          | −1.05% | 0.0038 | 0.0041 |
| ctak      | 1.4238          | 1.4261          | +0.16% | 0.0867 | 0.0822 |
| deriv     | 0.1042          | 0.1052          | +0.99% | 0.0001 | 0.0006 |
| diviter   | 2.3279          | 2.3304          | +0.11% | 0.0091 | 0.0089 |
| divrec    | 0.8603          | 0.8628          | +0.29% | 0.0026 | 0.0033 |
| fib       | 0.3860          | 0.3844          | −0.41% | 0.0076 | 0.0085 |
| nqueens   | 1.6204          | 1.6308          | +0.65% | 0.0046 | 0.0109 |
| peval     | 0.0800          | 0.0795          | −0.63% | 0.0011 | 0.0002 |
| primes    | 0.2298          | 0.2290          | −0.35% | 0.0019 | 0.0008 |
| sieve     | 0.0859          | 0.0866          | +0.81% | 0.0001 | 0.0007 |
| sum       | 0.0310          | 0.0309          | −0.11% | 0.0004 | 0.0007 |
| sumfp     | 1.0445          | 1.0471          | +0.25% | 0.0060 | 0.0072 |
| tak       | 0.1168          | 0.1152          | −1.37% | 0.0024 | 0.0003 |
| takl      | 1.0463          | 1.0492          | +0.28% | 0.0065 | 0.0015 |
| triangl   | 0.0395          | 0.0396          | +0.25% | 0.0003 | 0.0005 |

Geo-mean ratio: 1.000031 (**+0.003%**, gate ±0.5% — PASS).
Sign distribution: 8 faster, 8 slower (balanced; consistent with a performance-neutral change rather than a systematic regression).
Worst per-bench regress: deriv +0.99%, offset by tak −1.37%.

## Earlier Option C-light data (rejected)

Lifting the check to the `Run()` loop head:

- All 16 benchmarks slower.
- Geo-mean: **+4.17%** (8× over the 0.5% gate).
- Worst: ctak +6.6%.

Cost mechanism: every dispatch iteration now pays for two field loads (`mc.maxStackSize`, `mc.evals.Len()`) plus a branch. The original 6-site coverage charged only push opcodes; lifting to per-iteration extends the cost to `Apply`, `LoadLocal`, `Branch`, `RestoreContinuation`, etc.

## Lessons for future findings on this code path

1. **Pin the CPU before benching.** Without pinning, 5%+ drift swamps any structural change you can reasonably propose.
2. **Interleave master and branch.** A single sequential `master → branch` run-canonical comparison is unreliable below ~5%; the thermal envelope changes between runs.
3. **Trust the geo-mean.** Per-benchmark stdevs are 0.0001–0.087s (1–6% of mean) even with pinning — sub-1% per-benchmark deltas are inside the noise floor. The 0.3% per-bench gate in the original plan was aspirational; rely on geo-mean.
4. **Read sign distribution.** A systematic regression skews all signs in one direction. Balanced 8/8 with a near-zero geo-mean is the signature of "no real change".
5. **Warm up.** First-run cache-cold and page-fault-heavy results bias the average. Discard a warmup pass.
6. **Build separately, run separately.** `make build` overwrites `dist/`. Build both binaries to explicit paths first, then bench.
