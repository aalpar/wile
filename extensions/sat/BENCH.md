# SAT Solver Benchmark Baselines

Recorded on commit 1f72cbe0. Hardware: Apple M4 Max. Go version: go1.26.3 darwin/arm64.
Run with: `go test -bench=. -benchmem -run='^$' ./extensions/sat/`.

Regression threshold for PR review: ≥20% slowdown on any benchmark below
flags a code-level perf regression. Investigate before merging.

| Benchmark | Time/op | Allocs/op | Bytes/op |
|---|---|---|---|
| PHP_5 | 460264 ns/op | 661 | 41869 B/op |
| PHP_6 | 4394278 ns/op | 3202 | 240976 B/op |
| PHP_7 | 28043889 ns/op | 18068 | 2006640 B/op |
| Random3SAT_100 | 260653 ns/op | 1563 | 108757 B/op |

Notes:
- PHP_N scales superpolynomially. PHP_7 is the largest practical CI bench;
  PHP_8+ enters hours-long territory without preprocessing.
- Random 3-SAT at ratio 4.26 sits on the phase transition: mix of SAT and
  UNSAT, both classes hard at this density.
