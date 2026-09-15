# Quick Start: Running Benchmarks

## Run All Canonical Benchmarks

From the `examples/benchmarks` directory:

```bash
cd examples/benchmarks
./run-canonical.sh
```

This runs all 16 canonical Gabriel benchmarks and saves results to a timestamped CSV file.

## Compare Against Other Schemes

Install other Scheme implementations:

```bash
# macOS
brew install chezscheme

# Ubuntu/Debian
sudo apt install chezscheme racket chibi-scheme

# Fedora/RHEL
sudo dnf install chezscheme racket chibi-scheme
```

Then run comparison:

```bash
cd examples/benchmarks
./compare-schemes.sh
```

By default, this runs a few quick benchmarks (tak, fib, deriv, peval) on all installed Schemes.

Chez (`scheme`) and Racket each load a small prelude defining `current-jiffy` and `jiffies-per-second` before the benchmark. Benchmarks that import libraries or `include` Wile sources (`kanren-benchmark`, `parallel-matrix-mul`, `schelog-zebra-bench`, `bench-*`) report FAILED outside Wile.

To compare more benchmarks:

```bash
BENCHMARKS="tak fib ackermann deriv sieve nqueens" ./compare-schemes.sh
```

## Run Individual Benchmark

```bash
cd examples/benchmarks
../../dist/wile --file tak.scm
```

Or from the repository root:

```bash
./dist/wile --file examples/benchmarks/tak.scm
```

## Interpreting Output

Each benchmark prints:

```
=== Benchmark Name ===

Benchmark: description
Iterations: 10
Total time: 1.234s
Per iteration: 0.1234s
```

The **Total time** is what matters for comparison.

## Expected Performance

Wile is a bytecode interpreter. On `tak(18, 12, 6)` Wile measures roughly 180× slower than Chez Scheme and Racket CS; the table and its caveats are in [BENCHMARKING.md](BENCHMARKING.md#expected-performance). For compute-intensive inner loops, use Wile as a control layer with performance-critical code in Go via the FFI.

## Tracking Performance Over Time

Create a baseline:

```bash
cd examples/benchmarks
./run-canonical.sh
cp canonical-results-*.csv baseline.csv
```

After making changes:

```bash
./run-canonical.sh
# Compare the new canonical-results-*.csv against baseline.csv
```

## Detailed Comparison Guide

See `BENCHMARKING.md` for comprehensive information on:
- Cross-implementation comparison methodology
- Using the r7rs-benchmarks suite
- CI integration
- Performance analysis techniques
