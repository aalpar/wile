# Benchmarking Guide

## Running Wile's Canonical Benchmarks

### Quick Run - All Canonical Benchmarks

```bash
# Run only the canonical Gabriel suite benchmarks
for bench in tak takl ctak cpstak fib triangl sum sumfp diviter divrec deriv ackermann sieve nqueens primes peval; do
    echo "=== $bench ==="
    ./dist/wile --file examples/benchmarks/${bench}.scm
    echo ""
done
```

### Individual Benchmark

```bash
./dist/wile --file examples/benchmarks/tak.scm
```

### All Benchmarks (Including Non-Canonical)

```bash
cd examples/benchmarks
./run-all.sh
```

## Comparing Against Other Scheme Implementations

### Option 1: Use the r7rs-benchmarks Suite (Recommended)

The [ecraven/r7rs-benchmarks](https://github.com/ecraven/r7rs-benchmarks) repository provides a standardized benchmarking framework used across the Scheme community.

#### Setup

```bash
# Clone the benchmark suite
cd ~/projects  # or wherever you keep projects
git clone https://github.com/ecraven/r7rs-benchmarks.git
cd r7rs-benchmarks
```

#### Add Wile to the Benchmark Suite

Create `bench-wile`:

```bash
#!/usr/bin/env bash
# bench-wile - Wrapper for Wile Scheme in r7rs-benchmarks

WILE="/path/to/wile/dist/wile"

# The benchmark suite expects: bench-<scheme> <benchmark.scm>
# It provides input via stdin and measures wall-clock time

exec "$WILE" --file "$1"
```

```bash
chmod +x bench-wile
```

#### Run Benchmarks

```bash
# Run a single benchmark on Wile
./bench wile ack

# Run all benchmarks on Wile
./bench wile all

# Compare Wile against other implementations
./bench wile all
./bench chez all
./bench racket all
./bench chibi all
./bench guile all

# Results are written to results.wile, results.chez, etc.
```

#### Analyze Results

```bash
# Generate CSV with all results
./summarize

# View results
cat all.csv | column -t -s,
```

The CSV format is: `<scheme>,<benchmark>,<seconds>`

### Option 2: Manual Comparison

If you don't want to set up the full r7rs-benchmarks suite:

#### 1. Install Other Scheme Implementations

```bash
# macOS
brew install chezscheme racket chibi-scheme guile

# Ubuntu/Debian
apt install chezscheme racket chibi-scheme guile-3.0

# Fedora/RHEL
dnf install chezscheme racket chibi-scheme guile
```

#### 2. Run the Same Benchmark on Each

**tak.scm benchmark:**

```bash
# Wile
time ./dist/wile --file examples/benchmarks/tak.scm

# Chez Scheme
time scheme --script examples/benchmarks/tak.scm

# Racket
time racket examples/benchmarks/tak.scm

# Chibi Scheme
time chibi-scheme examples/benchmarks/tak.scm

# Guile
time guile examples/benchmarks/tak.scm
```

**Note:** Some implementations may require modifications to the benchmark files (imports, syntax). The r7rs-benchmarks suite handles this automatically via prelude files.

#### 3. Create a Comparison Script

```bash
#!/usr/bin/env bash
# compare-benchmarks.sh

BENCH="tak"

echo "Benchmark: $BENCH"
echo "========================"

echo -n "Wile:   "
time ./dist/wile --file examples/benchmarks/${BENCH}.scm 2>&1 | grep "Total time"

echo -n "Chez:   "
time scheme --script examples/benchmarks/${BENCH}.scm 2>&1 | grep "Total time"

echo -n "Racket: "
time racket examples/benchmarks/${BENCH}.scm 2>&1 | grep "Total time"

echo -n "Chibi:  "
time chibi-scheme examples/benchmarks/${BENCH}.scm 2>&1 | grep "Total time"
```

### Option 3: Larceny Benchmark Results

The Larceny project maintains historical benchmark results at:
- http://www.larcenists.org/benchmarksAboutR7.html

You can compare Wile's numbers against the published results for other implementations.

## Canonical Benchmark List

For cross-implementation comparison, use **only these benchmarks**:

| Benchmark | Description | Expected Time (M4 Max) |
|-----------|-------------|------------------------|
| `tak.scm` | Takeuchi function | ~0.5s |
| `takl.scm` | Takeuchi with lists | ~2s |
| `ctak.scm` | Continuation-based Takeuchi | ~5s |
| `cpstak.scm` | CPS Takeuchi | ~1s |
| `fib.scm` | Fibonacci | ~0.3s |
| `triangl.scm` | Double recursion | ~0.3s |
| `sum.scm` | Recursive sum | ~5s |
| `sumfp.scm` | Floating-point sum | ~1s |
| `diviter.scm` | Iterative division | ~1s |
| `divrec.scm` | Recursive division | ~1s |
| `deriv.scm` | Symbolic differentiation | ~0.7s |
| `ackermann.scm` | Ackermann function | ~2s |
| `sieve.scm` | Sieve of Eratosthenes | ~5s |
| `nqueens.scm` | N-Queens puzzle | ~3s |
| `primes.scm` | Prime generation | ~2s |
| `peval.scm` | Partial evaluation | ~0.2s |

Times are approximate and vary by hardware/implementation.

## Interpreting Results

### Performance Philosophy

Wile is a bytecode interpreter optimized for correctness, simplicity, and embeddability.

### Expected Performance Tier

Wile is a **bytecode interpreter**. When comparing against native-code compilers:

```
┌─────────────────────────────────┬────────────┐
│ Native Code Compilers           │  1x        │ (Chez Scheme, Gambit)
├─────────────────────────────────┼────────────┤
│ JIT Compilers                   │  5-10x     │ (Racket with JIT)
├─────────────────────────────────┼────────────┤
│ Bytecode Interpreters           │  50-2000x  │ (Wile, Guile, Chibi)
├─────────────────────────────────┼────────────┤
│ Tree-Walking Interpreters       │  100-5000x │
└─────────────────────────────────┴────────────┘
```

**Wile at 1,000-2,000x slower than Chez is completely normal.** This is the architectural tradeoff of bytecode interpretation vs. native compilation. It's like comparing Python to C - different tools for different jobs.

### Real-World Example

```
Benchmark: tak(18, 12, 6) × 10 iterations

Chez Scheme:    0.00073s  (native code)
Wile:           1.15500s  (bytecode)
Ratio:          ~1,580x slower

This is expected and acceptable for Wile's use cases.
```

### What Matters

1. **Correctness** - Results must match expected output
2. **Consistency** - Relative performance across benchmarks should be reasonable
3. **No Regressions** - Track Wile's performance over time (is v1.3 slower than v1.2?)
4. **Predictability** - Understand which operations are fast/slow

### What Doesn't Matter

1. **Absolute speed vs. native compilers** - Wile is designed for embedding, not compute-intensive workloads
2. **Microbenchmark noise** - Sub-second variations are usually system noise
3. **Unrealistic workloads** - Real use cases are configuration/scripting, not tight numeric loops
4. **Beating other implementations** - Cross-implementation comparison is for understanding position, not competition

### When Performance Matters

For compute-intensive inner loops, use Wile as a control/configuration layer with performance-critical code in Go via the FFI. The embedding API (`RegisterFunc`) makes this seamless.

## Tracking Wile's Performance Over Time

### Baseline Results

Create a baseline for your current version:

```bash
# Generate baseline
for bench in tak fib ack deriv sieve; do
    echo -n "$bench,"
    ./dist/wile --file examples/benchmarks/${bench}.scm 2>&1 | \
        grep "Total time" | awk '{print $3}' | tr -d 's'
done > benchmarks-baseline.csv
```

### Compare After Changes

```bash
# After making changes, generate new results
for bench in tak fib ack deriv sieve; do
    echo -n "$bench,"
    ./dist/wile --file examples/benchmarks/${bench}.scm 2>&1 | \
        grep "Total time" | awk '{print $3}' | tr -d 's'
done > benchmarks-current.csv

# Compare
paste benchmarks-baseline.csv benchmarks-current.csv | \
    awk -F, '{
        printf "%-15s baseline: %6.3fs  current: %6.3fs  change: %+6.1f%%\n",
               $1, $2, $3, (($3-$2)/$2)*100
    }'
```

### CI Integration

Add to `.github/workflows/benchmark.yml`:

```yaml
name: Benchmark
on: [push, pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-go@v4
        with:
          go-version: '1.21'

      - name: Build
        run: make build

      - name: Run Benchmarks
        run: |
          for bench in tak fib ack deriv sieve; do
            echo "=== $bench ==="
            ./dist/wile --file examples/benchmarks/${bench}.scm
          done

      - name: Check for Regressions
        run: |
          # Compare against stored baseline
          # Fail if any benchmark regresses >20%
          ./scripts/check-benchmark-regression.sh
```

## Benchmark-Driven Development

### Use Case: Optimizing Symbol Lookup

```bash
# 1. Identify slow benchmark
./dist/wile --file examples/benchmarks/deriv.scm
# "Total time: 0.8s" - this does a lot of symbol lookups

# 2. Make optimization to environment/binding.go

# 3. Rebuild and re-run
make build
./dist/wile --file examples/benchmarks/deriv.scm
# "Total time: 0.6s" - 25% improvement!

# 4. Run full suite to check for regressions
cd examples/benchmarks && ./run-all.sh
```

### Use Case: Testing a New Compiler Optimization

```bash
# Run before optimization
for b in tak fib triangl; do
    echo -n "$b: "
    ./dist/wile --file examples/benchmarks/$b.scm 2>&1 | grep "Total time"
done > before.txt

# Apply optimization, rebuild
# ...

# Run after optimization
for b in tak fib triangl; do
    echo -n "$b: "
    ./dist/wile --file examples/benchmarks/$b.scm 2>&1 | grep "Total time"
done > after.txt

# Compare
diff before.txt after.txt
```

## Common Pitfalls

### 1. System Noise

Run benchmarks multiple times and average:

```bash
for i in {1..5}; do
    ./dist/wile --file examples/benchmarks/tak.scm
done | grep "Total time" | awk '{sum+=$3; n++} END {print "Average:", sum/n}'
```

### 2. Thermal Throttling

Long benchmark runs can cause CPU throttling:

```bash
# Monitor CPU frequency during benchmarks
while true; do
    sysctl -n machdep.cpu.frequency 2>/dev/null || \
    cat /proc/cpuinfo | grep MHz | head -1
    sleep 1
done &

./run-all.sh
kill %1
```

### 3. Background Processes

Close unnecessary apps, especially browsers:

```bash
# Check CPU usage before benchmarking
top -l 1 -n 10 -o cpu
```

### 4. Cold vs. Warm Runs

First run is often slower due to disk cache, binary loading:

```bash
# Warmup run (discard results)
./dist/wile --file examples/benchmarks/tak.scm > /dev/null 2>&1

# Actual benchmark runs
for i in {1..3}; do
    ./dist/wile --file examples/benchmarks/tak.scm
done
```

## Further Reading

- [Gabriel Benchmark Suite](https://www.cs.utah.edu/~mflatt/benchmarks-20100126/) - Original suite
- [Larceny Benchmarks](http://www.larcenists.org/benchmarksAboutR7.html) - R7RS results
- [r7rs-benchmarks](https://github.com/ecraven/r7rs-benchmarks) - Standardized framework
- [Gambit Benchmarks](https://github.com/gambit/gambit/tree/master/bench) - Additional suite
- [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/) - Cross-language comparisons
