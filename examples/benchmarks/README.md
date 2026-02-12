# Wile Scheme Benchmarks

Standard Scheme benchmarks for comparing performance across implementations.

## Overview

These benchmarks are based on the classic Gabriel benchmark suite and other
standard Scheme tests. Each benchmark is a standalone `.scm` file that prints
timing information when run.

**Note on R6RS vs R7RS:** Some benchmarks in the canonical Gabriel suite use
R6RS procedure signatures (e.g., `error` with a `who` parameter). Wile is a
pure R7RS implementation. For benchmarks that need R6RS compatibility, load
the compatibility shim:

```scheme
(load "examples/lib/r6rs-compat.scm")
;; Now R6RS signatures work
```

This is currently not required for any benchmarks in this directory, as they
have been adapted to R7RS signatures.

## Running Benchmarks

Run a single benchmark:

```bash
./dist/scheme --file examples/benchmarks/tak.scm
```

Run all benchmarks:

```bash
for bench in examples/benchmarks/*.scm; do
    [ "$bench" = "examples/benchmarks/README.md" ] && continue
    echo "Running $(basename $bench)..."
    ./dist/scheme --file "$bench"
    echo ""
done
```

## Available Benchmarks

### Gabriel Benchmark Suite

The core benchmarks from Richard Gabriel's classic Lisp/Scheme benchmark suite:

| Benchmark | Description | Stresses |
|-----------|-------------|----------|
| `tak.scm` | Takeuchi function | Function call overhead, recursion |
| `takl.scm` | Takeuchi with lists | List allocation, GC pressure |
| `ctak.scm` | Continuation-based Takeuchi | call/cc, continuation capture |
| `cpstak.scm` | CPS Takeuchi | Closures, tail-call optimization |
| `fib.scm` | Naive Fibonacci | Doubly-recursive calls, stack management |
| `triangl.scm` | Double-recursive triangle | Tree-shaped recursion, memory allocation |
| `sum.scm` | Recursive summation | Basic recursion, integer arithmetic |
| `sumloop.scm` | Iterative summation | Tail recursion, loop optimization |
| `sumfp.scm` | Floating-point sum | Numeric tower, floating-point ops |
| `diviter.scm` | Iterative division | Tight loops, arithmetic |
| `divrec.scm` | Recursive division | Tail recursion with division |
| `deriv.scm` | Symbolic differentiation | List manipulation, symbolic computation |
| `destruct.scm` | List destructuring | Cons cell allocation, list operations |
| `browse.scm` | Tree browsing | Tree traversal, GC stress |
| `puzzle.scm` | Combinatorial puzzle | Backtracking, list manipulation |

### Additional Benchmarks

| Benchmark | Description | Stresses |
|-----------|-------------|----------|
| `ackermann.scm` | Ackermann function | Deep recursion, stack depth |
| `sieve.scm` | Sieve of Eratosthenes | List processing, filtering |
| `primes.scm` | Prime generation (trial division) | Integer arithmetic, simple algorithms |
| `nqueens.scm` | N-Queens puzzle | Backtracking, constraint satisfaction |
| `peval.scm` | Partial evaluation | Higher-order functions, composition |

## Performance Expectations

**Wile is a bytecode interpreter optimized for correctness, simplicity, and embeddability - not raw speed.**

Performance is explicitly deprioritized in Wile's design. The target workloads (configuration, policy evaluation, data transformation, scripting) are not bottlenecked on interpreter speed. For compute-intensive inner loops, use Wile as a control layer with performance-critical code in Go via the FFI.

### Expected Performance vs. Native Compilers

When comparing against native-code compilers like Chez Scheme:

- **Chez Scheme:** Compiles to native x86/ARM machine code with aggressive optimizations
- **Wile:** Compiles to bytecode, interprets on a virtual machine
- **Expected ratio:** 1,000-2,000x slower than Chez

**This is completely normal and expected.** It's the architectural tradeoff of a bytecode interpreter vs. a native compiler - like comparing Python to C.

### Performance Tier Positioning

```
┌─────────────────────────────────┬──────────┐
│ Native Code (Chez, Gambit)      │   1x     │
├─────────────────────────────────┼──────────┤
│ JIT (Racket with JIT)           │   5-10x  │
├─────────────────────────────────┼──────────┤
│ Bytecode (Wile, Guile, Chibi)   │  50-500x │ ← Wile is here
├─────────────────────────────────┼──────────┤
│ Tree-walking interpreters       │ 100-1000x│
└─────────────────────────────────┴──────────┘
```

### What Matters for Wile

1. **Correctness** - Results match R7RS specification
2. **Consistency** - No unexpected performance cliffs
3. **Tracking over time** - Did this change regress performance?
4. **Predictability** - Understanding which operations are fast/slow

**Cross-implementation comparison is useful for understanding Wile's position, not for competition.** The goal is not to beat native compilers - that's not the product.

For compute-intensive workloads, use Wile as a control/configuration layer
with performance-critical code in Go via the FFI.

## Comparison to Other Schemes

Approximate performance on Apple M4 Max (times are illustrative):

| Benchmark | Wile | Chez Scheme | Racket | Chibi Scheme |
|-----------|------|-------------|--------|--------------|
| tak(18,12,6) | ~0.5s | ~0.02s | ~0.1s | ~1.5s |
| fib(25) | ~0.3s | ~0.01s | ~0.05s | ~0.8s |
| ackermann(3,9) | ~2.0s | ~0.05s | ~0.3s | ~5.0s |

*Times are approximate and will vary by hardware and implementation version.*

## Benchmark Status

### Canonical Gabriel Suite Benchmarks

These match the canonical implementations from the Larceny/r7rs-benchmarks project:

- tak.scm, takl.scm, ctak.scm, cpstak.scm
- fib.scm, triangl.scm
- sum.scm, sumfp.scm, diviter.scm, divrec.scm
- deriv.scm (R7RS error signature)
- ackermann.scm, sieve.scm, nqueens.scm
- primes.scm, peval.scm

### Non-Canonical Benchmarks

These were created for this repository and may differ from canonical versions:

- browse.scm - Simplified tree traversal (canonical version has AI database pattern matching)
- destruct.scm - Functional list operations (canonical "destruc.scm" uses set-car!/set-cdr!)
- puzzle.scm - Simple list permutations (canonical has complex 3D puzzle placement backtracking)
- sumloop.scm - Not in canonical suite

**For benchmark comparisons with other Schemes**, use only the canonical benchmarks above.
The non-canonical versions test Wile's performance but aren't comparable across implementations.

## Adding New Benchmarks

Follow this template:

```scheme
;;; benchmark-name.scm - Brief description
;;;
;;; Detailed description of what this benchmarks.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/benchmark-name.scm

(define (your-benchmark-function args)
  ...)

(define (run-benchmark iterations args)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (your-benchmark-function args)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (/ (- end start) (jiffies-per-second))))
      (display "Benchmark: your-benchmark(...)\n")
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (/ elapsed iterations))
      (display "s\n")
      elapsed)))

;; Warmup
(your-benchmark-function test-input)

;; Benchmark
(display "=== Your Benchmark Name ===\n\n")
(run-benchmark 10 test-input)
```

Key requirements:
- Use `current-jiffy` and `jiffies-per-second` for timing
- Include a warmup run
- Print clear output with benchmark name, iterations, and time
- Make the benchmark repeatable and deterministic
- Complete in reasonable time (< 30 seconds for CI)
