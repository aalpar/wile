# Scheme Examples Plan

## Goal
Create showcase examples and benchmarks demonstrating all Wile features.

## Directory Structure

```
examples/
├── README.md                    # Index with descriptions
├── basics/                      # Core language features
│   ├── hello.scm               # First program
│   ├── recursion.scm           # Factorial, fibonacci
│   ├── higher-order.scm        # map, filter, fold patterns
│   └── closures.scm            # Lexical scoping, closures
├── numeric-tower/              # Full numeric capabilities
│   ├── exactness.scm           # Exact vs inexact arithmetic
│   ├── rationals.scm           # Rational number operations
│   ├── complex.scm             # Complex number math
│   ├── big-numbers.scm         # BigInteger, BigFloat
│   └── mixed-arithmetic.scm    # Cross-type operations
├── macros/                     # Hygienic macro system
│   ├── simple-macros.scm       # Basic syntax-rules patterns
│   ├── hygiene.scm             # Variable capture prevention
│   ├── anaphoric.scm           # Breaking hygiene intentionally
│   └── dsl.scm                 # Domain-specific language example
├── control/                    # Control flow features
│   ├── continuations.scm       # call/cc examples
│   ├── dynamic-wind.scm        # Resource cleanup patterns
│   ├── exceptions.scm          # guard, raise, with-exception-handler
│   ├── generators.scm          # Generators via continuations
│   └── coroutines.scm          # Cooperative multitasking
├── concurrency/                # Threading and Go primitives
│   ├── threads.scm             # SRFI-18 basic threading
│   ├── mutex.scm               # Mutual exclusion patterns
│   ├── channels.scm            # Go-style channel messaging
│   ├── producers-consumers.scm # Classic concurrency pattern
│   └── parallel-map.scm        # Parallel computation
├── data-structures/            # Records, vectors, etc.
│   ├── records.scm             # define-record-type usage
│   ├── association-lists.scm   # alist patterns
│   ├── vectors.scm             # Vector operations
│   └── lazy-streams.scm        # Infinite streams with delay/force
├── io/                         # Input/output
│   ├── file-io.scm             # Reading and writing files
│   ├── string-ports.scm        # String I/O
│   └── binary-io.scm           # Bytevector I/O
└── benchmarks/                 # Performance testing
    ├── tak.scm                 # Takeuchi function (recursion)
    ├── fib.scm                 # Fibonacci (naive vs memoized)
    ├── primes.scm              # Prime sieve
    ├── sort.scm                # Sorting algorithms
    ├── matrix.scm              # Matrix multiplication
    ├── gc-stress.scm           # Allocation-heavy workload
    └── channel-throughput.scm  # Channel message passing speed
```

## Example Template

Each example file should follow this structure:

```scheme
;;; example-name.scm - Brief description
;;;
;;; Demonstrates: feature1, feature2
;;; Wile-specific: any non-standard features used
;;;
;;; Usage: ./dist/scheme --file examples/category/example-name.scm

;; Explanation of the concept being demonstrated
;; ...

;; Code with inline comments explaining key points
(define (example-function args)
  ...)

;; Interactive examples that print results
(display "Expected: ...")
(newline)
(display "Result: ")
(display (example-function test-input))
(newline)
```

## Priority Examples (Phase 1)

High-impact examples to create first:

| File | Why Important |
|------|---------------|
| `basics/hello.scm` | First example users see |
| `numeric-tower/mixed-arithmetic.scm` | Unique feature, complex implementation |
| `macros/hygiene.scm` | Core differentiator (Flatt 2016 model) |
| `control/continuations.scm` | Advanced feature showcase |
| `concurrency/channels.scm` | Go-native extension |
| `benchmarks/tak.scm` | Classic Lisp benchmark |
| `benchmarks/fib.scm` | Simple, comparable to other implementations |

## Benchmark Format

Benchmarks should:
1. Print timing information using `current-jiffy` and `jiffies-per-second`
2. Include a warmup run
3. Report iterations/second or time per operation
4. Be runnable standalone

```scheme
;;; benchmark-name.scm - Description
;;;
;;; Benchmark for: feature being tested

(define (run-benchmark iterations)
  (let ((start (current-jiffy)))
    ;; ... benchmark code ...
    (let ((end (current-jiffy)))
      (/ (- end start) (jiffies-per-second)))))

;; Warmup
(run-benchmark 1000)

;; Actual benchmark
(let ((time (run-benchmark 100000)))
  (display "Time: ")
  (display time)
  (display " seconds")
  (newline))
```

## Files to Create

### Phase 1: Core Examples (7 files)
1. `examples/README.md`
2. `examples/basics/hello.scm`
3. `examples/numeric-tower/mixed-arithmetic.scm`
4. `examples/macros/hygiene.scm`
5. `examples/control/continuations.scm`
6. `examples/concurrency/channels.scm`
7. `examples/benchmarks/tak.scm`

### Phase 2: Complete Coverage (remaining ~25 files)
All remaining files from the directory structure above.

## Verification

After creating examples:
1. Run each example: `./dist/scheme --file examples/<path>.scm`
2. Verify output is correct
3. For benchmarks, ensure timing output is reasonable
4. Update `examples/README.md` with any issues or prerequisites

## Notes

- All examples should work with the current Wile implementation
- No external dependencies beyond standard libraries
- Each example should be self-contained and runnable
- Benchmarks should complete in <30 seconds for CI friendliness
