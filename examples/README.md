# Wile Scheme Examples

Comprehensive examples demonstrating Wile's capabilities as a full R7RS Scheme with hygienic macros, Go interoperability, and advanced features.

## Quick Start

```bash
# Run any example
./dist/wile --file examples/basics/hello.scm

# Build the interpreter first if needed
make build
```

## Example Categories

### Basics

Foundational Scheme concepts for newcomers.

| File | Description |
|------|-------------|
| [hello.scm](basics/hello.scm) | First program - output, strings, basic syntax |
| [recursion.scm](basics/recursion.scm) | Recursive vs tail-recursive functions |
| [higher-order.scm](basics/higher-order.scm) | map, filter, fold, function composition |
| [closures.scm](basics/closures.scm) | Lexical scoping, private state, data abstraction |
| [error-source-tracking.scm](basics/error-source-tracking.scm) | Source location tracking in errors |
| [meta-eval.scm](basics/meta-eval.scm) | Meta-circular evaluator |

### Numeric Tower

Full numeric tower with exact/inexact distinction, rationals, complex, and arbitrary precision.

| File | Description |
|------|-------------|
| [exactness.scm](numeric-tower/exactness.scm) | Exact vs inexact arithmetic, contagion rules, conversions |
| [rationals.scm](numeric-tower/rationals.scm) | Rational arithmetic, Egyptian fractions, continued fractions |
| [complex.scm](numeric-tower/complex.scm) | Complex numbers, polar/rectangular forms, Mandelbrot set |
| [big-numbers.scm](numeric-tower/big-numbers.scm) | Arbitrary precision - BigInteger, large exact computations |
| [mixed-arithmetic.scm](numeric-tower/mixed-arithmetic.scm) | Cross-type operations, type promotion, exactness contagion |
| [symbolic-diff.scm](numeric-tower/symbolic-diff.scm) | Symbolic differentiation using exact arithmetic |

**Key Feature**: Wile implements the full R7RS numeric tower including exact rationals, complex numbers, and automatic exactness preservation. Operations like `(/ 1 3)` produce exact rationals, not floating-point approximations.

### Macros

Hygienic macro system using "sets of scopes" (Flatt 2016) — the same model as Racket.

| File | Description |
|------|-------------|
| [simple-macros.scm](macros/simple-macros.scm) | Basic syntax-rules patterns - when, or, let*, repeat, swap |
| [hygiene.scm](macros/hygiene.scm) | Variable capture prevention, scope isolation demonstrations |
| [anaphoric.scm](macros/anaphoric.scm) | Controlled hygiene breaking - aif, awhen, acond patterns |
| [state-machine.scm](macros/state-machine.scm) | DSL for state machines via macros |

**Key Feature**: Wile's macro system prevents variable capture automatically while allowing intentional hygiene-breaking when needed. Macros expand in a separate phase and can generate code that itself uses macros.

### Control Flow

First-class continuations, exceptions, and control operators.

| File | Description |
|------|-------------|
| [continuations.scm](control/continuations.scm) | call/cc fundamentals - early return, backtracking, generators |
| [dynamic-wind.scm](control/dynamic-wind.scm) | Resource cleanup with before/after guards, continuation-safe |
| [exceptions.scm](control/exceptions.scm) | guard, raise, with-exception-handler, error objects |
| [amb.scm](control/amb.scm) | Non-deterministic computation with backtracking |
| [coroutines.scm](control/coroutines.scm) | Cooperative multitasking via continuations |
| [generators.scm](control/generators.scm) | Generators using call/cc |

**Key Feature**: Full `call/cc` support enables advanced control flow patterns. Continuations are first-class values that can be invoked multiple times.

### Data Structures

Records, lazy evaluation, and functional data structures.

| File | Description |
|------|-------------|
| [records.scm](data-structures/records.scm) | define-record-type, constructors, accessors, predicates |
| [association-lists.scm](data-structures/association-lists.scm) | Alist patterns - assoc, updates, merging, caching |
| [vectors.scm](data-structures/vectors.scm) | Random access, mutation, matrices, circular buffers |
| [lazy-streams.scm](data-structures/lazy-streams.scm) | Infinite streams with delay/force |
| [unification.scm](data-structures/unification.scm) | Unification algorithm for pattern matching |

### I/O

File operations, string ports, and binary I/O.

| File | Description |
|------|-------------|
| [file-io.scm](io/file-io.scm) | Reading and writing files, line-by-line processing |
| [string-ports.scm](io/string-ports.scm) | In-memory I/O with string ports |
| [binary-io.scm](io/binary-io.scm) | Bytevector operations and binary file I/O |

### Concurrency

Threading, synchronization, and message passing backed by Go's runtime.

| File | Description |
|------|-------------|
| [threads.scm](concurrency/threads.scm) | SRFI-18 basic threading - creation, joining, state |
| [mutex.scm](concurrency/mutex.scm) | Mutual exclusion, critical sections, race prevention |
| [channels.scm](concurrency/channels.scm) | Go-style channel messaging and pipelines |
| [producers-consumers.scm](concurrency/producers-consumers.scm) | Classic work queue pattern |
| [parallel-map.scm](concurrency/parallel-map.scm) | Parallel computation and thread pools |

**Key Feature**: Wile threads map directly to Go goroutines. Channels provide CSP-style message passing. Mutexes prevent race conditions on shared state.

### Applications

Real-world use cases and domain-specific applications.

| File | Description |
|------|-------------|
| [parser-combinators.scm](applications/parser-combinators.scm) | Parser combinator library |
| [rule-engine.scm](applications/rule-engine.scm) | Rule-based inference engine |

### Logic Programming

Embedded logic programming via Schelog (Scheme + Prolog).

| Directory | Description |
|-----------|-------------|
| [logic/schelog/](logic/schelog/) | Schelog logic programming system |

**Key Feature**: Schelog brings Prolog-style logic programming to Scheme, demonstrating Wile's extensibility and macro capabilities.

### Embedding

Examples of embedding Wile in Go applications.

| File | Description |
|------|-------------|
| [embedding/basic.go](embedding/basic.go) | Basic Go embedding - creating engines, evaluating code |
| [embedding/source-tracking/](embedding/source-tracking/) | Source location tracking integration |

**Key Feature**: Pure Go implementation means no CGo, no C toolchain, no cross-compilation headaches. Add Wile as a `go get` dependency and start embedding.

### Benchmarks

Standard Scheme benchmarks from the Gabriel suite and others.

| File | Description |
|------|-------------|
| [benchmarks/README.md](benchmarks/README.md) | Full benchmark documentation and usage |
| [tak.scm](benchmarks/tak.scm) | Takeuchi function (recursion stress test) |
| [fib.scm](benchmarks/fib.scm) | Fibonacci (naive recursive) |
| [nqueens.scm](benchmarks/nqueens.scm) | N-queens puzzle (backtracking) |
| [ackermann.scm](benchmarks/ackermann.scm) | Ackermann function (deep recursion) |
| [sieve.scm](benchmarks/sieve.scm) | Sieve of Eratosthenes (allocation) |

**21 benchmarks** covering recursion, allocation, floating-point, list processing, and more. See [benchmarks/README.md](benchmarks/README.md) for the complete list and cross-implementation comparison scripts.

## Running Examples

### Single Example

```bash
./dist/wile --file examples/basics/hello.scm
```

### Interactive REPL After Loading

```bash
./dist/wile --file examples/macros/state-machine.scm --interactive
```

### All Examples in a Category

```bash
for f in examples/basics/*.scm; do
  echo "=== $f ==="
  ./dist/wile --file "$f"
  echo
done
```

### All Benchmarks

```bash
cd examples/benchmarks
./run-all.sh
```

## Example Structure

Each example follows a consistent format:

```scheme
;;; filename.scm - Brief description
;;;
;;; Demonstrates: feature1, feature2, feature3
;;; Wile-specific: non-standard extensions used (if any)
;;;
;;; Usage: ./dist/wile --file examples/category/filename.scm

;; Explanatory comments
(define (example-function args)
  ;; Implementation
  ...)

;; Demonstration output
(display "Expected: ...")
(display (example-function test-input))
```

## Key Wile Features Demonstrated

### 1. Full R7RS Compliance

All examples use R7RS-small standard procedures. No vendor-specific extensions required for core functionality.

### 2. Hygienic Macros

`syntax-rules` macros with automatic hygiene. See [macros/](macros/) for DSL examples.

### 3. Numeric Tower

Exact arithmetic, rationals, complex numbers, arbitrary precision. See [numeric-tower/](numeric-tower/).

### 4. First-Class Continuations

`call/cc` for advanced control flow. See [control/](control/) for backtracking and coroutines.

### 5. Go Integration

- **Concurrency**: Threads backed by goroutines, channels for CSP
- **Embedding**: Pure Go, no CGo, clean API for Go developers
- **Performance**: Go's GC, no FFI overhead

### 6. Proper Tail Calls

Tail-recursive functions run in constant space. Compare tail vs non-tail in [basics/recursion.scm](basics/recursion.scm).

## Learning Path

**New to Scheme?** Start here:
1. [basics/hello.scm](basics/hello.scm) - Syntax and basic operations
2. [basics/recursion.scm](basics/recursion.scm) - Recursive thinking
3. [basics/higher-order.scm](basics/higher-order.scm) - Functional patterns
4. [basics/closures.scm](basics/closures.scm) - Lexical scoping

**Experienced Schemer?** See what's unique:
1. [macros/state-machine.scm](macros/state-machine.scm) - Hygiene system
2. [concurrency/channels.scm](concurrency/channels.scm) - Go integration
3. [numeric-tower/symbolic-diff.scm](numeric-tower/symbolic-diff.scm) - Exact arithmetic
4. [logic/schelog/](logic/schelog/) - Logic programming embedding

**Go developer?** Embedding examples:
1. [embedding/basic.go](embedding/basic.go) - API overview
2. [concurrency/threads.scm](concurrency/threads.scm) - Concurrency model
3. [applications/rule-engine.scm](applications/rule-engine.scm) - Practical use case

## Documentation

- **Language Reference**: See R7RS specification at https://small.r7rs.org/
- **Primitives**: See `PRIMITIVES.md` in the repository root
- **Implementation**: See `docs/dev/` for architecture and design documents

## Contributing Examples

Examples should:
- Be self-contained and runnable standalone
- Include clear comments explaining concepts
- Use R7RS-standard procedures when possible
- Note any Wile-specific extensions in the header
- Produce output demonstrating the concept
- Follow the standard template (see above)

## R7RS Compatibility Note

Some examples use R7RS features that may not be available in R5RS or R6RS implementations. Wile is a pure R7RS-small implementation.

For R6RS compatibility (e.g., different `error` signatures), load the shim:

```scheme
(load "examples/lib/r6rs-compat.scm")
```

## License

All examples are available under the same license as Wile (see repository root LICENSE file).
