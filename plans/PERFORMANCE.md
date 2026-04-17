# Performance Plans

**Status:** 1 complete, 1 open, 1 rejected

> **Completed:** Procedure Inlining (PR #605).
> **Incomplete:** Environment Frame Slimming (no floatVal, stackEntry, PushFloat, or lightweight frame type).
> **Rejected:** NaN-Boxing (requires `unsafe`, hard constraint).

## Remaining Optimization Opportunities

### Procedure Inlining

**Status:** **Complete** (PR #605)
**Requires `unsafe`:** No. Compiler-level bytecode transformation.

Wile performs no compile-time inlining of known procedures. Racket/Chez inline small known procedures at call sites when the binding is immutable.

**Design direction:**
- Bindings not targeted by `set!` are immutable candidates
- Inline criterion: procedure body is a single expression (or small number of instructions), binding is not `set!`-ed, callee is in scope
- New pass: scan for `OpCallCachedBinding` where the target is a known small closure, replace with inlined body

The opcode promotion work already inlines the 11 hottest primitives at the VM level — this would extend to user-defined functions.

### Environment Frame Slimming

**Status:** Open
**Requires `unsafe`:** No. Struct redesign.

`EnvironmentFrame` carries 6 fields: `parent`, `local`, `global`, `phaseLevel`, `phases`, `topLevel`. Closure bodies only need `local` (for parameter bindings). The other 5 fields are set but never read at runtime after flattening.

**Design direction:**
- Lightweight parameter-only frame type: `struct { bindings []Binding }` with no parent/global/phase fields
- Or: make `EnvironmentFrame` fields lazy
- Eliminates 5 pointer/int copies per closure call

### NaN-Boxing / Tagged Pointers

**Requires `unsafe`:** **YES. Will NOT be completed for Wile.**

`values.Value` is a Go interface (16 bytes). Small integers, booleans, characters could be encoded in 64 bits. Eliminates interface overhead, reduces stack/binding sizes by 50%. Requires `unsafe.Pointer` — not compatible with Wile's pure-Go constraint.

## Benchmark Baseline (2026-03-16, `ec26f1c8`)

| Benchmark | Avg (s) | Min (s) |
|-----------|---------|---------|
| tak | 0.1123 | 0.1086 |
| takl | 1.0883 | 1.0669 |
| ctak | 1.6532 | 1.5877 |
| cpstak | 0.1806 | 0.1753 |
| fib | 0.3715 | 0.3621 |
| triangl | 0.0382 | 0.0367 |
| sum | 0.0311 | 0.0300 |
| sumfp | 0.6206 | 0.6038 |
| diviter | 2.5677 | 2.5217 |
| divrec | 0.8759 | 0.8452 |
| deriv | 0.1028 | 0.1001 |
| ackermann | 0.4851 | 0.4660 |
| sieve | 0.0808 | 0.0786 |
| nqueens | 1.9047 | 1.8476 |
| primes | 0.2367 | 0.2316 |
| peval | 0.0675 | 0.0653 |

6 runs, M4 Max.

## Measurement

Run benchmarks: `make bench-gabriel` for the 16-benchmark Gabriel suite. ZebraPuzzle (`go test -bench=BenchmarkZebraPuzzle`) for backtracking stress test. Profile with `go test -bench=X -cpuprofile` and `go test -bench=X -memprofile` for allocation analysis.

---

# Fused Lexing and Parsing: The Flap Approach

**Status:** Research reference

Based on: *flap: A Deterministic Parser with Fused Lexing* (Yallop, Xie,
Krishnaswami — PLDI 2023), building on *A Typed, Algebraic Approach to Parsing*
(Krishnaswami & Yallop — PLDI 2019, Distinguished Paper).

## 1. The Core Problem

Traditional pipelines separate lexing and parsing into two phases:

```
source bytes -> Lexer -> Token stream -> Parser -> AST
                         ^
                   heap-allocated objects
                   carrying type tag + string slice
```

This separation is good for modularity but introduces overhead:

1. **Token materialization** — each token is allocated as a data structure
   (type tag, source slice, position metadata).
2. **Redundant branching** — the lexer *knows* it just recognized a `NUMBER`
   token, then immediately discards that knowledge. The parser later
   case-switches on the token tag to recover the same information.
3. **Cache pressure** — the token objects are short-lived heap allocations
   that pollute the cache between the point of lexical recognition and
   syntactic consumption.

Flap's insight: the lexer and parser can be *defined* separately (preserving
modularity) but *compiled* into a single fused pass (recovering performance).

## 2. Sketch: Applying Fused Lexing to Wile

### Current Architecture

```
source string
    |
    v
+---------------------------------------------+
| Tokenizer  (internal/tokenizer/)            |
|  io.RuneReader -> single rune lookahead     |
|  Next() -> *SimpleToken (heap-allocated)    |
|  SimpleToken ~ 56 bytes                     |
+----------------+----------------------------+
                 |  Token interface
                 v
+---------------------------------------------+
| Parser  (internal/parser/)                  |
|  Recursive descent, switch on TokenizerState|
|  Builds *Pair / SyntaxValue AST            |
+---------------------------------------------+
```

### Where Fusion Would Help

1. **Token allocation** — every `Next()` call creates a `*SimpleToken` on the heap
2. **Token type switch** — parser's `readSyntax()` dispatches on ~70-variant enum
3. **Number parsing** — 15 token sub-types classified then re-examined

### Incremental Adoption Path

1. **Phase 1: Measure** — profile to confirm token allocation is a meaningful cost
2. **Phase 2: Internalize** — move tokenizer's scanning methods into the parser
3. **Phase 3: Eliminate tokens for simple cases** — single-character tokens read directly
4. **Phase 4: Fuse number parsing** — merge tokenizer's number classification with parser
5. **Phase 5: Fuse remaining token types** — strings, symbols, booleans, characters
6. **Phase 6: Optimize input model** — operate on `[]byte` directly for in-memory sources

### Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|------------|
| Increased parser complexity | Medium | S-expression grammar is simple |
| Error message quality | Medium | Needs care with character-level errors |
| Testing surface | Low | Existing parser tests specify input->output |

## References

- Yallop, Xie, Krishnaswami. *flap: A Deterministic Parser with Fused Lexing.* PLDI 2023.
- Krishnaswami, Yallop. *A Typed, Algebraic Approach to Parsing.* PLDI 2019.
