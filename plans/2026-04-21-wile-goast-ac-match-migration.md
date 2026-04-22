# wile-goast AC-match Migration

**Status:** Stub — deferred follow-up.
**Predecessor:** `plans/2026-04-21-ac-matching-design.md`

## Scope

Migrate `wile-goast/cmd/wile-goast/lib/wile/goast/unify.scm:421` from `discover-equivalences` (from `(wile algebra symbolic)`) to `ac-unify` (from `(wile algebra unification)`).

## Three risks to address

1. **Term-protocol contract compliance.** Add protocol-conformance test for wile-goast's Go-AST protocol. The `term-protocol`'s `term-compare` must be a total order consistent with `equal?` modulo AC-equivalence under the caller's theory — `(wile algebra unification)` assumes this invariant holds on every compare callback.
2. **Trace-emitting diagnostic paths.** Audit consumers of `discover-equivalences` traces; any call site relying on transformation traces stays on `discover-equivalences` until a trace-reconstructing variant of `ac-unify` exists.
3. **Small-arity benchmark.** Measure Eker+matrix vs direct-enumeration crossover on typical wile-goast input sizes. v1 `ac-unify` uses Stickel reduction with 0/1 basis multiplicities and a permutation enumeration over operand pairs; larger multiplicities or high-arity nodes may regress vs the existing exponential fallback.

## Scope (estimated ~100 LOC)

- Call-site migration: ~10 LOC
- Benchmark harness: ~40 LOC
- Protocol-conformance test: ~30 LOC
- Optional retirement of `discover-equivalences` from `symbolic.scm`: ~20 LOC (gated on no surviving consumers)

## Out of scope

- Wile-side changes beyond library export surface.
- Full Stickel with non-unit multiplicities (variable ↔ `(op …)` from opposite side) — deferred per Phase 5 library notes; required only if a benchmark input exhibits the pattern.
