# Algebra Library Documentation Design

**Date:** 2026-04-14
**Status:** Approved
**Scope:** Documentation + examples for `(wile algebra)` library

## Problem

The algebra library has 14 sub-libraries, 117 exports, and zero user-facing documentation or examples. Test files exist but aren't approachable for someone learning the library.

## Audience

Layered for two audiences:
- **Go developers embedding Wile** — unfamiliar with abstract algebra, need motivation and concrete examples
- **Math-literate users** — know what a ring is, need to learn the Wile API

## Deliverables

### 1. `docs/ALGEBRA.md` — Introduction + Design

Purpose: Explain *why* the library exists, how it's organized, and how the pieces fit together.

Sections:
- **Overview** — one paragraph: what the library provides
- **Design philosophy** — operational records, explicit composition, three roles (operational/equational/explanatory)
- **Structure hierarchy** — diagram showing relationships (setoid → order → lattice → ..., monoid → group → ring → ...)
- **Patterns** — `validate-X`, `with-X`, forgetful projections, predicate-based matching
- **Learning path** — which examples to read in what order
- **Links** — to reference doc, examples, bibliography

Target: ~200 lines. Readable in 5 minutes.

### 2. `docs/ALGEBRA_REFERENCE.md` — API Reference

Purpose: Complete lookup table for every export, organized by sub-library.

Structure per sub-library:
- Brief description (1-2 sentences)
- Constructor signature + parameters
- Operation signatures
- Built-in instances (if any)
- Validation function signature
- `with-X` macro usage
- Projection functions (if any)

Sub-libraries in dependency order:
1. Setoid
2. Partial Order
3. Lattice
4. Closure Operator
5. Heyting Algebra
6. Boolean Algebra
7. Monoid
8. Category
9. Semiring
10. Group
11. Ring
12. Differential Ring
13. Field
14. Galois Connection
15. Rewrite
16. Symbolic

Target: ~500-700 lines. Comprehensive but terse.

### 3. `examples/algebra/` — Tutorial Examples

Each file is self-contained, runnable, produces output, follows the existing example format convention. Files build in complexity — each introduces one layer.

#### `getting-started.scm`
- First contact with the library
- Make a monoid (addition), fold a list, compute powers
- Validate monoid laws
- Use `with-monoid` for clean syntax
- **Audience entry point:** "What does this library do?"

#### `structures.scm`
- Build lattices (flat, powerset, product)
- Build rings (integer, modular)
- Build fields (rational)
- Forgetful projections: ring→semiring, boolean→heyting→lattice
- `with-X` pattern across structures
- **Key lesson:** structures compose via projection, not inheritance

#### `rewriting.scm`
- Define a term protocol for S-expressions
- Create axioms: identity, commutativity, absorbing, idempotence, involution, absorption
- Build normalizers and apply them
- Show no-match behavior
- **Key lesson:** axioms are data, normalization is mechanical

#### `symbolic.scm`
- Named axioms with human-readable descriptions
- Build theories from named axioms
- Theory combinators: filter, exclude, prioritize, merge
- Recursive normalizer with transformation tracing
- Format and display traces
- Fuel exhaustion behavior
- **Key lesson:** theories are composable, normalization is explainable

#### `boolean-simplifier.scm`
- End-to-end workflow: powerset Boolean algebra → theory → normalizer
- Simplify nested Boolean expressions with traced output
- Compare with Heyting (intuitionistic) vs Boolean (classical) simplification
- **Key lesson:** algebraic structure determines what simplifications are valid

#### `equivalence-discovery.scm`
- `discover-equivalences` on ring expressions
- Show how different sub-theories produce different normal forms
- Custom theory construction for domain-specific equivalences
- **Key lesson:** equivalence depends on which laws you assume

### 4. Update `examples/README.md`

Add an Algebra section to the existing examples README following the established table format.

## Non-Goals

- No changes to library implementation
- No new sub-libraries or exports
- No CLAUDE.md files for the algebra directories (the docs serve this purpose)
- No docs/learn/ article (that's a future task)

## Implementation Order

1. Write the 6 example files (they inform what the docs need to explain)
2. Write `docs/ALGEBRA.md` (introduction)
3. Write `docs/ALGEBRA_REFERENCE.md` (reference)
4. Update `examples/README.md`
5. Verify all examples run: `./dist/wile --file examples/algebra/*.scm`
