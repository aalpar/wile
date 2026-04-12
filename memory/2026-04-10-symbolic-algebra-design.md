# Symbolic Algebra — Connecting Structures to Rewriting

**Date:** 2026-04-10
**Status:** Implemented (PRs #632, #633)
**Depends on:** 2026-03-25-algebra-library-design.md, 2026-03-25-algebra-rewrite-design.md, 2026-04-09-orthogonal-algebra-types.md

## Problem

The algebra library and the rewrite library are disconnected. Algebraic structures
(monoid, lattice, ring, boolean, etc.) hold operational closures — they compute
values. The rewrite library transforms symbolic terms by equational axioms. Neither
knows about the other:

- No structure can produce the axioms its laws satisfy.
- No rewrite rule carries a name or general-form string for human explanation.
- The normalizer is single-step, single-site — it cannot simplify nested expressions
  or iterate to a fixed point.
- There is no way to report *which* law justified a transformation.

The validators (`validate-monoid`, `validate-lattice`, etc.) test the same axioms
the rewriter would apply — identity, commutativity, absorption, idempotence — but
by brute-force sampling rather than equational transformation. The mathematical
content is duplicated in two disconnected forms.

## Consumers

Four systems need algebraic reasoning over symbolic terms:

| Consumer | Language | Integration | Priority |
|----------|----------|-------------|----------|
| wile-goast (beliefs, FCA, Go boolean expressions) | Go→Scheme | Go AST projected into Scheme terms | Near-term |
| Symbolic algebra library | Scheme | Native Scheme terms | Near-term |
| CRDT merge reasoning | Scheme | Native Scheme terms | Near-term |
| Go rules engine | Go→Scheme | Axioms from Go, rewriting in Scheme | Distant |

All consumers share the same need: given an algebraic structure, produce its
equational theory, normalize symbolic terms against that theory, and explain the
transformations in human-readable form.

The Go side (wile-goast, future rules engine) declares axioms and constructs
symbolic terms. Scheme performs all rewriting and presentation.

## Architecture

Three layers, Approach A (theory layer with separate symbolic library):

```
┌─────────────────────────────────────────────────────────┐
│  Consumers                                              │
│  wile-goast (beliefs, FCA) · symbolic algebra · CRDT    │
└────────────────────────┬────────────────────────────────┘
                         │ imports
┌────────────────────────▼────────────────────────────────┐
│  (wile algebra symbolic)            NEW                 │
│                                                         │
│  • Named axioms (name + general-form + axiom)           │
│  • ->theory projections for each structure              │
│  • Recursive normalizer (fixed-point over subterms)     │
│  • Transformation trace (rule chain + explanation)      │
│  • Theory combinators (filter, prioritize, merge)       │
│  • Equivalence discovery                                │
│  • Reporter (human-readable trace formatting)           │
│  • Standard S-expression term protocol                  │
└──────────┬──────────────────────┬───────────────────────┘
           │ imports              │ imports
┌──────────▼──────────┐  ┌───────▼───────────────────────┐
│ (wile algebra *)    │  │ (wile algebra rewrite)        │
│                     │  │                               │
│ Operational structs │  │ Axiom types, term protocol,   │
│ (monoid, lattice,   │  │ single-step normalizer        │
│  ring, boolean...)  │  │                               │
│                     │  │ + absorption-axiom       NEW  │
│ UNCHANGED           │  │ + associativity-axiom    NEW  │
│                     │  │ + directional-axiom?     NEW  │
└─────────────────────┘  └───────────────────────────────┘
```

**Key decisions:**

1. `(wile algebra rewrite)` stays as-is except for two new axiom types and one
   new predicate. The single-step normalizer remains the primitive.

2. `->theory` projections live in `(wile algebra symbolic)`, not in each structure
   file. The symbolic library is the bridge — existing algebra files are untouched.

3. Axiom metadata (name, general-form string) is a new record type in `symbolic`.
   The existing axiom types in `rewrite` are not modified — `named-axiom` wraps them.

4. The recursive normalizer in `symbolic` calls `make-normalizer` internally for
   single-step rule application, layering recursion, fixed-point iteration, and
   trace accumulation on top.

## Three Roles

Each algebraic structure serves three roles. The first exists today; the second
and third are added by this design:

| Role | Purpose | Where |
|------|---------|-------|
| **Operational** (closures) | Compute values — ground truth oracle | Existing `(wile algebra *)` |
| **Equational** (axiom lists) | Drive the rewriter — generate candidate transformations | `->theory` projections in `symbolic` |
| **Explanatory** (axiom metadata) | Name the law, show the general form — tell the user *why* | `named-axiom` records in `symbolic` |

The operational closures are not waste — they verify that a proposed rewrite is
sound for the specific domain instance. The rewriter proposes, the structure
confirms, the reporter explains.

## Additions to `(wile algebra rewrite)`

### New axiom type: absorption

Absorption is a two-operator axiom: `op1(a, op2(a, b)) → a`. Needed by lattice
and Boolean algebra theories.

```scheme
(define-record-type <absorption-axiom>
  (make-absorption-axiom op-outer op-inner)
  absorption-axiom?
  (op-outer absorption-axiom-op-outer)
  (op-inner absorption-axiom-op-inner))
```

`axiom->rules` produces two rules for absorption:
- `op1(a, op2(a, b)) → a`
- `op1(op2(a, b), a) → a` (symmetric in the outer operand position)

### New axiom type: associativity

Associativity is a structural rewrite: `op(op(a,b), c) → op(a, op(b,c))`.
It normalizes bracketing to right-associative canonical form without reducing
term size.

```scheme
(define-record-type <associativity-axiom>
  (make-associativity-axiom op)
  associativity-axiom?
  (op associativity-axiom-op))
```

Associativity axioms are **directional** by construction (see below).

### Directional axioms

A directional axiom fires left-to-right only. The normalizer applies it like
any other rule, but `discover-equivalences` does not explore the reverse
direction. This prevents combinatorial explosion: N operands under an
associative operator produce Catalan(N-1) bracketings; directional
associativity settles on one canonical form.

```scheme
(define (directional-axiom? x)
  ;; Currently only associativity is directional.
  ;; The predicate is general — future axiom types can opt in.
  (associativity-axiom? x))
```

All existing axiom types remain non-directional (simplifying). The flag is a
predicate on axiom values, not a slot — no changes to existing record types.

### Updated axiom type list

| Axiom Type | Pattern | Directional? | Existed |
|------------|---------|:------------:|:-------:|
| identity | `op(a, e) → a` | no | yes |
| commutativity | `op(a, b) → op(b, a)` when b < a | no | yes |
| absorbing | `op(a, z) → z` | no | yes |
| idempotence | `op(a, a) → a` | no | yes |
| involution | `op(op(a)) → a` | no | yes |
| **absorption** | `op1(a, op2(a, b)) → a` | no | **new** |
| **associativity** | `op(op(a,b), c) → op(a, op(b,c))` | **yes** | **new** |

## `(wile algebra symbolic)` — Full API

### Named axioms and theories

```scheme
(define-record-type <named-axiom>
  (make-named-axiom name general-form axiom)
  named-axiom?
  (name         named-axiom-name)          ;; "absorption"
  (general-form named-axiom-general-form)  ;; "a ∧ (a ∨ b) = a"
  (axiom        named-axiom-axiom))        ;; the rewrite axiom object

(define-record-type <theory>
  (make-theory axioms associative-ops)
  theory?
  (axioms          theory-axioms)          ;; list of <named-axiom>
  (associative-ops theory-associative-ops)) ;; list of operator symbols
```

### Theory combinators

```scheme
(theory-prioritize theory rule-names)  → <theory>
;; Move named rules to the front, preserving relative order of the rest.

(theory-filter theory rule-names)      → <theory>
;; Keep only the named rules.

(theory-exclude theory rule-names)     → <theory>
;; Drop the named rules.

(theory-merge theory1 theory2)         → <theory>
;; Combine two theories (e.g., additive + multiplicative parts of a ring).
```

### Structure → theory projections

Each projection takes the structure plus the operator symbol(s) used in the
consumer's term language. The operator symbols bridge operational closures
(which are anonymous) to symbolic terms (where operators are named).

```scheme
(monoid->theory M op-symbol)                            → <theory>
(group->theory G op-symbol inv-symbol)                  → <theory>
(semiring->theory S plus-sym times-sym)                 → <theory>
(ring->theory R plus-sym times-sym neg-sym)             → <theory>
(field->theory F plus-sym times-sym neg-sym recip-sym)  → <theory>
(lattice->theory L join-sym meet-sym)                   → <theory>
(heyting->theory H join-sym meet-sym imp-sym)           → <theory>
(boolean->theory B join-sym meet-sym comp-sym)          → <theory>
```

**Example — monoid projection:**

```scheme
(define (monoid->theory M op-symbol)
  (make-theory
    (list
      (make-named-axiom
        "identity"
        (string-append (symbol->string op-symbol) "(a, e) = a")
        (make-identity-axiom op-symbol
          (lambda (x) (equal? x (monoid-identity M)))))
      (make-named-axiom
        "associativity"
        (string-append (symbol->string op-symbol)
                       "(" (symbol->string op-symbol) "(a,b), c) = "
                       (symbol->string op-symbol) "(a, " (symbol->string op-symbol) "(b,c))")
        (make-associativity-axiom op-symbol)))
    (list op-symbol)))
```

### Axiom inventory per structure

| Structure | Axioms | Count |
|-----------|--------|:-----:|
| Monoid | identity, associativity | 2 |
| Group | identity, associativity, involution(inverse) | 3 |
| Semiring | identity(+), identity(×), commutativity(+), absorbing(×,0), associativity(+), associativity(×) | 6 |
| Ring | semiring axioms + involution(negate) | 7 |
| Field | ring axioms + involution(reciprocal) | 8 |
| Lattice | identity(∨,⊥), identity(∧,⊤), commutativity(∨), commutativity(∧), idempotence(∨), idempotence(∧), absorption(∨/∧), absorption(∧/∨), associativity(∨), associativity(∧) | 10 |
| Heyting | lattice axioms (via lattice->theory on projection) | 10+ |
| Boolean | lattice axioms + involution(complement) | 11+ |
| Partial Order | *(no binary operations — no rewrite axioms)* | 0 |
| Galois Connection | *(adjunction is a property, not a rewrite rule)* | 0 |

### Recursive normalizer

```scheme
(make-recursive-normalizer theory proto)
;; Returns: (lambda (term) → (values normal-form trace))
;;
;; Strategy: bottom-up, fixed-point
;;   1. Recursively normalize all subterms (children first)
;;   2. Try all rules on the rebuilt term (via make-normalizer from rewrite lib)
;;   3. If any rule fires, record the step in the trace, repeat from (1)
;;   4. If no rule fires, term is in normal form
;;
;; Fuel: optional parameter (default 100), caps iteration.
;;       Returns best result so far with 'fuel-exhausted in trace.
```

### Equivalence discovery

```scheme
(discover-equivalences theory proto term)
;; Returns: list of (normal-form . trace) pairs, deduplicated.
;;
;; Runs the term through multiple filtered/reordered theories:
;;   - Each single-rule theory
;;   - Interesting combinations
;; Collects distinct normal forms.
;;
;; Respects directional-axiom?: does not explore reverse direction
;; for directional axioms (prevents associativity Catalan explosion).
;;
;; Consumer constraint: use theory-filter/theory-prioritize to control
;; which equivalences are explored. "Only show me absorption and identity"
;; or "prioritize commutativity" — expressed as theory manipulation.
```

### Transformation trace

```scheme
(define-record-type <rewrite-step>
  (make-rewrite-step rule-name general-form before after)
  rewrite-step?
  (rule-name    step-rule-name)     ;; "absorption"
  (general-form step-general-form)  ;; "a ∧ (a ∨ b) = a"
  (before       step-before)        ;; the term before rewriting
  (after        step-after))        ;; the term after rewriting

;; The normalizer returns (values normal-form trace) via multiple return.
;; Consumers that don't need the trace ignore the second value.

(format-trace trace)                → list of strings
;; Each string: "absorption (a ∧ (a ∨ b) = a): (and x (or x y)) → x"
```

### Standard term protocol

```scheme
(sexp-term-protocol compare)       → <term-protocol>
;; S-expression terms: (op arg ...) as pairs, atoms as leaves.
;; compare: ordering predicate for commutativity normalization.
;;
;; compound-term?: pair?
;; get-operator:   car
;; get-operands:   cdr
;; make-term:      (lambda (term new-args) (cons (car term) new-args))
;; compare:        supplied by caller
```

## Consumer Integration

### wile-goast — beliefs

The belief DSL is unchanged. wile-goast scripts construct a symbolic term
alongside the operational belief predicate:

```scheme
;; Operational (existing):
(define-belief "lock-unlock"
  (sites (functions-matching (contains-call "Lock")))
  (expect (paired-with "Lock" "Unlock"))
  (threshold 0.8 3))

;; Symbolic (new, constructed by wile-goast script):
;; (and (calls "Lock") (calls "Unlock"))
;;
;; Two beliefs whose symbolic terms normalize to the same form
;; under boolean->theory are equivalent. The trace explains why.
```

### wile-goast — FCA

FCA attribute sets are elements of a powerset Boolean algebra. The concept
lattice ordering is the lattice ordering on intents:

```scheme
;; C1: intent = {Cache.Entries, Cache.TTL}
;; C2: intent = {Cache.Entries, Cache.TTL, Index.Keys, Index.Version}
;;
;; C1's intent ⊆ C2's intent  →  C1 is a superconcept of C2
;; C2's intent = C1's intent ∧ {Index.Keys, Index.Version}
;;
;; Report: "The Cache+Index concept extends the Cache-only concept
;;          by adding Index fields — meet in the concept lattice"
```

### wile-goast — Go boolean expressions

SSA analysis extracts boolean conditions, projected as symbolic terms:

```scheme
;; Go: x != nil && (x != nil || y > 0)
;; Symbolic: (and (not-nil x) (or (not-nil x) (gt y 0)))
;;
;; Boolean normalization via boolean->theory:
;;   absorption (a ∧ (a ∨ b) = a): (and (not-nil x) (or (not-nil x) (gt y 0))) → (not-nil x)
;;
;; Report: "redundant disjunction — absorption law"
```

### Symbolic algebra library (future consumer)

```scheme
(let* ((R (integer-ring))
       (theory (ring->theory R '+ '* '-))
       (proto  (sexp-term-protocol (lambda (a b) (string<? (symbol->string a)
                                                           (symbol->string b)))))
       (norm   (make-recursive-normalizer theory proto)))
  (norm '(+ (* 0 y) (+ x 0))))
;; → (values 'x '(step1 step2 step3))
;; trace: absorbing (* 0 y → 0), identity (+ 0 x → x), identity (+ x 0 → x)
```

### CRDT

A CRDT merge is a join-semilattice — three axioms:

```scheme
(define (crdt-merge-theory merge-sym)
  (make-theory
    (list
      (make-named-axiom "commutativity" "merge(a,b) = merge(b,a)"
        (make-commutativity-axiom merge-sym))
      (make-named-axiom "idempotence" "merge(a,a) = a"
        (make-idempotence-axiom merge-sym))
      (make-named-axiom "associativity" "merge(merge(a,b),c) = merge(a,merge(b,c))"
        (make-associativity-axiom merge-sym)))
    (list merge-sym)))
```

## Phasing

### Phase 1 — Foundation

Connects what exists. Self-contained and testable.

**`(wile algebra rewrite)` additions:**
- `absorption-axiom` type + `axiom->rules` case
- `associativity-axiom` type + `axiom->rules` case
- `directional-axiom?` predicate
- Tests for new axiom types

**`(wile algebra symbolic)` — new library:**
- `named-axiom` record type
- `theory` record type
- Theory combinators: `theory-prioritize`, `theory-filter`, `theory-exclude`
- `make-recursive-normalizer` with trace and fuel
- `rewrite-step` record type
- `format-trace`
- `sexp-term-protocol`
- Three projections: `monoid->theory`, `lattice->theory`, `boolean->theory`
- Tests: normalize boolean expressions, trace output, fuel exhaustion

**Deliverable:** Build a Boolean algebra, extract its theory, normalize a
symbolic expression like `(and x (or x y))`, get a traced explanation
naming absorption as the justifying law.

### Phase 2 — Complete Projections + Equivalence Discovery

- Remaining projections: `group->theory`, `semiring->theory`, `ring->theory`,
  `field->theory`, `heyting->theory`
- `discover-equivalences`
- `theory-merge`
- Tests: ring normalization, equivalence discovery across orderings,
  theory merging for composed structures

### Phase 3 — wile-goast Integration

- Boolean expression term protocol for Go AST conditions
- Belief predicate symbolic representation
- FCA concept lattice algebraic annotation
- Reporting integration with `boundary-report` and belief output
- Tests: end-to-end from Go source to algebraic explanation

### Deferred — No Phase Assigned

- Symbolic algebra library (`(wile symbolic)`) — separate project, consumes this design
- CRDT merge theory — trivial once Phase 1 lands
- Go rules engine — distant, axiom-only interface from Go
- miniKanren integration — complementary capability, no dependency
- Phase 2 algebra types (setoid, category, closure, differential) — `->theory`
  projections added when those types land

## Relationship to Existing Plans

- **2026-03-25-algebra-library-design.md** — master design for operational structures.
  This design adds the equational and explanatory roles without modifying the
  operational layer.
- **2026-03-25-algebra-rewrite-design.md** — term rewriting engine. This design
  adds two axiom types and one predicate; the core engine is unchanged.
- **2026-04-09-orthogonal-algebra-types.md** / **2026-04-10-orthogonal-algebra-phase2-design.md** —
  new structure types. Each will get a `->theory` projection when implemented,
  following the pattern established here.
- **wile-goast plans/2026-04-08-false-boundary-detection-design.md** — FCA design.
  Phase 3 of this plan adds algebraic annotation to FCA output.

## What This Design Does NOT Cover

- **Polynomial representation.** How to represent `3x^2 + 2x + 1` as a term.
  That's the symbolic algebra library's concern — it defines domain-specific
  term constructors and uses `ring->theory` to get the rewrite rules.
- **Confluence or termination proofs.** The fuel parameter is the pragmatic
  answer. Formal analysis of rule interaction is out of scope.
- **AC (associative-commutative) matching.** The current approach handles
  associativity and commutativity as separate axioms applied sequentially.
  True AC-matching (matching modulo associativity and commutativity simultaneously)
  is a significant complexity jump. Deferred unless the sequential approach
  proves insufficient.
- **Existing library modifications.** No changes to any `(wile algebra *)`
  structure file. No changes to the belief DSL. No changes to FCA core.
