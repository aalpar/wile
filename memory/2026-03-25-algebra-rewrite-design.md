# (wile algebra rewrite) — Equational Term Rewriting Design

## Summary

General-purpose term rewriting library for wile's `(wile algebra)` stdlib.
Generates rewrite rules from declared algebraic axioms (identity, commutativity,
absorbing elements, idempotence, involution). Primary consumer: wile-goast's
`ssa-normalize` for SSA equivalence detection.

## Motivation

wile-goast's `ssa-normalize.scm` hand-codes algebraic rewrite rules as per-operator
cond chains. Each axiom (identity elimination, commutativity, annihilation) is
written separately for each operator (+, *, &, |, ^). Adding new normalizations
for SSA equivalence v2 (idempotence, involution, and eventually distributivity,
De Morgan) would multiply the hand-coded cases.

The rewrite library inverts this: declare which algebraic properties hold for
each operator, and the library generates rewrite rules automatically.

## Design decisions

- **Term protocol:** User-provided accessor functions (get-operator, get-operands,
  make-term, compare). Generic — not tied to tagged alists or SSA nodes.
- **`make-term` receives the original term:** `(term, new-operands) → term`, not
  `(op, operands) → term`. The protocol destructures terms (`get-operator`,
  `get-operands`) and reconstructs them (`make-term`). Reconstruction needs the
  original term to preserve metadata the algebra doesn't touch (names, types,
  source positions). The algebraic projection via `get-operator`/`get-operands`
  is lossy; the rebuilder inverts it, so it needs the full concrete representation.
- **Element matching uses predicates:** `(identity-axiom op pred)` where `pred`
  is `(value → boolean)`. This lets consumers use domain-specific matching
  (e.g., `constant-zero?` for SSA constants `"0"`, `"0:int"`, `"0:uint64"`)
  instead of requiring `equal?` on a single canonical value.
- **Axioms as values:** First-class axiom records, composed into theories via
  plain lists. `append` is theory composition.
- **Normalizer returns `#f` on no-match:** `(make-normalizer theory protocol)` →
  `(term → value-or-#f)`. Single pass, first-match wins. Returns `#f` when no
  rule fires, not the original term. This makes normalizers composable as rules;
  consumers wrap with `(or (normalizer term) term)`. No fixed-point iteration —
  caller controls that.
- **Type scoping:** Consumer's responsibility (e.g., integer-only guard). Keeps
  the rewrite library domain-agnostic.
- **v1 scope:** 5 single-operator axiom types. Multi-operator axioms (distributivity,
  De Morgan) deferred — they require nested-term matching.

## Architecture

```
Term Protocol          Axioms              Normalizer
─────────────    ×    ──────────    →    ────────────
get-operator          identity            make-normalizer
get-operands          commutativity       → (term → value-or-#f)
make-term             absorbing
compare               idempotence
                      involution
```

The normalizer bridges axioms and protocol: for each axiom, it calls internal
`axiom->rules` with the protocol to produce concrete rewrite functions
`(term → value-or-#f)`.

## Term protocol

```scheme
(make-term-protocol get-operator get-operands make-term compare)
;; get-operator : term → symbol
;; get-operands : term → (list operand ...)
;; make-term    : (term, (list operand ...)) → term
;; compare      : (value, value) → boolean (less-than for canonical ordering)
```

`make-term` receives the original term and new operands. It preserves all
structure the algebra doesn't touch. For the simplest case (plain lists):

```scheme
(lambda (term new-args) (cons (car term) new-args))
```

For richer representations (SSA nodes with name/type metadata):

```scheme
(lambda (node new-args)
  (cons 'ssa-binop
        (map (lambda (pair)
               (cond
                 ((eq? (car pair) 'x) (cons 'x (car new-args)))
                 ((eq? (car pair) 'y) (cons 'y (cadr new-args)))
                 ((eq? (car pair) 'operands) (cons 'operands new-args))
                 (else pair)))
             (cdr node))))
```

The protocol is passed to `make-normalizer`, not to individual axioms.
Axioms are abstract declarations; the protocol is the concrete bridge.

## Axiom types

Five axiom constructors, each declaring a property for a specific operator.
Identity and absorbing take an element predicate `(value → boolean)`:

| Constructor | Declaration | Generated rules |
|-------------|-------------|-----------------|
| `(identity-axiom op pred)` | `op(x, e) = x` when `(pred e)` | `op(x,e)→x`, `op(e,x)→x` |
| `(commutativity-axiom op)` | `op(x,y) = op(y,x)` | Swap when `(compare y x)` is true |
| `(absorbing-axiom op pred)` | `op(x,z) = z` when `(pred z)` | `op(x,z)→z`, `op(z,x)→z` |
| `(idempotence-axiom op)` | `op(x,x) = x` | `op(x,x)→x` |
| `(involution-axiom op)` | `op(op(x)) = x` | Unary: `op(op(x))→x` |

Type predicates: `identity-axiom?`, `commutativity-axiom?`, etc. Plus generic `axiom?`.

## Theories

Theories are plain lists of axioms. No special type.

```scheme
(define int-additive
  (list (identity-axiom '+ constant-zero?)
        (commutativity-axiom '+)))

(define int-multiplicative
  (list (identity-axiom '* constant-one?)
        (commutativity-axiom '*)
        (absorbing-axiom '* constant-zero?)))

;; Compose by appending
(define int-arithmetic (append int-additive int-multiplicative))
```

## Normalizer

```scheme
(make-normalizer theory protocol) → (term → value-or-#f)
```

Construction: converts each axiom to rewrite rules via `axiom->rules`.
Application: tries rules in order, first non-`#f` wins. Returns `#f`
if no rule fires. Consumer wraps with fallback:

```scheme
(or (normalizer term) term)
```

Consumer handles type scoping:

```scheme
(define int-normalize (make-normalizer int-theory ssa-protocol))

(define (ssa-normalize node)
  (if (integer-type? (nf node 'type))
      (or (int-normalize node) node)
      node))
```

## Exports

```scheme
(define-library (wile algebra rewrite)
  (export
    ;; Term protocol
    make-term-protocol term-protocol?
    term-get-operator term-get-operands term-make-term term-compare
    ;; Axioms
    identity-axiom identity-axiom?
    commutativity-axiom commutativity-axiom?
    absorbing-axiom absorbing-axiom?
    idempotence-axiom idempotence-axiom?
    involution-axiom involution-axiom?
    axiom?
    ;; Normalizer
    make-normalizer))
```

Re-exported from `(wile algebra)` umbrella.

## Files

```
wile/stdlib/lib/wile/algebra/rewrite.sld   — library definition
wile/stdlib/lib/wile/algebra/rewrite.scm   — implementation
wile/stdlib/lib/wile/algebra.sld           — add re-export
```

## Consumer migration (wile-goast)

`ssa-normalize.scm` replaces hand-coded rules with theory declarations and
`make-normalizer`. Domain-specific concerns (predicate-based constant matching,
type scoping, metadata-preserving reconstruction) are handled through the
protocol and consumer wrappers:

```scheme
(import (wile algebra rewrite))

(define ssa-protocol
  (make-term-protocol
    (lambda (t) (nf t 'op))
    (lambda (t) (list (nf t 'x) (nf t 'y)))
    (lambda (node new-args)
      (cons 'ssa-binop
            (map (lambda (pair)
                   (cond
                     ((eq? (car pair) 'x) (cons 'x (car new-args)))
                     ((eq? (car pair) 'y) (cons 'y (cadr new-args)))
                     ((eq? (car pair) 'operands) (cons 'operands new-args))
                     (else pair)))
                 (cdr node))))
    (lambda (a b) (and (string? a) (string? b) (string<? a b)))))

(define int-identity-theory
  (list (identity-axiom '+ constant-zero?)
        (identity-axiom '* constant-one?)
        (identity-axiom '|\|| constant-zero?)
        (identity-axiom '^ constant-zero?)))

(define int-absorbing-theory
  (list (absorbing-axiom '* constant-zero?)
        (absorbing-axiom '& constant-zero?)))

(define comm-theory
  (list (commutativity-axiom '+)
        (commutativity-axiom '*)
        (commutativity-axiom '&)
        (commutativity-axiom '|\||)
        (commutativity-axiom '^)
        (commutativity-axiom '==)
        (commutativity-axiom '!=)))
```

Legacy API preserved as thin wrappers that add type guards:

```scheme
(define int-identity-rewrite (make-normalizer int-identity-theory ssa-protocol))

(define (ssa-rule-identity)
  (lambda (node)
    (and (tag? node 'ssa-binop)
         (integer-type? (nf node 'type))
         (int-identity-rewrite node))))
```

## Not in scope (deferred)

- Multi-operator axioms: distributivity `×(x, +(y,z)) → +(×(x,y), ×(x,z))`,
  De Morgan `¬(∧(x,y)) → ∨(¬x, ¬y)`, absorption `∧(x, ∨(x,y)) → x`
- AC-unification, confluence checking, Knuth-Bendix completion
- Associativity normalization (flattening nested same-op expressions)
- `validate-theory` — spot-checking axioms against sample terms. Deferred
  until a real validation strategy is designed (needs evaluation, not just
  structural matching)
