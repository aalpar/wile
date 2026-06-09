# Algebra library conventions

Shared conventions for the 20+ algebraic-structure libraries under
`(wile algebra X)`. Reviewers and new-structure authors: treat this as the
template. Existing structures that deviate for good reason should say so in
a comment at the deviation site.

## The five-part structure API

Every structure library `(wile algebra X)` exports the following pieces for
its principal record type `<X>`:

| Piece            | Shape                                                | Example                              |
|------------------|------------------------------------------------------|--------------------------------------|
| Constructor      | `(make-X arg1 arg2 ... . opts)`                      | `(make-group op id inv . opts)`      |
| Predicate        | `(X? v)`                                             | `(group? v)`                         |
| Accessors        | `(X-field structure)` for each record field          | `(group-op G)`, `(group-identity G)` |
| Field binder     | `(with-X s (field ...) body ...)` macro              | `(with-group G (op id inv) ...)`     |
| Law checker      | `(validate-X s samples)` returning `#t` or `list`    | `(validate-group G '(1 2 3))`        |

The law checker's return contract is uniform: `#t` on success, or a reversed
list of `(violation-type arg ...)` entries. Consumers who prefer exception
semantics wrap the call with `assert-validation`:

    (assert-validation (validate-group G '(-2 -1 0 1 2)))

The older `assert-X` wrappers (`assert-group`, `assert-graph`) are retained
as thin conveniences but new structures should not add their own — use the
generic `assert-validation` instead.

## Export wiring — leaf `.sld` + umbrella mirror

Adding a new export to any `(wile algebra X)` leaf library is a **two-file
edit**, not one:

1. The leaf `combinatorial-graph.sld` (or `group.sld`, etc.) — the `export`
   list of the library that defines the binding.
2. The umbrella `stdlib/lib/wile/algebra.sld` — which must re-export **every**
   symbol from **every** leaf, under the matching `;;`-comment section.

This coupling is enforced by `TestAlgebraUmbrellaCoversLeafExports`
(`algebra_umbrella_drift_test.go`): it diffs the umbrella's exports against the
union of all leaf exports and fails on any leaf symbol the umbrella omits.

**Why the per-library Scheme suite won't catch a missing mirror.** A leaf test
imports the leaf directly — `(import (wile algebra combinatorial-graph))` — so
the new binding resolves and every test passes locally. The umbrella gap is
invisible from there; only the Go drift test, which inspects the umbrella, sees
it. A missing mirror therefore passes the leaf suite and **fails `make ci`** —
a wasted round-trip. Mirror the export when you add it, not after CI tells you.

The umbrella sections mirror the leaf section comments (e.g. `;; Combinatorial
graphs — matching`), so place the new umbrella entry under the same heading you
used in the leaf. There is no auto-generation — the drift test is a checker, not
a generator; the mirror is maintained by hand.

## Shared plumbing — `(wile algebra setoid)`

Every leaf library imports `(wile algebra setoid)` (even those whose domain
has nothing to do with equivalence classes). It hosts five pieces of shared
plumbing:

| Helper                      | Purpose                                                              |
|-----------------------------|----------------------------------------------------------------------|
| `setoid-member?`            | Boolean membership under an arbitrary equivalence                    |
| `setoid-assoc`              | `assv` analogue keyed by an arbitrary equivalence                    |
| `setoid-dedup`              | `delete-duplicates` analogue under an arbitrary equivalence          |
| `assv-or`                   | Option-alist lookup with fallback                                    |
| `validate-opts-keys`        | Reject unknown keys in a trailing-option alist                       |
| `make-violation-reporter`   | Two-mode procedure shared by every `validate-X` body                 |
| `assert-validation`         | Macro: raise if `(validate-X ...)` did not return `#t`               |
| `assert-procedure`          | Macro: raise if argument is not a procedure, captures identifier     |

Use these rather than re-rolling private `%`-prefixed copies. The project
has already paid the cost of consolidating these — a new private copy
regresses that work.

## Procedural-argument discipline for `make-X`

Every required procedural argument of a `make-X` constructor is validated
at the top of the constructor body using `assert-procedure` from
`(wile algebra setoid)`:

    (define (make-ring plus times zero one negate)
      (assert-procedure "make-ring" plus)
      (assert-procedure "make-ring" times)
      (assert-procedure "make-ring" negate)
      ...)

The macro captures the source identifier, so the failure message reads
`"make-ring: plus must be a procedure"` — no hand-written label needed.

When the constructor name is also the record-type constructor (i.e. the
record's `make-X` is directly consumed by callers), introduce a
`make-X*`-suffixed internal constructor and wrap it:

    (define-record-type <monoid>
      (make-monoid* op identity)  ; record-type constructor (private)
      monoid?
      ...)

    (define (make-monoid op identity)  ; public wrapper
      "..."
      (assert-procedure "make-monoid" op)
      (make-monoid* op identity))

## Options-alist discipline for `make-X`

Trailing-option alist pattern:

    (define (make-X required1 required2 . opts)
      ...validate required args...
      (validate-opts-keys "make-X" opts '(setoid element? extra-field ...))
      (let ((setoid  (assv-or opts 'setoid     (default-setoid)))
            (elem?   (assv-or opts 'element?   #f))
            (extra   (assv-or opts 'extra-field default)))
        ;; validate each optional arg's type before stuffing the record
        (%make-X required1 required2 setoid elem? extra)))

Unknown keys must raise via `validate-opts-keys`, not silently fall through
to the fallback — typos (`'elements?` for `'element?`) need to surface at
construction.

## Validator body shape

    (define (validate-X S samples)
      "...docstring..."
      (let ((fail! (make-violation-reporter))
            (other-cache-bindings (compute-from S))  ; optional
            ...)
        ;; ...checks calling (fail! 'violation-type arg ...) on mismatch...
        (fail!)))  ; final call with zero args returns #t or reversed list

Delegation to a parent structure's validator (e.g. field → ring, boolean →
lattice, partial-order/setoid → partial-order) splices the parent's result
back in:

    (let ((parent-result (validate-parent P samples)))
      (unless (eq? #t parent-result)
        (for-each (lambda (v) (apply fail! v)) parent-result)))

## `with-X` macro shape

Each structure defines its own `with-X` because the field list differs, but
the skeleton is fixed:

    (define-syntax with-X
      (syntax-rules ()
        ((with-X s (field1 field2 ...) body ...)
         (let ((tmp s))
           (let ((field1 <expression accessing tmp>)
                 (field2 <expression accessing tmp>)
                 ...)
             body ...)))))

Single-argument accessors are wrapped in a lambda; zero-argument ones are
bound directly. Fields that are procedures taking the structure's elements
get `(lambda (a b) (X-op tmp a b))`. See `monoid.scm:28`, `group.scm:368`,
`lattice.scm:105`.

No meta-macro is provided — the duplication is stable, mechanical, and easy
to read. This convention is the *only* thing keeping `with-X` macros
consistent across libraries.

## Extension fields via tier-lifting

When a structure grows optional metadata (a setoid for carrier equality, an
`elements` enumeration, a membership predicate), follow the pattern
established in `group.scm` and `lattice.scm`:

- The record type carries the new fields.
- `make-X` accepts them as trailing alist entries (`(setoid . S)`, etc.).
- Accessors expose them: `group-setoid`, `group-elements`.
- Tier predicates distinguish capability levels: `finite-group?`,
  `finitely-generated-group?`.

Backwards compatibility: the required positional arguments of `make-X`
don't change; new metadata is always optional with sensible defaults.

## Carrier opt — bridging Scheme structures to Go-side fast paths

Structure records that declare a `(carrier . SYM)` opt let consumer
libraries dispatch on the carrier type without inspecting the structure's
operations directly. `<semiring>` is the first carrier of this pattern:

| Symbol | Carrier | Status |
|--------|---------|--------|
| `'big-int` | `*BigInteger` | Active — opts into `(wile algebra graph)`'s `count-paths-in-dag` integration when conditions hold (see below) |
| `'saturating` | int (clamped to `[0, CAP]`) | Active — `bounded-carrier-semiring?` and `semiring-cycle-safe?` recognise it |
| `'boolean` | `#t`/`#f` | Active — `semiring-cycle-safe?` recognises it (idempotent ⊕) |
| `'tropical` | number ∪ `'tropical-inf` | Active — `semiring-cycle-safe?` recognises it (idempotent ⊕) |
| `'modular` | int in `[0, P-1]` | Advisory — declared by `modular-counting-semiring` for future Go-side modular kernels |
| `'log-float` | float64 in log-space | Advisory — declared by `log-counting-semiring` |
| `#f` (absence) | n/a | No fast path — dispatch falls through to the generic Scheme inner loop |

Reserved symbols for future sub-paths (no fast path attached today;
declaring them is equivalent to `#f`): `'integer`, `'rational`, `'real`,
`'complex`, `'opaque`. The active vocabulary reuses
`values/NumericTypeSpec.schemeName` where applicable. Unknown symbols
are accepted silently per the "never error on unrecognised carrier"
contract below.

The Scheme-side consumers of the carrier symbol today are
`bounded-carrier-semiring?` (matches `'saturating`) and
`semiring-cycle-safe?` (matches `'saturating`, `'boolean`, `'tropical`).
Both are closed-set lookups: unknown symbols answer `#f`. Adding a new
cycle-safe carrier requires extending the predicate's `case` list, not
silently inferring from operations.

The carrier symbol is *advisory* — declaring it doesn't change Scheme-visible
arithmetic. It signals consumer-side fast-path eligibility. The built-in
`bigint-counting-semiring` is shorthand for
`(make-semiring + * 0 1 '(carrier . big-int))`, so users opting into the
bignum fast path can do so with one call.

Consumer libraries that wire up a Go-side kernel should:

1. Detect the carrier symbol at construction time, not query time
   (eligibility shouldn't be re-checked on every operation).
2. Document which carrier symbols they handle and what fast path attaches.
3. Fall through to the existing generic path when the carrier doesn't match
   — never error on unrecognised carrier (the structure remains valid
   under the generic path).

See `graph.scm`'s `compute-via-count-paths-in-dag` for the canonical
integration shape: name→index hashtable, kernel call, alist projection.

## Private helpers

Within a library, use a `%` prefix for private helpers that shouldn't cross
the library boundary. Do not export them. If a helper becomes useful to
multiple libraries, promote it (drop the `%`, export it) rather than
copy-pasting.

## References

- `docs/algebra/` — user-facing library documentation
- `plans/` — design documents for structural additions
- `TODO.md` — open work; Algebra subsection tracks consistency debt
