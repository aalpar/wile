# Records as Formal Types

You write `define-record-type` and get a constructor, a predicate, and some
accessors. Useful, sure. But when a type theorist says "type," they mean
something precise — and Scheme records fit that definition with surprising
exactness. Let's see how.

## What *Is* a Type?

Forget the programming language meaning for a moment ("int, string, bool").
In type theory — the mathematical framework — a type is characterized by
exactly three things:

1. **Introduction rules**: how do you *create* a value of this type?
2. **Elimination rules**: how do you *use* or *observe* a value of this type?
3. **Computation rules**: what happens when you eliminate something you just
   introduced?

That's it. A type isn't a set of values (that's the set-theoretic view). A type
is defined by the ways you can build values and the ways you can take them apart.

The classic example is the **product type** A × B:

- **Introduction**: if you have an `a : A` and a `b : B`, you can form
  `(a, b) : A × B`
- **Elimination**: if you have `p : A × B`, you can extract `fst(p) : A`
  and `snd(p) : B`
- **Computation** (β-rule): `fst((a, b))` = `a` and `snd((a, b))` = `b`

There's also an η-rule: `(fst(p), snd(p))` = `p` — if you take a product
apart and put it back together, you get the original. This one matters for
reasoning about program equivalence.

## Records *Are* Product Types

Now look at what `define-record-type` gives you:

```scheme
(define-record-type <point>
  (make-point x y)      ; introduction rule
  point?                ; type membership test
  (x point-x)           ; elimination rule (projection π₁)
  (y point-y))          ; elimination rule (projection π₂)
```

Map the pieces:

| Type theory | Scheme record | Concrete |
|-------------|---------------|----------|
| Introduction rule | Constructor | `(make-point 3 4)` |
| Elimination rules | Field accessors | `(point-x p)`, `(point-y p)` |
| β-rule | Accessor ∘ constructor = identity on that field | `(point-x (make-point 3 4))` = `3` |
| η-rule | Reconstruct from accessors = original | `(make-point (point-x p) (point-y p))` = `p`* |

The asterisk on the η-rule: this holds for immutable records, where identity
is determined by field values. For mutable records, the η-rule gives you an
*equal* value, not the *same* object — `eq?` may return `#f`. That's
a distinction between extensional and intensional equality, which matters in
type theory too.

So a record type is a **labeled product type**. "Labeled" because the
projections have names (`point-x`, `point-y`) rather than being positional
(`fst`, `snd`). In the type theory literature, this is sometimes written as
a dependent sum with constant families, but the intuition is just "a tuple
where you can access fields by name."

```
Point = { x : Number, y : Number }
```

The cardinality (total number of possible values) of this type is
|Number| × |Number| — exactly what you'd expect from a product.

## The Nominal Twist

Here's where Scheme records depart from the simple product-type story.
Consider:

```scheme
(define-record-type <point>
  (make-point x y) point? (x point-x) (y point-y))

(define-record-type <vec2>
  (make-vec2 x y) vec2? (x vec2-x) (y vec2-y))

(point? (make-vec2 3 4))  ; => #f
```

Both `<point>` and `<vec2>` have structure `{ x : Number, y : Number }`.
Structurally, they're the same type. But `point?` rejects a `vec2`. **Scheme
records are nominally typed**: two types with identical structure are still
different types because they have different *names* (more precisely, different
*generative identities*).

In type theory, the distinction works like this:

- **Structural type systems** (most of core type theory, Go interfaces,
  TypeScript): type compatibility is determined by shape. If two types have
  the same fields with the same types, they're interchangeable.

- **Nominal type systems** (Java classes, Haskell `newtype`, Scheme records):
  type compatibility is determined by declaration. Each `define-record-type`
  introduces a *fresh type name* that's incomparable to everything else.

Why does this matter? Because **nominality is how you encode meaning that
structure alone can't carry.** A 2D point and a complex number might both be
pairs of reals. A latitude-longitude and a width-height might both be pairs
of floats. Structural typing says these are the same. Nominal typing says: you
told me these are different things, and I'll hold you to that.

Formally, each `define-record-type` invocation is **generative**: it produces
a type constructor that has never existed before and will never be produced
again (even by an identical `define-record-type` expression evaluated a second
time). This is exactly the property that R6RS calls "generativity" and
optionally suppresses with "nongenerative" record types.

## The Predicate: Runtime Typing Judgment

In a statically-typed language, the judgment "x has type T" is verified by the
compiler before the program runs. In Scheme, that judgment is pushed to runtime
in the form of the predicate:

```
Static:    Γ ⊢ x : Point        (compiler proves this)
Dynamic:   (point? x)  =>  #t   (runtime tests this)
```

The predicate `point?` is the **runtime reification of the typing judgment.**
It doesn't change what types *are* — it changes *when* and *how* type
membership is checked.

This is why dynamically-typed languages aren't "untyped" — they have exactly
the same type structure as their statically-typed counterparts, just enforced
at a different phase. Scheme records make this especially visible because they
give you an explicit predicate for each type, which is literally a boolean
function that answers the typing judgment question.

## Disjointness: Partitioning the Universe

R7RS requires that record types are disjoint from all built-in types and from
each other. Formally:

> For any two distinct types T₁, T₂ (whether built-in or record):
> T₁ ∩ T₂ = ∅

Together with the built-in types (pair, vector, string, number, boolean,
character, symbol, port, procedure, eof-object, null), record types help
**partition** the universe of Scheme values. Every value belongs to exactly
one leaf type.

```
              Value
         ┌──────┼──────────┐
       pair   vector   <point>  ...
                        (disjoint from everything)
```

This is a powerful property. It means predicate dispatch is **unambiguous** —
you never have a value that satisfies two record predicates. And it means
you can implement exhaustive dispatch with a chain of `cond` clauses and
know that if none of the cases match, the value genuinely isn't any of
those types.

In type-theoretic terms, the universe of Scheme values forms a **sum type**
(tagged union, coproduct) where each record type is one summand:

```
Value = Pair + Vector + String + Number + ... + Point + Vec2 + ...
```

Each `define-record-type` adds a new summand to this open sum.

## What Records Don't Express

To understand what records *are* in type theory, it helps to see what they
*aren't*:

**No subtyping.** There's no way to say "every Point3D is also a Point."
In type theory, this would be a subtyping judgment T₁ <: T₂ — meaning
anywhere you expect a T₂, you can supply a T₁. R7RS records are flat; no
record type is a subtype of any other. (R6RS and SRFI-99 add this.)

**No parametric polymorphism.** You can't write `<pair-of T>` where T varies.
In type theory, this is a type constructor — a function from types to types.
Records are always monomorphic: the fields hold any Scheme value, which is
the single dynamic type, not a parameter.

Actually, that's a subtle point worth pausing on. In Scheme, every field
has type `Value` — the universal dynamic type. So a `<point>` isn't really
`{ x : Number, y : Number }` in the typed sense. It's `{ x : Value, y : Value }`.
The fact that you *intend* `x` to hold a number is a convention, not a
constraint the type system enforces. Records give you nominal type identity
but not field type constraints.

**No sum types as first-class constructs.** You can't declare that a `Shape`
is either a `Circle` or a `Rect`. You can simulate this with predicate
dispatch (as shown in the companion document `scheme-types-records-and-mop.md`),
but the type system doesn't know about the relationship.

**No recursive types.** A record field can hold a value that happens to be
another instance of the same record type, but this isn't expressed in the
type definition. There's no formal distinction between a field that *may*
be self-referential and one that *must* be.

## Seeing the Correspondence

Let's trace a complete example through both lenses. Imagine a statically-typed
language with a `Point` type:

```
-- Type theory
Point : Type
Point = { x : ℝ, y : ℝ }

-- Introduction
mkPoint : ℝ → ℝ → Point
mkPoint a b = { x = a, y = b }

-- Elimination
proj_x : Point → ℝ
proj_y : Point → ℝ

-- β-rules
proj_x (mkPoint a b) = a
proj_y (mkPoint a b) = b

-- η-rule
mkPoint (proj_x p) (proj_y p) = p
```

Now the Scheme version:

```scheme
;; "Type declaration" (generative, nominal)
(define-record-type <point>
  (make-point x y) point? (x point-x) (y point-y))

;; Introduction
(define p (make-point 3.0 4.0))

;; Elimination
(point-x p)  ; => 3.0
(point-y p)  ; => 4.0

;; β-rule
(point-x (make-point 3.0 4.0))  ; => 3.0

;; η-rule (extensionally)
(let ((q (make-point (point-x p) (point-y p))))
  (and (= (point-x p) (point-x q))
       (= (point-y p) (point-y q))))  ; => #t
```

The *structure* is identical. The *difference* is where the type information
lives:

| Aspect | Static (ML, Haskell) | Dynamic (Scheme) |
|--------|---------------------|------------------|
| Type membership | Compiler judgment | Runtime predicate |
| Field type constraints | Enforced at compile time | Convention only |
| Nominality | Declaration creates distinct type | `define-record-type` is generative |
| β-rule | Compiler can optimize away | Runtime reduction |
| η-rule | Compiler can prove equivalence | Holds extensionally, not by `eq?` |

## What Would Break Without Nominal Identity

Suppose records were structural — `point?` returned `#t` for any value with
fields named `x` and `y`. What goes wrong?

```scheme
(define-record-type <cartesian> ...)  ; fields x, y — geographic coordinates
(define-record-type <pixel> ...)      ; fields x, y — screen position

;; With structural typing:
(cartesian? (make-pixel 100 200))  ; => #t  -- disaster
```

A pixel at (100, 200) is not a geographic coordinate. Passing it to a
function that feeds the values into a map projection would silently produce
garbage. Nominal identity catches this: the function demands a `<cartesian>`,
the predicate rejects a `<pixel>`, the error is caught.

This is exactly the argument for `newtype` in Haskell: sometimes two things
have the same representation but different *meaning*, and the type system's
job is to keep meaning-incompatible values apart.

## The Takeaway

Scheme records are **nominally-typed labeled product types** with the typing
judgment deferred to runtime. They satisfy the type-theoretic definition
cleanly:

- Introduction rules (constructor)
- Elimination rules (accessors)
- Computation rules (β and η)
- Nominal generativity (disjointness)

They live in a dynamically-typed universe where every field holds `Value`,
so they provide type *identity* without type *constraints* on fields. This
is a deliberate trade-off: maximum flexibility (any value in any field) at
the cost of pushing type errors to runtime.

If you want the vocabulary: records are the point where Scheme's dynamic
type system makes contact with formal type theory. They're the one construct
in R7RS-small where you're actually *defining a new type* in the
type-theorist's sense of the phrase.

---

See also: [Scheme Types, Records, and the MOP Question](scheme-types-records-mop.md)
for the practical landscape of record systems in Scheme.

Sources:
- Benjamin Pierce, *Types and Programming Languages*, chapters 11 (product types) and 24 (existential types for data abstraction)
- R7RS-small §5.5 (record-type definitions)
- [Nominal type system — Wikipedia](https://en.wikipedia.org/wiki/Nominal_type_system)
- [Type theory — Wikipedia](https://en.wikipedia.org/wiki/Type_theory)
