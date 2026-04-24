# Scheme Types, Records, and the Meta-Object Question

Suppose you're building a Scheme program that models geometric shapes. You want a
`point` type with `x` and `y` fields, and you want a `point3d` that adds a `z`
field. You want a predicate `point?` that recognizes both. Maybe you want to write
a generic `distance` function that works differently for 2D and 3D points.

In Java or Python, you'd reach for classes and inheritance. In Go, you'd use struct
embedding and interfaces. What does Scheme give you?

The short answer: less than you might expect from a standard, and more than you
might expect from the ecosystem. Let's trace the path from "nothing" to "full
meta-object protocol" and see where the standards draw their lines.

## The Baseline: Disjoint Record Types (R7RS-small, SRFI-9)

R7RS-small gives you exactly one tool: `define-record-type`. It creates a new type
that is *disjoint* from every other type — including other record types.

```scheme
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define p (make-point 3 4))
(point? p)    ; => #t
(point-x p)   ; => 3
(vector? p)   ; => #f  -- disjoint from all other types
```

This is what Wile implements today. You get:

- A **constructor** (`make-point`)
- A **predicate** (`point?`)
- **Accessors** (`point-x`, `point-y`)
- Optionally, **mutators** (if you write `(x point-x set-point-x!)`)

What you *don't* get is any relationship between record types. If you define
`<point3d>` separately, it has nothing to do with `<point>`. The predicate `point?`
returns `#f` for a `point3d` instance. There is no inheritance, no subtyping, no
shared structure.

This is SRFI-9 (1999), adopted verbatim into R7RS-small. It is deliberately
minimal — the philosophy is "give users a way to make new disjoint types; leave
everything else to libraries and future standards."

## Why No Inheritance? The Philosophical Split

This isn't an oversight. It reflects a deep tension in Scheme's design philosophy.

**Records are nominal types.** A `<point>` is a `<point>` because you said so at
definition time, not because it has fields named `x` and `y`. Two record types
with identical fields are still different types. This is the opposite of structural
typing (where shape determines compatibility).

The question of inheritance is: should nominal types form hierarchies? The Scheme
community has been arguing about this since the 1990s, and the argument produced
*at least seven competing SRFIs* before anyone tried to reconcile them.

The core disagreement:

1. **Minimalists** (R7RS-small, SRFI-9): Records are just data. If you want
   polymorphism, use generic procedures that dispatch on predicates. Inheritance
   creates coupling. Keep it simple.

2. **Pragmatists** (SRFI-99, SRFI-136): Single inheritance for records is useful
   and doesn't require an object system. An AST node type that extends a base node
   is natural and safe.

3. **Maximalists** (R6RS, SRFI-240): Full-featured records with inheritance,
   sealing, opacity, nongenerativity, procedural construction — the whole toolkit,
   because real programs need it.

4. **Object-oriented** (Tiny CLOS, GOOPS, SRFI-20): Records are the wrong
   abstraction. What you really want is generic functions with multiple dispatch
   and a meta-object protocol.

Each camp has a point. Let's look at what each layer actually provides.

## Layer 1: Procedural Records (SRFI-99, Wile)

Beyond the syntactic `define-record-type`, there's a procedural layer that lets you
create record types at runtime. Wile implements this:

```scheme
(define rtd (make-record-type 'color '(r g b)))
(define make-color (record-constructor rtd '(r g b)))
(define color? (record-predicate rtd))
(define color-r (record-accessor rtd 'r))
(define set-color-r! (record-modifier rtd 'r))

(define c (make-color 255 128 0))
(color-r c)          ; => 255
(set-color-r! c 200) ; => mutates
(record? c)          ; => #t
(record-type c)      ; => #<record-type:color>
(record-type? rtd)   ; => #t
```

The procedural layer is important because it makes record types *first-class
values*. You can pass an RTD (record-type descriptor) to a function, store it in a
data structure, and construct records dynamically. This is what makes frameworks
and code generators possible.

SRFI-99 organizes this into three sub-layers:

| Layer | Purpose | Key procedures |
|-------|---------|----------------|
| **Syntactic** | Convenience macros | `define-record-type` |
| **Procedural** | Runtime type creation | `make-record-type`, `record-constructor`, `record-predicate`, `record-accessor`, `record-modifier` |
| **Inspection** | Querying type structure | `record?`, `record-type`, `record-type?` |

Wile provides all three layers, but without inheritance in any of them.

## Layer 2: Record Inheritance (SRFI-99, SRFI-136, R6RS)

This is where things get interesting — and contentious.

### SRFI-99 / SRFI-131: ERR5RS Style

SRFI-99 extends `make-record-type` with a parent argument:

```scheme
;; NOT in Wile today — showing the SRFI-99 API
(define point-rtd (make-rtd 'point #f '(x y)))
(define point3d-rtd (make-rtd 'point3d point-rtd '(z)))

;; point3d inherits x and y, adds z
;; (record-predicate point-rtd) returns #t for point3d instances
```

The syntactic layer adds a parent clause to `define-record-type`:

```scheme
;; SRFI-99 / SRFI-131 syntax (not R7RS-small)
(define-record-type point3d (point x y z)  ; parent is point
  point3d?
  (z point3d-z))
```

SRFI-131 is a reduced version of SRFI-99's syntactic layer that can be implemented
purely with `syntax-rules` — no low-level macro system required.

### SRFI-136: Extensible Record Types

SRFI-136 takes a more conservative approach. It extends R7RS records with
inheritance but adds safety guarantees:

- Immutable fields in a parent *stay immutable* in subtypes
- Subtypes cannot override parent fields
- Subtypes cannot change which parent fields the constructor initializes

These constraints prevent a subtype from violating invariants established by the
parent type — a real concern in Scheme, where there's no access-control syntax
(`private`, `protected`).

### R6RS: The Full Toolkit

R6RS went furthest. Its record system adds four properties that SRFI-9 lacks:

| Property | What it does | Why you'd want it |
|----------|-------------|-------------------|
| **Inheritance** | Single-parent type hierarchies | AST nodes, protocol messages, domain models |
| **Sealed** | Prevents subtypes | Guarantee that your dispatch cases are exhaustive |
| **Opaque** | Hides from `record?` inspection | Implement built-in-looking types; enforce abstraction |
| **Nongenerative** | Same UID = same type across evaluations | Local record definitions with lexical scoping that don't create a new type each time |

```scheme
;; R6RS style (not R7RS-small, not Wile)
(define-record-type point
  (fields (immutable x) (immutable y)))

(define-record-type point3d
  (parent point)
  (fields (immutable z)))

;; point? recognizes point3d instances
;; point-x works on point3d instances
```

The R6RS approach is powerful but divisive. The standard was controversial enough
that several Scheme implementers voted against ratifying it, citing complexity in
the record system as one reason. R7RS-small deliberately retreated to SRFI-9's
simplicity.

### SRFI-240: The Reconciliation Attempt

SRFI-240 ("Reconciled Records") tries to end the argument by defining a
`define-record-type` that extends R7RS-small's syntax to include R6RS features.
It's designed for R7RS-large adoption. As of 2026, it's still in draft status —
which tells you something about how hard consensus is in this space.

## Layer 3: Object Systems and the MOP Question

Now we get to your actual question: does Scheme have meta-objects for types?

**The standard answer: no.** No Scheme standard (R5RS, R6RS, R7RS-small, or any
finalized R7RS-large draft) defines an object system with a meta-object protocol.

**The ecosystem answer: yes, several.** But they're all non-standard extensions.

### What Is a Meta-Object Protocol?

A meta-object protocol (MOP) lets you treat the *machinery of the type system
itself* as objects you can inspect and customize. In a MOP:

- Classes are objects (instances of metaclasses)
- Generic functions are objects
- Method dispatch is itself a method you can override
- You can change how instances are allocated, initialized, and represented

The canonical example is CLOS (Common Lisp Object System), designed by Gregor
Kiczales and others, documented in *The Art of the Metaobject Protocol* (1991).
CLOS gives you:

- **Multiple dispatch**: methods specialize on *all* arguments, not just "self"
- **Multiple inheritance**: with a linearized class precedence list (C3)
- **Generic functions**: methods don't belong to classes
- **Full MOP**: metaclasses, custom slot allocation, method combinations

### Tiny CLOS: The Scheme Bridge

Kiczales himself wrote **Tiny CLOS** — a portable Scheme implementation of CLOS's
core ideas. It's small (~500 lines), bootstraps its own class hierarchy, and
provides:

```scheme
;; Tiny CLOS style (not standard Scheme)
(define <point>
  (make-class (list <object>)
              (list (list 'x) (list 'y))))

(define <point3d>
  (make-class (list <point>)
              (list (list 'z))))

;; Generic function with methods
(define distance (make-generic))

(add-method distance
  (make-method (list <point>)
    (lambda (call-next-method p)
      (sqrt (+ (sqr (slot-ref p 'x))
               (sqr (slot-ref p 'y)))))))

(add-method distance
  (make-method (list <point3d>)
    (lambda (call-next-method p)
      (sqrt (+ (sqr (slot-ref p 'x))
               (sqr (slot-ref p 'y))
               (sqr (slot-ref p 'z)))))))
```

Tiny CLOS became the ancestor of:

- **GOOPS** (Guile) — full MOP, production quality
- **STklos** — directly derived from Tiny CLOS
- **Swindle** (Racket) — CLOS-style system for Racket
- **SRFI-20** — an attempt to standardize a simple object system (based on Meroon-V3, itself CLOS-inspired)

### Why Scheme Doesn't Standardize a MOP

The MOP question keeps being deferred for philosophical reasons:

1. **Scheme prefers procedures over methods.** Generic dispatch in Scheme is
   typically done with `cond`/`case` on predicates, not method tables. This is
   simpler and explicit.

2. **Records + closures cover most use cases.** If a `<point>` carries a closure
   that computes its distance, you don't need generic dispatch — you need a field.

3. **A MOP is a language within a language.** CLOS's MOP is beautiful engineering,
   but it's also an enormous surface area. Standardizing it means every Scheme
   implementation must carry it.

4. **Multiple dispatch changes everything.** Once methods don't belong to classes,
   the entire mental model of "objects" shifts. That's a big commitment for a
   standard.

## The Practical Landscape for Wile

Here's where Wile sits today:

| Feature | Status |
|---------|--------|
| `define-record-type` (R7RS/SRFI-9) | Implemented |
| Procedural layer (`make-record-type`, accessors, modifiers) | Implemented |
| Inspection layer (`record?`, `record-type`, `record-type?`) | Implemented |
| Opaque record types (`define-opaque-record-type`, `make-opaque-record-type`) | Implemented |
| Record inheritance (Scheme-level API) | Not implemented (internal `NewDerivedRecordType` exists but is not exposed) |
| Sealed/nongenerative | Not implemented |
| Object system / MOP | Not implemented |

For custom subtypes, you currently have these options in Wile:

### Option A: Manual Dispatch (Idiomatic Scheme)

```scheme
(define-record-type <shape>
  (make-shape kind data)
  shape?
  (kind shape-kind)
  (data shape-data))

(define (make-circle r) (make-shape 'circle r))
(define (make-rect w h) (make-shape 'rect (cons w h)))

(define (area s)
  (case (shape-kind s)
    ((circle) (* 3.14159 (expt (shape-data s) 2)))
    ((rect) (* (car (shape-data s)) (cdr (shape-data s))))))
```

This is the classic Scheme approach: tagged data with explicit dispatch. It's
simple, debuggable, and works today. The downside is that adding a new shape
requires modifying `area`.

### Option B: Closure-Based Dispatch

```scheme
(define-record-type <shape>
  (make-shape area-fn)
  shape?
  (area-fn shape-area-fn))

(define (make-circle r)
  (make-shape (lambda () (* 3.14159 r r))))

(define (make-rect w h)
  (make-shape (lambda () (* w h))))

(define (area s) ((shape-area-fn s)))
```

This is the "objects are closures" approach. Adding a new shape doesn't require
modifying `area`. The downside is that adding a new *operation* (e.g., `perimeter`)
requires modifying every constructor.

> This is exactly the expression problem: tagged dispatch is easy to extend with
> new operations but hard to extend with new variants; closure dispatch is the
> reverse.

### Option C: Separate Record Types + Predicate Dispatch

```scheme
(define-record-type <circle> (make-circle r) circle? (r circle-r))
(define-record-type <rect> (make-rect w h) rect? (w rect-w) (h rect-h))

(define (area s)
  (cond
    ((circle? s) (* 3.14159 (expt (circle-r s) 2)))
    ((rect? s) (* (rect-w s) (rect-h s)))
    (else (error "area: unknown shape" s))))
```

Each type is fully independent. No hierarchy, no shared fields, full type safety.
This is the approach most Scheme programs actually use in practice.

## What Would Break Without These Features?

For most programs: nothing. The Scheme community has been writing substantial
software with flat records and predicate dispatch for decades. The programs that
genuinely *need* record inheritance are the ones with deep type hierarchies —
compilers (AST nodes), protocol implementations (message types), and GUI toolkits
(widget types).

The programs that genuinely need a MOP are rarer still: frameworks that need to
customize how dispatch itself works, or that need reflective access to the type
system for serialization, persistence, or code generation.

## Summary: The Standards Staircase

```
Level 0: Tagged lists           (define point (list 'point 3 4))
Level 1: SRFI-9 / R7RS-small    define-record-type, flat, no inheritance
Level 2: SRFI-99 / SRFI-131     + single inheritance, procedural layer
Level 3: SRFI-136               + inheritance with safety constraints
Level 4: R6RS / SRFI-240        + sealed, opaque, nongenerative
Level 5: Tiny CLOS / GOOPS      + generic functions, multiple dispatch, MOP
```

Wile sits at Level 1 (SRFI-9) with the SRFI-99 procedural/inspection layer
and has also adopted one Level-4 feature à la carte: opaque record types
(`define-opaque-record-type` / `make-opaque-record-type`, R6RS-inspired).
Missing Level-4 features are sealed and nongenerative. The question for
Wile is whether to climb to Level 2 (SRFI-99 inheritance) — a modest,
well-understood extension — or continue picking individual features from
higher levels.

Sources:
- [SRFI-9: Defining Record Types](https://srfi.schemers.org/srfi-9/srfi-9.html)
- [SRFI-99: ERR5RS Records](https://srfi.schemers.org/srfi-99/srfi-99.html)
- [SRFI-131: ERR5RS Record Syntax (reduced)](https://srfi.schemers.org/srfi-131/srfi-131.html)
- [SRFI-136: Extensible Record Types](https://srfi.schemers.org/srfi-136/srfi-136.html)
- [SRFI-240: Reconciled Records](https://srfi.schemers.org/srfi-240/)
- [SRFI-237: R6RS Records (refined)](https://srfi.schemers.org/srfi-237/srfi-237.html)
- [SRFI-20: Simple Object System](https://srfi.schemers.org/srfi-20/srfi-20.html)
- [R6RS Records Library](https://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-7.html)
- [Guile GOOPS Manual](https://www.gnu.org/software/guile/manual/html_node/GOOPS.html)
- [Guile Metaobject Protocol](https://www.gnu.org/software/guile/manual/html_node/The-Metaobject-Protocol.html)
- [Tiny CLOS Tutorial](https://home.adelphi.edu/sbloch/class/archive/272/spring1997/tclos/tutorial.html)
- [Kiczales et al., *The Art of the Metaobject Protocol*](https://books.google.com/books?id=3X5Gnudn3k0C)
- [Scheme Object Systems (community wiki)](https://groups.scheme.org/objects/)
