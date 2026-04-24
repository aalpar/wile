# Abstract Data Types and Existential Types

In the [previous document](records-as-formal-types.md), we saw that records are
nominally-typed labeled product types. You get a constructor, accessors, a
predicate. Clean correspondence with type theory. But there's a problem.

## The Problem: Records Don't Hide Anything

```scheme
(define-record-type <stack>
  (raw-make items)
  stack?
  (items stack-items))

(define (make-stack) (raw-make '()))
(define (push s v)   (raw-make (cons v (stack-items s))))
(define (pop s)      (values (car (stack-items s))
                             (raw-make (cdr (stack-items s)))))
```

You *intend* the representation to be a list. You *intend* `make-stack` to be
the only constructor and `push`/`pop` the only operations. But nothing
enforces this:

```scheme
;; Anyone can peek at the representation
(stack-items (push (make-stack) 42))  ; => (42)

;; Anyone can construct an invalid stack
(define bad (raw-make "not-a-list"))
(stack? bad)  ; => #t -- the type system says this is a stack!
```

The record gives you **nominal identity** (a stack is not a list, not a vector,
not any other record type). But it doesn't give you **abstraction** — the
ability to hide the representation so that clients can only interact through
the operations you provide.

This is the difference between a **concrete type** and an **abstract data
type**. And it turns out to have a precise formalization: existential types.

## Abstract Data Types, Informally

An abstract data type (ADT) is a type defined by its *operations*, not its
*representation*. When you use a stack, you care that:

- `make-stack` gives you an empty stack
- `push` adds an element
- `pop` removes the most recent element
- `empty?` tells you if there are no elements

You don't care — and shouldn't know — whether the stack is implemented as a
list, a vector, a tree, or a linked chain of closures. The representation is
the implementor's business, not the client's.

This is a familiar idea from software engineering ("information hiding,"
"encapsulation"). What's less familiar is that type theory gives it a precise
formulation using **existential types**.

## Existential Types: The Formal Mechanism

In type theory, a **universal type** (∀) says "this works for *any* type you
give me." A **existential type** (∃) says "there *exists* some type, but I
won't tell you which one."

A universally-quantified function:

```
∀T. T → T          -- "give me any type, I'll hand back a value of that type"
                    -- example: the identity function
```

An existentially-quantified package:

```
∃T. { new  : Unit → T,
      push : T → Value → T,
      pop  : T → Value × T,
      empty: T → Bool }
```

Read that as: "There exists some type T, and I'm giving you four operations
that work with T. You can use these operations. You cannot inspect T directly.
You don't know what T is."

This is the Mitchell-Plotkin (1988) formulation: **abstract data types are
existential types.** The representation type T is *hidden* — the only way to
interact with values of type T is through the provided operations. The type
system itself enforces the abstraction barrier.

### Introduction and Elimination (Again)

Existential types have their own introduction and elimination rules, and this
is where the mechanism becomes concrete:

**Introduction** (packing): the implementor *packs* a concrete representation
with its operations:

```
pack { T = List,
       new   = λ(). [],
       push  = λ(s, v). cons(v, s),
       pop   = λ(s). (car(s), cdr(s)),
       empty = λ(s). null?(s)
     } as ∃T. { ... }
```

The implementor knows T = List. But after packing, that information is sealed
inside the package.

**Elimination** (unpacking): the client *opens* the package and gets a
type variable and the operations, but **not** the concrete type:

```
open stack_package as { T, ops } in
  let s = ops.new()
  let s2 = ops.push(s, 42)
  ops.empty(s2)              -- this is fine
  car(s2)                    -- TYPE ERROR: s2 has type T, not List
```

The client knows `s2 : T` for some unknown `T`. It can pass `s2` to `ops.push`
or `ops.pop` (which accept `T`), but it cannot call `car` on it (which requires
`List`). The abstraction is enforced by the type system: **T is opaque**.

## How Scheme Approximates This

Scheme doesn't have existential types in the type system — it's dynamically
typed. But it achieves the same effect through two mechanisms, each
corresponding to one of the formal operations:

### Mechanism 1: Module Boundaries (Export Control)

A Scheme library can define a record type internally and export only the
operations, not the accessor or raw constructor:

```scheme
(define-library (my-project stack)
  (export make-stack push pop empty?)      ; public operations
  ;; NOT exported: raw-make, stack-items   ; representation hidden
  (begin
    (define-record-type <stack>
      (raw-make items) stack? (items stack-items))
    
    (define (make-stack) (raw-make '()))
    (define (push s v)   (raw-make (cons v (stack-items s))))
    (define (pop s)      (values (car (stack-items s))
                                 (raw-make (cdr (stack-items s)))))
    (define (empty? s)   (null? (stack-items s)))))
```

A client that `(import (my-project stack))` gets `make-stack`, `push`, `pop`,
`empty?`. They cannot call `stack-items` or `raw-make` — those names aren't
in scope. The module boundary acts as the existential *pack*: the
representation is sealed inside.

This is exactly how ML modules work (signatures hide structure components),
just enforced by name resolution rather than by a type checker.

### Mechanism 2: Closures (The Lambda Encoding)

You can also hide representation without any module system at all, using
closures:

```scheme
(define (make-stack-package)
  (define-record-type <stack>
    (raw-make items) stack? (items stack-items))
  
  (define (make-stack) (raw-make '()))
  (define (push s v)   (raw-make (cons v (stack-items s))))
  (define (pop s)      (values (car (stack-items s))
                               (raw-make (cdr (stack-items s)))))
  (define (empty? s)   (null? (stack-items s)))
  
  ;; Return only the operations — representation captured in closures
  (list make-stack push pop empty?))
```

Each call to `make-stack-package` creates a *fresh* record type (because
`define-record-type` is generative) and returns closures over it. The caller
gets operations but has no access to `stack-items` or `raw-make`. The closures
*close over* the representation — that's the existential pack, encoded in
lambda calculus.

This is not just an analogy. There's a precise formal result: **existential
types are encodable as universal types in System F** (the polymorphic lambda
calculus), and the encoding uses exactly this closure pattern. Scheme's
closures are doing the same job that System F's type abstraction does, just
without static type checking to enforce it.

## The Correspondence, Precisely

| Formal concept | ML / Haskell | Scheme |
|---------------|--------------|--------|
| Existential pack | Module structure matching signature | Library with limited exports |
| Existential open | Functor application / import | `(import ...)` |
| Representation type T | Abstract type in signature | Record type (unexported) |
| Operations over T | Functions in structure | Exported procedures |
| Opacity enforcement | Type checker | Lexical scoping |
| Client access to T | Type error at compile time | Name not in scope at expand time |

The strength differs: ML catches abstraction violations at compile time;
Scheme catches them at macro expansion time (unexported names fail to resolve).
Both prevent the client from depending on the representation. But Scheme's
enforcement is weaker — nothing stops a client from probing with `record?`
and `record-type` inspection procedures, if the record system provides them.

## R6RS Opaque Records: Closing the Gap

R6RS addresses this with **opaque records**. An opaque record type hides its
identity from the inspection layer:

```scheme
;; R6RS style (not R7RS-small)
(define-record-type stack
  (opaque #t)
  (fields (immutable items)))
```

With opacity, `(record? s)` returns `#f` for stack instances. The only way
to recognize a stack is through its predicate — and if the predicate isn't
exported, *there is no way.* This is the Scheme analog of sealing the
existential: the representation type is truly hidden, not just unexported.

> **Where Wile is:** Wile provides opaque record types via
> `define-opaque-record-type` (and the procedural `make-opaque-record-type`)
> using R7RS-compatible syntax, not R6RS's `(opaque #t)` clause:
>
> ```scheme
> (define-opaque-record-type <stack>
>   (raw-make items) stack? (items stack-items))
> ```
>
> Instances of opaque record types return `#f` from `record?`, and
> `record-type` signals an error on them — the type is sealed against
> generic inspection. Type-specific predicates and accessors still work,
> so the operations the implementor exports continue to function for
> clients.

## Why This Matters

The ADT / existential-type perspective reframes what "good API design" means
in formal terms:

**A well-designed module is an existential package.** It hides a
representation type and exports operations over it. Clients program against
the operations, not the representation. This is what makes it possible to
change the representation (list → vector → tree) without breaking clients.

**Records alone are not ADTs.** They're *concrete* types — product types
with all their internals exposed. Records become ADTs only when combined
with an abstraction mechanism (modules, closures, or opacity).

**The abstraction barrier is the existential quantifier.** Every time you
decide "I'll export this but not that," you're performing the type-theoretic
operation of existential packing. The formalism isn't decoration — it tells
you exactly what the barrier guarantees and where it can leak.

The hierarchy, from least to most abstract:

```
Product type            { x : A, y : B }           all fields visible
  ↓ + nominal identity
Record type             define-record-type          disjoint, but fields exposed
  ↓ + export control
Module-hidden record    library with limited exports name-based hiding
  ↓ + opacity
Opaque record           R6RS opaque #t              inspection-proof hiding
  ↓ + static checking
Existential type        ML abstract type            compiler-enforced hiding
```

Each level adds one more layer of information hiding. Scheme records start
at the second level. How far up the ladder you climb depends on how much
abstraction your design requires.

---

See also:
- [Records as Formal Types](records-as-formal-types.md) — introduction/elimination
  rules for records
- [Scheme Types, Records, and the MOP Question](scheme-types-records-mop.md) —
  practical landscape of record systems

Sources:
- Mitchell & Plotkin, "Abstract Types Have Existential Type" (1988)
- Cardelli & Wegner, "On Understanding Types, Data Abstraction, and Polymorphism" (1985)
- Pierce, *Types and Programming Languages*, chapter 24 (existential types)
- R6RS §6.2 (record layer, opacity, sealing)
- R7RS-small §5.5 (record-type definitions)
