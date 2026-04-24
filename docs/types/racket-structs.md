# Racket Structs: Records That Grew Up

You know `define-record-type` from R7RS. It gives you a constructor, a predicate,
and accessors. That's it. No inheritance, no protocols, no information hiding beyond
what your module exports. Racket's `struct` starts from the same idea — named
fields, a predicate — but then asks: *what if the type descriptor itself could carry
behavior?*

The result is a system that does things R7RS records can't even express.

## Starting Simple

A Racket struct declaration looks deceptively familiar:

```racket
(struct point (x y))

(define p (point 3 4))
(point? p)    ; => #t
(point-x p)   ; => 3
```

So far this is just `define-record-type` with less syntax. The constructor is the
struct name itself (`point`), the predicate is `point?`, and the accessors are
`point-x`, `point-y`. But three things are already different from R7RS records, and
they matter.

## Difference 1: Opacity by Default

Print that point:

```racket
(point 3 4)   ; => #<point>
```

You get `#<point>`. Not `#<point: 3 4>`. Not `(point 3 4)`. The fields are
*invisible*. If you don't export `point-x` from your module, nobody can read the x
coordinate. Nobody can pattern-match on it. Nobody can even tell two points apart
(since `equal?` on opaque structs is reference equality).

This is the default. To make fields visible, you opt in:

```racket
(struct point (x y) #:transparent)

(point 3 4)   ; => (point 3 4)
```

Racket gives you three levels:

| Level | Keyword | Who can inspect? | Who can construct? |
|-------|---------|-------------------|--------------------|
| **Opaque** | *(default)* | Only code with the accessors | Only code with the constructor |
| **Transparent** | `#:transparent` | Anyone (fields visible in printing, `equal?` is structural) | Only code with the constructor (but guard validates) |
| **Prefab** | `#:prefab` | Anyone | Anyone, including the reader — `'#s(point 3 4)` is a literal |

Why does this matter? Because it lets a library author control the abstraction
boundary. If you export `point?` and `point-x` but not the constructor, your type
is a true abstract data type — clients can test and read, but can't forge instances.
R7RS records give you this only if your module system cooperates (which it usually
does), but Racket makes it a property of the *type itself*.

> **Where Wile is:** Wile's `NewOpaqueRecordType` implements the opaque level.
> Opaque records print as `#<point>` instead of `#<record:point>`, and they're
> invisible to `record?` — making them behave like built-in types rather than
> user-defined records. This is the same design goal as Racket's default opacity,
> though Wile doesn't have the transparent/prefab tiers.

## Difference 2: Inheritance

Racket structs support single inheritance:

```racket
(struct point (x y) #:transparent)
(struct point3d point (z) #:transparent)

(define p (point3d 1 2 3))
(point? p)      ; => #t    -- a point3d IS a point
(point-x p)     ; => 1     -- parent accessors work
(point3d-z p)   ; => 3     -- child accessor works
```

The subtype constructor takes parent fields first, then child fields. The parent
predicate recognizes child instances. Parent accessors work on child instances. This
is straightforward single inheritance — nothing exotic.

But here's the interesting part: the subtype can be opaque even if the parent is
transparent, or vice versa. And a subtype's guard runs *after* the parent's guard.
The type hierarchy is a chain of validation steps:

```racket
(struct point (x y)
  #:guard (lambda (x y name)
            (unless (real? x) (error name "x must be real"))
            (unless (real? y) (error name "y must be real"))
            (values x y)))

(struct point3d point (z)
  #:guard (lambda (x y z name)
            (unless (real? z) (error name "z must be real"))
            (values x y z)))

(point3d 1 2 "three")  ; => error: point3d: z must be real
```

R7RS has no equivalent to guards. Construction is unchecked — you can put anything
in any field. If you want validation, you write a wrapper procedure that checks
before calling the real constructor. Racket makes validation part of the type.

## Difference 3: Struct Type Properties (The Big One)

This is where Racket structs fundamentally diverge from anything in the Scheme
record tradition. A struct type can carry *properties* — key-value pairs attached to
the type itself, not to instances. Properties are the mechanism for making structs
participate in protocols.

The most common example is making a struct callable:

```racket
(struct greeter (greeting)
  #:property prop:procedure
  (lambda (self name)
    (string-append (greeter-greeting self) ", " name "!")))

(define g (greeter "Hello"))
(g "World")   ; => "Hello, World!"
```

The `greeter` struct has one field (`greeting`), but instances are also procedures.
When you call `(g "World")`, Racket looks up `prop:procedure` on the struct type,
finds the lambda, and invokes it.

Other built-in properties:

| Property | What it does |
|----------|-------------|
| `prop:procedure` | Makes instances callable |
| `prop:equal+hash` | Custom `equal?` and hashing |
| `prop:custom-write` | Custom printing |
| `prop:evt` | Makes instances synchronizable (like Go channels) |
| `prop:input-port` / `prop:output-port` | Makes instances usable as ports |
| `prop:sequence` | Makes instances iterable with `for` loops |

And you can define your own properties. This is the key: **struct properties are
Racket's answer to interfaces.** In Go, you'd write:

```go
type Stringer interface {
    String() string
}
```

and any type with a `String()` method satisfies it. In Racket, you'd define a struct
property:

```racket
(define-values (prop:stringable stringable? stringable-ref)
  (make-struct-type-property 'stringable))
```

and any struct type with `#:property prop:stringable <value>` satisfies it. The
`stringable-ref` procedure extracts the property value from an instance. Subtypes
inherit parent properties and can override them.

This is a fundamentally different design from R7RS records, which have no notion of
per-type metadata at all. A record type in R7RS is just a name and a list of
fields. A struct type in Racket is a name, fields, *and an extensible set of
behaviors*.

## Why This Design?

Racket uses struct properties pervasively because they solve a problem that records
can't: **extending behavior without modifying dispatch code.**

Consider: in R7RS, if you want `equal?` to work on your record type, you have to
hope the implementation's `equal?` already does structural comparison on records
(most do, but R7RS doesn't require it). If you want custom printing, you're at the
mercy of the implementation. If you want your type to be usable as a port — that's
simply impossible.

In Racket, all of these are just properties. The `equal?` implementation checks for
`prop:equal+hash`. The printer checks for `prop:custom-write`. The port system
checks for `prop:input-port`. Your struct type opts into each protocol independently,
and the core system discovers the behavior through property lookup.

This is the expression problem solved for the *language runtime itself*: new types
can participate in existing protocols without modifying the protocol implementations.

## Generativity and Prefab

One more distinction worth understanding. In Racket (and R7RS), each `struct`
evaluation creates a *new* type:

```racket
(define (make-a-point-type)
  (struct point (x y))
  point)

(define Point1 (make-a-point-type))
(define Point2 (make-a-point-type))

((Point1 3 4) . point? . Point2)  ; would be #f -- different types!
```

This is **generativity** — the type's identity comes from the act of creating it,
not from its shape. Two struct declarations with identical names and fields produce
incompatible types. This is useful for abstraction (a library's internal `<node>`
type can't be confused with another library's `<node>`) but makes serialization
hard: you can't reconstruct a type from its printed form.

Racket's `#:prefab` solves this:

```racket
(struct point (x y) #:prefab)

;; This is a literal -- the reader can parse it
'#s(point 3 4)
```

Prefab structs are non-generative — the type is determined entirely by its name and
field count. Any module can create a `point` prefab, and they're all the same type.
The trade-off: prefab structs can't have guards, can't have properties, and can't be
opaque. They're just data.

R7RS records are generative (each `define-record-type` creates a new type) but have
no prefab equivalent. You can't write a record literal in source code.

## The Design Space at a Glance

```
                        R7RS records        Racket structs
                        ───────────         ──────────────
Fields                  named, typed        named, typed
Predicate               yes                 yes
Inheritance             no (SRFI-136 adds)  single, built-in
Opacity                 always opaque*      opaque/transparent/prefab
Guards                  no                  yes (constructor validation)
Properties              no                  yes (per-type protocols)
Generativity            always generative   generative or prefab
Mutability              per-field opt-in    per-field opt-in (immutable default)
First-class descriptor  SRFI-99 (RTD)       yes (struct-type-info)
Callable instances      no                  yes (prop:procedure)
Custom equality         no                  yes (prop:equal+hash)
Custom printing         no                  yes (prop:custom-write)

* R7RS doesn't specify print representation; most implementations print fields
```

## What Would Break Without Properties?

Imagine implementing a pattern-matching library for Racket without struct properties.
Your `match` macro needs to destructure structs — but structs are opaque by default.
Without `prop:match-expander` (a custom property), `match` would need to know about
every struct type at compile time, or every struct would need to be transparent.

With properties, any struct can opt into pattern matching by attaching the right
property. The `match` macro checks for the property and uses it to generate the
destructuring code. New struct types participate in `match` without `match` knowing
they exist.

This is why Racket's struct system is central to the language in a way that R7RS
records are not. Records are a data definition mechanism. Structs are a *type
definition* mechanism — they define not just shape, but behavior.

## Further Reading

- [Racket Guide: Programmer-Defined Datatypes](https://docs.racket-lang.org/guide/define-struct.html)
- [Racket Reference: Structures](https://docs.racket-lang.org/reference/structures.html)
- [Racket Reference: Struct Type Properties](https://docs.racket-lang.org/reference/structprops.html)
- [scheme-types-records-mop.md](scheme-types-records-mop.md) — Scheme record standards staircase, SRFI landscape, and MOP question
- [records-as-formal-types.md](records-as-formal-types.md) — Records as nominally-typed labeled product types (introduction/elimination rules)
- [abstract-data-types.md](abstract-data-types.md) — Records as existential types and ADTs
- [SRFI-136: Extensible record types](https://srfi.schemers.org/srfi-136/srfi-136.html) — Conservative R7RS inheritance extension
