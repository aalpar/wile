# R7RS Semantic Differences

This document catalogs differences between the current implementation and the R7RS-small specification. These are semantic differences where the implementation produces results but may not match R7RS behavior for certain inputs.

**Reference:** [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf)

---

## Summary

18 known differences exist:
1. Non-blocking I/O detection (`char-ready?`, `u8-ready?`) always returns `#t`. Conservative safe behavior with minimal practical impact.
2. `parameterize` uses continuation marks instead of `dynamic-wind`. This fixes composable continuation bugs at the cost of a minor semantic difference when mutating parameters via `(p val)` inside `parameterize`.
3. `set-current-directory!` changes the process-global working directory via `os.Chdir`, which is inherently shared across all Wile engines and goroutines in the same OS process.
4. Vector and bytevector literals are **immutable** — mutating one **raises an error** (R7RS permits but does not require this detection), matching immutable string literals. Pair literals are **not** detected: the flag would have to grow the cons cell, and R7RS does not require the detection.
5. **Default** (opt out with `WithMutableTopLevel`): a defined-once, never-`set!`-in-unit top-level `define` in the user program is immutable, so a later `set!` **raises an error**, and code already compiled against a sealed base binding does not observe a later shadowing re-`define` (Chez two-environment model). User-loaded libraries stay mutable. Use `WithMutableTopLevel()` for strict R7RS top-level mutability.
6. Importing one identifier from two libraries with **different** bindings **raises an error** (`ErrDuplicateBinding`) rather than silently letting the last import win. R7RS §5.6 makes this "an error" (undefined) but does not require signalling; Wile signals it, matching Chez/Racket. Re-export diamonds and repeated imports stay legal.
7. Delivering a number of values other than one into a **single-value slot** (a call argument, a `let` init, a `define` or `set!` operand) **raises an error**, whether those values come from `(values …)` or from invoking a continuation. R7RS §6.10 leaves the ≠1-value case **unspecified** for continuations not made by `call-with-values`, so this is a choice within unspecified territory; it matches Chez and Racket. `call-with-values` and the other multiple-value receivers still accept 0, 1 or N. **Changed:** Wile previously spliced the values into the slot.
8. `current-second` returns POSIX/Unix time, not TAI. R7RS §6.13.2 specifies TAI (International Atomic Time); Wile returns seconds since the Unix epoch (leap seconds excluded), which trails TAI by a fixed offset (37 s as of 2017). A portable leap-second table is maintenance overhead with little practical benefit, so the deviation is documented rather than corrected.
9. `equal?` is **structural** on records, hashtables, and boxes, where Chez and Racket answer `#f` for distinct objects. R7RS §6.1 permits either — records fall under "in all other cases, `equal?` may return either `#t` or `#f`" — so this is a deliberate choice, not a deviation from the spec. It is a deviation from most other Schemes, which is why it is listed here. **Item 14 narrows this for hashtables specifically.**
10. Procedure calls evaluate **strictly left to right**, operator before operands, and `let` evaluates its inits in written order. R7RS §4.1.3 leaves that order **unspecified**, so the guarantee is stricter than the standard requires: a program that relies on it does not port to an implementation that evaluates right to left.
11. `(eqv? +nan.0 +nan.0)` returns `#t`. R7RS §6.1 makes this **explicitly unspecified** ("As an exception, the behavior of `eqv?` is unspecified when both `obj1` and `obj2` are NaN"), so this is a choice within unspecified territory, matching Chez and Racket. Numeric `=` keeps IEEE-754 semantics: `(= +nan.0 +nan.0)` is still `#f`.
12. `(rnrs hashtables)` is provided, with one gap: `make-hashtable` accepts only
    the built-in `(equal-hash, equal?)` pair, recognized by **primitive
    identity** — each argument must be a closure the registry built from the
    `equal-hash` / `equal?` spec, whichever environment minted it
    (`machine.PrimitiveIdentity`). A user-supplied hash or equivalence
    procedure, including an embedder's own primitive registered under the same
    name, raises `ErrUnsupportedHashtableKind`. R6RS's condition system is not
    implemented, so every R6RS "raises `&assertion`" here raises a Wile sentinel
    instead (`ErrImmutableHashtable`, `ErrUnsupportedHashtableKind`) —
    matchable with `errors.Is` from Go and by the standard exception machinery
    from Scheme.

    A **closure-pointer** compare was the first design and did not survive
    contact with `import`. A library environment is a flat island
    (`Namespace.NewChildRuntime` gives it `parent: nil`), so the library env
    factory re-applies the whole registry into it and mints a second closure per
    primitive; `(scheme base)` exports `equal?`, so after the import every
    conforming R7RS program opens with, the program's `equal?` was the library's
    copy while the sealed base still held the engine's, and the pair was
    refused. The identity token is shared by every copy, so it answers "is this
    the registered `equal-hash`?" rather than "is this the sealed base's copy of
    it?" — a question that also stops being answerable once
    `WithStrictNamespace` or a dialect's `PrimitiveRemover` narrows the sealed
    base. `hashtable-hash-function` and `hashtable-equivalence-function` hand
    back the copy visible at the namespace's mutable top level, falling back to
    the sealed base, so the pair a program reads off a table is `eq?` to the
    pair it can write.
13. `make-equal-hashtable` is a non-standard constructor, matching Chez,
    Larceny, Vicare, and Ypsilon. R6RS spells it
    `(make-hashtable equal-hash equal?)`, which Wile also accepts. Prefer
    `make-eq-hashtable` when the keys are objects whose `equal?` **is** identity
    (a record type, a port, a procedure): those all hash to one bucket under
    `equal-hash` and scan linearly, where an eq table hashes them by identity.
14. `equal?` on two hashtables is structural only when both use the **same key
    equivalence** and **every key is a non-container**. Otherwise it is
    identity. Item 9 above states the structural choice; the restriction exists
    because pairing entries across two tables requires an eager key lookup,
    which recurses on the host stack once per hashtable reachable as a key and
    does not terminate on a cycle of them.
15. R6RS **version references are parsed and ignored**. A library name's final
    element may be a list — `(rnrs hashtables (6))`, or a reference such as
    `(srfi :1 (and (>= 1) (< 2)))` — and it is dropped, so the name denotes
    `(rnrs hashtables)`. R7RS name parts are only identifiers and exact
    non-negative integers, so a list in that position is unambiguously R6RS and
    cannot collide with a real part; a list anywhere else is still an error.
    Nothing is matched against, because Wile carries no library version
    metadata: every version reference is vacuously satisfied, and the contents
    are unvalidated for the same reason.
16. The `(rnrs hashtables)` library does **not export**
    `equal-hash`, `string-hash`, `string-ci-hash` or `symbol-hash`, which R6RS
    lists in it — those four are bound in the sealed base and remain callable
    after the import, so this is a completeness gap, not a reachability one. It
    was formerly load-bearing (exporting them broke `make-hashtable`, see item
    12); under identity recognition it no longer is. The one consequence left to
    weigh before closing it is that `(srfi 13)` exports a different, bounded
    `string-hash`, so a program importing both libraries would meet the R7RS
    §5.6 conflict — correctly, but newly.
17. Under `WithDialect(NoMutation)` (opt-in), a `define-record-type` that
    **declares a modifier** fails at **definition** time, not at the modifier's
    first use. The dialect removes `record-modifier`, and
    `bootstrap_macros.scm` expands a modifier-declaring field spec to
    `(define modifier (record-modifier type 'field-tag))`, so the whole
    declaration raises `no such binding "record-modifier" with compatible
    scopes` and the record type never comes into existence — before any
    instance does. A modifier-free `define-record-type` is unaffected. The
    diagnostic names `record-modifier` rather than the modifier the program
    wrote.
18. A splice in the **operand** of an over-deep `unquote` — `` ``(a ,,@x) `` —
    stays **literal**. R7RS §7.1.4 gives `unquote` exactly one operand and types
    it `⟨qq template D−1⟩`, from which `⟨splicing unquotation⟩` is unreachable,
    so the form is either ungrammatical or an ordinary list of data depending on
    how widely the section's precedence note is read; Wile takes the latter. Chez
    and Racket both extend `unquote` to splice there, and **disagree with each
    other** on the multi-operand case, where Racket returns Wile's answer. A
    splice in any **element** position is unaffected and fully conforming.

---

## Non-Blocking I/O Detection

**Affected Primitives:** `char-ready?`, `u8-ready?`

**R7RS §6.13.2 Requirement:**
> Returns `#t` if a character (or byte) is ready on the input port and returns `#f` otherwise. If `char-ready?` returns `#t` then the next `read-char` (or `read-u8`) operation on the given port is guaranteed not to hang.

**Wile Behavior:** Always returns `#t`.

**Rationale:**

Go's `io.Reader` interface does not expose readiness status or non-blocking I/O semantics. Implementing true non-blocking detection would require:
1. OS-specific syscalls (`select`/`poll` on Unix, overlapped I/O on Windows)
2. Platform-specific build tags and dependencies (`golang.org/x/sys/unix`, `golang.org/x/sys/windows`)
3. Handling buffered readers (`bufio.Reader`) where buffered data makes reads non-blocking even when the underlying descriptor would block
4. Significant complexity in the I/O layer with cross-platform maintenance burden

The conservative behavior (always returning `#t`) is **safe**: it may cause blocking where R7RS code expected non-blocking, but never claims data is available when it isn't (which would violate R7RS guarantees).

**Workaround:**

Read on a dedicated thread and hand the result back through a shared slot:

```scheme
;; Instead of polling with char-ready?:
(if (char-ready? port)
    (read-char port)
    'not-ready)

;; Read asynchronously on a thread, publishing into an atomic box:
(let ((slot (make-atomic #f)))
  (thread-start!
    (make-thread
      (lambda ()
        (atomic-store! slot (read-char port)))))
  ;; ... poll (atomic-load slot) or join the thread when the result is needed.
  slot)
```

**Impact:** **LOW** — `char-ready?` and `u8-ready?` are rarely used in modern Scheme code. These predicates were designed for select-style event loops, a pattern largely superseded by async/await style concurrency. Most I/O in Wile is either:
- File-based (always ready, blocking is acceptable)
- Network streams where blocking semantics are expected
- Interactive REPL input where immediate blocking is desired

**Estimated implementation effort:** 4-8 hours including cross-platform support and testing.

**ROI analysis:** Documentation (15 minutes) provides clear expectations at far better ROI than implementation (4-8 hours) for an exotic edge case.

---

## Parameterize Implementation (Marks-Based)

**Affected Form:** `parameterize`

**R7RS §4.2.6:** The R7RS reference implementation uses `dynamic-wind` to save/restore parameter values. Wile uses `with-continuation-mark` instead, storing parameter bindings as continuation marks keyed by the parameter object.

**Why:** The `dynamic-wind` approach has bugs when composable continuations (`call-with-composable-continuation`) cross `parameterize` boundaries. The after-thunk captures the "old" value at definition time. When a composable continuation is invoked from a different `parameterize` context, the stale old value clobbers the outer binding. Marks-based `parameterize` eliminates this class of bugs because bindings ride on the continuation frames structurally.

**Semantic difference:** `(p val)` (calling a parameter with 1 argument) inside `parameterize` sets the parameter's base value. With `dynamic-wind`-based `parameterize`, the mutation is visible within the extent and undone on exit. With marks-based `parameterize`, the mark shadows the base value, so the mutation is invisible while the `parameterize` is active but persists after it exits.

This difference is observable only when code mutates a parameter via `(p val)` inside a `parameterize` body — a rare pattern. The standard pattern of reading `(p)` inside `parameterize` is unaffected.

**Impact:** **LOW** — standard R7RS programs use `parameterize` for scoped binding, not direct mutation. The marks-based approach matches Racket's semantics and is correct for composable continuations.

---

## Immutable Vector and Bytevector Literals

**Affected Primitives:** `vector-set!`, `vector-fill!`, `vector-copy!`, `bytevector-u8-set!`, `bytevector-copy!`

**R7RS §6.4 Requirement:**
> It is an error to attempt to store in a literal.

R7RS §1.3.2 clarifies "it is an error" as a case implementations are not required to detect, but "encouraged to detect ... so as to help the programmer detect them."

**Wile Behavior:** Mutating a vector or bytevector literal **raises an error** (`ErrImmutableVector` / `ErrImmutableBytevector`), matching the long-standing immutable-string behavior. Wile detects the case R7RS encourages detecting.

```scheme
(vector-set! '#(1 2 3) 0 'x)       ; raises ErrImmutableVector
(bytevector-u8-set! '#u8(1) 0 9)   ; raises ErrImmutableBytevector
(string-set! "abc" 0 #\X)          ; raises ErrImmutableString
```

**A PAIR literal is not detected**, and that is a decision rather than an omission. The detection needs a per-object flag; `*values.Pair` is a two-word cons cell and the dominant heap object, so the flag is ~25% growth on it, and the mutation path it would guard is one the peephole optimizer promotes to an inline opcode. Measured: 5.86 ns for the side-set lookup this replaced, against 0.73 ns for a flag read and 0.22 ns for the store being guarded. Since R7RS only encourages the detection, dropping it for pairs stays conforming.

```scheme
(set-car! '(a b c) 999)   ; succeeds
```

Because Wile shares structure for same-shape literals (`(eq? '(a b c) '(a b c)) → #t`, R7RS-permissible), detection also prevents non-local corruption: mutating one vector literal through a shared instance — which would otherwise be visible through every syntactically identical quotation — is rejected rather than silently propagated. **That protection is exactly what pairs give up.** Within one compilation unit, `(begin (define x '(1 2 3)) (define y '(1 2 3)) (set-cdr! x 99))` leaves `y` as `(1 . 99)`. A program that mutates list structure must build it with `list` or `cons`.

**Implementation:**

Immutability is **intrinsic to the value**. `*values.Vector` and `*values.ByteVector` are structs carrying an `immutable` flag, the same shape `*values.String` uses; `values.Immutable` normalizes the read across all three (the underlying fields do not agree on polarity), and `values.MarkImmutable` is the one write surface. Each type's `Set` self-enforces, so a caller reaching the setter from a path nobody gated still gets the refusal.

The flag is written by one compile-time walk, the quote hook's `markLiteralImmutable` (`machine/compilation/compile_literal_immutability.go`), which descends the whole literal and flags every vector and bytevector in it — including ones reachable only through a pair spine, which is why that walk still traverses pairs while flagging nothing in them. The write happens before the value can be named from Scheme, so a plain `bool` suffices with no synchronization. Only literals are constrained: `list`, `cons`, `make-vector` and the copying procedures all yield unflagged allocations, and structure-shared siblings are one object, so one flag covers them all.

There was, until 2026-08, a second home: an engine-scoped `sync.Map` side set keyed by pointer identity, which is how pair literals were tracked. It is deleted along with pair-literal immutability.

**Workaround (for programs that must mutate):**

Construct with `list`, `cons`, `make-vector`, or `vector-copy` to obtain an allocation not shared with any literal:

```scheme
;; Error — literal is immutable:
(let ((xs '(1 2 3))) (set-car! xs 99) xs)

;; Right — guaranteed mutable, no aliasing with literals:
(let ((xs (list 1 2 3))) (set-car! xs 99) xs)
(let ((v (vector-copy '#(1 2 3)))) (vector-set! v 0 99) v)
```

**Impact:** **LOW** — programs that mutate literals are already non-portable across R7RS implementations; Wile now rejects them rather than silently corrupting shared structure.

---

## Immutable Top-Level Definitions (default; opt out with `WithMutableTopLevel`)

**Affected:** `set!` and re-`define` of top-level variables. **Enabled by default.** Opt out per-engine with `WithMutableTopLevel()` to restore strict R7RS top-level mutability. `WithImmutableTopLevel()` remains as an explicit (now-redundant) selector for the default.

**R7RS §4.1.6 / §5.3.1:** top-level variables are mutable (`set!`) and redefinable.

**Wile Behavior (default):** A top-level `define` in the user program that is *defined exactly once* and *never `set!` within its compilation unit* is marked rebind-stable. A subsequent `set!` of such a binding **raises `ErrImmutableBinding`** at compile time:

```scheme
;; Default (immutable top level):
(define f 5)
(set! f 6)        ; raises ErrImmutableBinding — f is stable

;; Still mutable: a define that IS set! within its own unit is not stable.
(begin (define g 5) (set! g 6) g)   ; => 6, permitted

;; Opt out for strict R7RS mutability:
;;   wile.NewEngine(ctx, wile.WithMutableTopLevel())
```

**Why this is now the default (the layered-environment carve).** Each engine splits its
runtime bindings into an immutable **sealed base** (Go primitives + sealed stdlib
procedures) parented by a **mutable runtime** (user defines). Resolution walks
`mutable-runtime → sealed-base`. This dissolves the two blockers that previously kept
immutability opt-in:

- A user or stdlib **re-`define` of a sealed name is a shadow, not a rebind.** `(define
  car …)` or `(import (scheme cxr))` creates a *new* binding in the mutable child (the
  name is absent from that frame's own map), so the redefinition guard never fires. The
  sealed `car` is untouched.
- **Enforcement is scoped to the compiled program (the engine's root namespace).** Defines
  landing in the engine's own user runtime are frozen, and the sealed base's *defined-once*
  bootstrap procedures (`map`, `assoc`, the `cxr` accessors, …) **are** stamped `Stable` and
  frozen too: a top-level `(set! map …)` is rejected. (Go primitives are stamped `Stable`
  only when they are *capture-safe*, so `(set! car …)` and `(set! vector-ref …)` are both
  rejected, while `(set! apply …)` — a procedure-invoking primitive, hence not
  capture-safe — stays permitted.) Two contexts stay **mutable**: **user-loaded
  libraries** — a library body's cross-form `(define *x* …)` / `(set! *x* …)` works — and
  **interactive / first-class eval environments** — the REPL, the `--mcp` session, and the
  namespaces built by `(environment …)` / `scheme-report-environment` are mutable scratch
  spaces (Chez interaction-environment model), where a redefine is a permitted shadow. The
  frame-reclaim GC payoff therefore applies to compiled programs (files, `-e` batches), not
  to interactive redefinition.

**Redefine-visibility deviation (Chez two-environment model).** Because a sealed binding
is resolved and pinned at *compile time*, an already-compiled closure over a sealed name
keeps seeing the **sealed** value after a later shadow:

```scheme
(define (use-car p) (car p))   ; car pinned to the sealed base at compile time
(define car (lambda (x) 99))   ; a shadow in the mutable runtime
(use-car '(7 8))               ; => 7  (the REAL car — the shadow is not observed)
(car '(7 8))                   ; => 99 (this call compiled AFTER the shadow)
```

This mirrors Chez's sealed-base + interaction-environment split: code already compiled
against the base does not observe later interactive redefines. It is an intentional,
documented deviation from R7RS's single mutable top level.

**Free template identifiers resolve definition-site — on the macro path too.** The same
definition-site pinning applies to a free identifier in a `syntax-rules` template, for macro
references as well as value references (R7RS §4.3.2 referential transparency). A bootstrap
macro whose template references a private helper macro — e.g. `guard` referencing `guard-aux`
— resolves that helper to its definition-site binding, so a later use-site
`(define-syntax guard-aux …)`, `let-syntax`/`letrec-syntax` binder, or `import` of a
same-named macro does **not** capture it:

```scheme
(define-syntax guard-aux (syntax-rules () ((_ . r) 'PWNED)))
(guard (e (else 'x)) (raise 'y))   ; => x   (guard's own guard-aux, not the redefinition)
(guard-aux ignored)                ; => PWNED  (a direct call sees the redefinition)
```

The helper macros are sealed in a per-namespace immutable **expand base** (phase 1), the
macro-phase analogue of the sealed base above, and macro dispatch consults the pin after the
local `let-syntax` arm and before the use-site arms (so a co-introduced keyword still shadows,
the R1 invariant). This holds for a helper's **own recursion** as well: a recursive macro's
template reference to itself (e.g. `guard-aux`'s multi-clause recursion, `define-record-type`
building one accessor per field) resolves to its own definition-site binding, so a use-site
redefinition of the private helper cannot capture the recursion. Scope: this is a
*private-helper / definition-site* guarantee, not "freeze every public macro" — a user who
redefines a public macro like `cond` still sees their redefinition in code they subsequently
write that expands through it (a directly-typed reference carries no pin).

**define/set! asymmetry on sealed names.** `(define caar …)` *introduces* a child-frame
shadow (closures keep the sealed binding); `(set! caar …)` *mutates the existing* binding
in place (closures observe it) — and in the compiled program, `set!` of a `Stable` sealed
name (a capture-safe primitive such as `car`, or any defined-once bootstrap procedure such
as `caar`/`map`) is rejected with `ErrImmutableBinding`. This is ordinary Scheme semantics
(`define` introduces, `set!` mutates) applied to the sealed/mutable split.

**Rationale:** This is the language-level enforcement half of the frame-reclamation optimizer. The optimizer may release a function's stack frame at a tail call only if every callee it relies on provably never captures a continuation; proving that for a *top-level* callee requires knowing the binding will not be rebound to a capturing procedure. Rather than *infer* unit-closure (undecidable for an incremental/embedded system), the engine *enforces* it — the "compile for speed" contract used by sealed-module Schemes (Racket modules, Chez `optimize-level 3`).

**Implementation:** Pure compile-time, scoped to the engine's **root** namespace. The redefinition guard fires only for a define landing in the root's own user runtime or sealed base (`compile_validated.go`); child namespaces report `ImmutableTopLevel() == false`, so REPL / `(environment …)` / `scheme-report-environment` redefines are permitted. The compiler stamps `BindingMeta.Stable`; the `set!` guard keys on `IsStable()` **directly** (not on the namespace flag), so a `Stable` anchor copied into a mutable child stays `set!`-protected — preserving frame-reclaim soundness while still allowing define-shadow. Imported-binding `set!` rejection (always on) is unchanged.

**Impact:** Programs that rebind their own never-mutated top-level definitions via `set!` are rejected by default; the common case (define-once, call-many) is unaffected and gains the optimization. Use `WithMutableTopLevel()` for strict R7RS top-level mutability.

---

## Conflicting Imports Signalled (R7RS §5.6)

**R7RS §5.6:** "It is an error to import the same identifier more than once with different bindings." Per §1.3.2, the phrase *"is an error"* (without *"is signalled"*) means the program is erroneous but the implementation is **not required** to detect it — only encouraged to.

**Wile signals it.** Importing one identifier from two libraries (or two import sets) that resolve to **different** bindings raises `werr.ErrDuplicateBinding` rather than silently letting the last import win. This matches Chez Scheme and Racket (R6RS-strict implementations signal the same situation) and avoids the fragile, order-dependent last-import-wins resolution that Guile and pre-strict Wile used.

```scheme
;; (scheme base) string-map is the R7RS variadic form; (srfi 13) string-map is the
;; single-string + range form — two DIFFERENT procedures under one name.
(import (scheme base) (srfi 13))                 ; ERROR: conflicting import of string-map
(import (except (scheme base) string-map) (srfi 13))  ; OK — srfi-13's string-map
(import (scheme base) (prefix (srfi 13) s:))     ; OK — srfi-13 names prefixed s:
(import (scheme r5rs) (srfi 13))                 ; OK — r5rs has no string-map
```

**Diamonds and repeats stay legal.** Importing one identifier from two libraries that re-export the **same** binding (e.g. `make-list` from both `(scheme base)` and `(srfi 1)`, or `nan?` from both `(scheme base)` and `(scheme inexact)`), and re-importing the same library, are not conflicts.

**SRFI guidance.** Several SRFIs (notably SRFI-13) predate R7RS and the module system entirely (SRFI-13 targets R5RS, which had no `string-map`). Their R7RS port can collide with `(scheme base)`. Use `(scheme r5rs)` as the base for full SRFI surfaces, or `except`/`prefix`/`rename` to disambiguate against `(scheme base)`.

**Implementation:** Wile import **recompiles** a re-exported definition — an ambient procedure or macro is rebuilt into each manifest library that re-exports it, so the copies are distinct closures with no shared template/env/pointer. The conflict check (`importConflicts` in `library_bindings.go`) therefore identifies "the same binding" for closures by procedure **name** (the recompilation-stable signal: the registry is name-unique for primitives, and a named procedure keeps its name across recompiles), and by `EqualTo` for everything else. The one genuine stdlib collision — `(scheme base) string-map` vs `(srfi 13) string-map`, both name-less `case-lambda`s — is caught by `EqualTo`: a re-exported case-lambda shares its value pointer (diamond), whereas these two differ structurally (conflict).

**Limitation (deliberate, irreducible):** a collision the *name* cannot distinguish is treated as the same binding and silently last-import-wins. This covers **name-less closures** — macro transformers and var-form-defined procedures (`(define f (lambda …))`), whose template name is empty, so `"" == ""` reads as identical — and **same-named function-form procedures**. Catching these would require a definition origin (source location), which was rejected because it falsely flags the ubiquitous, legal **define-over-import shadow**: a program that does `(import (scheme base))` then `(define (zero? x) …)` would see its own `zero?` "conflict" with the imported one. Name comparison preserves that shadowing, and the only signal that could separate a recompiled re-export from a genuine same-name clash is exactly the origin that breaks shadowing — so the gap is irreducible at the value layer. No such hidden clash exists in the bundled stdlib (the one real collision, `string-map`, is caught above).

---

## Value-Count at a Single-Value Slot (Arity-Checked, Not Spliced)

**Affected:** every expression that delivers its result into a slot holding
exactly one value: a procedure-call argument, a `let` / `let*` / `letrec` init, a
`define` or `set!` operand, an operand of a promoted primitive. Invoking a
captured continuation is included, because a continuation resumes *at* the
delivery instruction: `call-with-current-continuation` / `call/cc` escape
procedures and `call-with-composable-continuation` are therefore governed by the
same rule as `(values …)`.

**R7RS §6.10 Requirement:**
> Except for continuations created by the `call-with-values` procedure
> (including the initial continuation) … the effect of passing no value or more
> than one value to continuations that were not created by `call-with-values` is
> unspecified.

**Wile Behavior:** delivering any count but one **raises**. Chez and Racket
agree on every row below; Chez's wording is "returned 2 values to single value
return context".

The raise is a **catchable** condition, as it is in Chez:
`(guard (e (#t 'caught)) (+ (values 1 2) 3))` answers `caught`. `error-object?`
is `#t`, and it impersonates nothing (`file-error?` and `read-error?` are `#f`).
From Go it matches `werr.ErrWrongNumberOfValues`, which is deliberately distinct
from `ErrWrongNumberOfArguments`: `(define x (values))` has no arguments, and
`(f (values 1 2))` is a well-formed one-argument call whose argument misbehaved.
It reports at the **offending subexpression** rather than the enclosing form, so
`(+ 3 (values 1 2))` names column 15 and `(let ((a 1) (x (values 1 2))) x)` names
the second binding's init.

```scheme
(+ (values 1 2) 3)                     ; raises  (was 6, i.e. (+ 1 2 3))
(list (values 1 2) 9)                  ; raises  (was (1 2 9))
(define x (values))                    ; raises  (was a Stack.Pop panic)
(let ((x (values))) x)                 ; raises  (was a Stack.Pop panic)
(+ 1 (call/cc (lambda (c) (c 5 6))))   ; raises  (was 12)
(+ 1 (call/cc (lambda (c) (c))))       ; raises  (was 1)
```

Multiple-value *receivers* are unaffected and still accept 0, 1 or N:

```scheme
(call-with-values (lambda () (values 1 2)) +)                        ; => 3
(call-with-values (lambda () (values)) list)                         ; => ()
(call-with-values (lambda () (call/cc (lambda (c) (c 1 2 3)))) list)  ; => (1 2 3)
(let-values (((a b) (values 1 2))) (list a b))                       ; => (1 2)
(call-with-values
  (lambda () (dynamic-wind (lambda () 1) (lambda () (values 1 2)) (lambda () 3)))
  list)                                                              ; => (1 2)
```

**Mechanism:** the value register holds 0, 1 or N values, and two opcodes read
it. `OpPush` delivers exactly one into one stack slot and raises on any other
count; `OpPushValues` spreads whatever is there. Only `applyToValuesCode`
(`pkg/machine/run_body_under_frame.go`) emits `OpPushValues`, which is what
`call-with-values`, `make-parameter`, `call-with-exit`, `with-timeout` and the
non-continuable `raise` escalator run under. Everything the compiler emits uses
`OpPush`. See `docs/continuations/delimited.md` and
`docs/continuations/escape-design.md` for the capture/restore mechanism.

**Rationale:** the previous splicing was emergent rather than designed. One
opcode served both roles, so `OpPush` pushed the register's live arity and N
values became N stack entries. Every drain that reads a count fixed at compile
time then read a depth the stack did not have: `DrainN(argCount)` under
`OpSelfTailCall`, `Stack.Pop2` in a fused promoted operator, `Stack.Pop` under
the store opcodes. The result was wrong answers and reachable internal-invariant
panics from ordinary Scheme, including a case where the extra value survived on
the eval stack and was drained by an unrelated later call
(`(define (f) (let ((x (values 1 2))) (list 'a)))` answered `(1 a)`). Splitting
the opcode fixes the whole family at the point of delivery, which is the last
instruction that can still see the count: once the values are on the stack,
`[+, 1, 2, 3]` is indistinguishable from a genuine three-argument call.

**Reverses a prior decision.** Enforcing a value-count in single-value
resumption contexts had been investigated and declined, on the grounds that it
costs a check on the `RestoreContinuation` hot path to constrain behavior R7RS
leaves unspecified. That cost objection does not apply to the check as built:
it sits at the delivery instruction, on a branch `pushValueRegisterTo` already
took to distinguish the single-value fast path from the multiple-value one.
`RestoreContinuation` is untouched.

**Unchanged:** `dynamic-wind` still preserves multiple values from its thunk,
`guard` still propagates them, and `procedure-arity` still reports a
continuation as `(0 . #f)`, matching its `AcceptsArity` of `true`. What a
continuation *accepts* has not narrowed; what a single-value slot accepts has.

---

## `current-second` Returns Unix Time, Not TAI

**Primitive:** `current-second`

R7RS §6.13.2 specifies that `current-second` returns the current time as
**TAI** (International Atomic Time) seconds. Wile returns **POSIX/Unix time** —
`float64(time.Now().Unix()) + nanoseconds/1e9` — which counts seconds since the
Unix epoch *excluding* leap seconds. The two clocks differ by a fixed integer
offset (37 seconds since 2017-01-01; the offset grows only when the IERS inserts
a new leap second).

**Why.** Computing true TAI requires a leap-second table that must be updated
whenever a leap second is announced — a maintenance and distribution burden for
a value almost every program uses only as a monotonic-ish wall-clock reading or
to compute elapsed real time (where the constant offset cancels). Most host
runtimes (including Go's `time` package) expose Unix time, not TAI.

**Impact.** `(- (current-second) t0)` for elapsed-time measurement is unaffected
(the offset cancels). Programs that need a true atomic timescale, or that compare
`current-second` against an external TAI source, will see the fixed offset. For
monotonic elapsed time prefer `current-jiffy` / `jiffies-per-second`.

## A Splice in an Unquote's Operand (`,,@x`) Stays Literal

**Affected:** a splice written in the **operand position** of an `unquote` /
`unsyntax` that is too deep to fire — `` ``(a ,,@x) `` and its dotted, vector and
`quasisyntax` spellings. Not affected: `,@` in any **element** position, which is
the only place the grammar puts it, and which behaves exactly as specified.

**R7RS §7.1.4 Grammar:**

```
⟨unquotation D⟩           → , ⟨qq template D−1⟩
                          | (unquote ⟨qq template D−1⟩)
⟨qq template or splice D⟩ → ⟨qq template D⟩
                          | ⟨splicing unquotation D⟩
⟨splicing unquotation D⟩  → ,@ ⟨qq template D−1⟩
                          | (unquote-splicing ⟨qq template D−1⟩)
```

> In ⟨quasiquotation⟩s, a ⟨list qq template D⟩ can sometimes be confused with
> either an ⟨unquotation D⟩ or a ⟨splicing unquotation D⟩. The interpretation as
> an ⟨unquotation⟩ or ⟨splicing unquotation D⟩ takes precedence.

`unquote` takes **exactly one** operand, and that operand is a
`⟨qq template D−1⟩` — a nonterminal from which `⟨splicing unquotation⟩` is
unreachable. `,@` derives only from `⟨qq template or splice⟩`, which occurs only
in the element positions of `⟨list qq template⟩` and `⟨vector qq template⟩`:
never as an operand, never as a tail. So `,,@x` has no derivation as an
unquotation, and the precedence note settles it two ways depending on how widely
it is read. As a global rule it forces the splicing reading, which is
inadmissible in operand position, leaving the form **ungrammatical**. As a rule
that applies only where the context admits both readings, the one surviving
derivation is `(unquote-splicing x)` as an **ordinary two-element list of data**.

**Wile Behavior:** the second reading. The operand is a literal list and the `,@`
inside it is inert, so the keyword survives into the output datum.

```scheme
(define x (list 1 2))

`(a ,@x)          ; => (a 1 2)          element position: fires, as specified
`(a ,,@x)         ; => error            depth 1: the escape hands the raw
                  ;                     splice to the compiler (all three agree)
``(a ,,@x)        ; => (quasiquote (a (unquote (unquote-splicing x))))
``(a . ,,@x)      ; => (quasiquote (a unquote (unquote-splicing x)))
``#(a ,,@x)       ; => (quasiquote #(a (unquote (unquote-splicing x))))
```

Chez and Racket both implement an **extension** here, splicing into the
reconstructed operand list, which can yield several operands or an improper tail
(`` ``(a . ,,@x) `` with `x` bound to `5` gives `(quasiquote (a unquote . 5))`).
The two disagree about how far it reaches, which is the clearest evidence that
this is unspecified territory rather than a settled requirement:

| Form (`x` = `(1 2)`) | Wile | Chez | Racket |
|---|---|---|---|
| `` ``(a ,,@x) `` | `(a (unquote (unquote-splicing x)))` | `(a (unquote 1 2))` | `(a (unquote 1 2))` |
| `` ``(a (unquote ,@x ,@x)) `` | `(a (unquote (unquote-splicing x) (unquote-splicing x)))` | `(a (unquote 1 2 1 2))` | `(a (unquote (unquote-splicing x) (unquote-splicing x)))` |

(outer `quasiquote` elided). The second row has **two** operands, so it is not an
`⟨unquotation⟩` under any reading — only Chez extends that far, and Racket lands
back on Wile's answer.

**Why.** No conforming program can contain the form: under the strict reading it
is ungrammatical, and under the permissive one it is inert data whose value Wile
already produces. Adopting the extension would mean rewrapping an over-deep
keyword form as `(cons '<keyword> …)` rather than `(list '<keyword> …)`, which
re-shapes *every* over-deep form at identical values, and would still require
choosing Chez's reading over Racket's on the multi-operand row. The behavior is
documented rather than changed.

**Impact.** None for portable code. A program that relies on Chez's or Racket's
splicing here does not port to Wile, and one that relies on Wile's literal
reading does not port to them. `quasisyntax` behaves as the mirror image
(`` #`#`(a #,#,@x) `` yields `(quasisyntax (a (unsyntax (unsyntax-splicing x))))`)
because both dialects share `rewrapQuasiForm`; the mirror-image property itself
is pinned by `TestQuasiExpandShape`'s dotted rows, though no row covers this
form specifically.

## Extensions Beyond R7RS

These are Wile-specific features that extend R7RS. They do not conflict with R7RS behavior — standard Scheme programs behave identically. Some use reader prefixes in the `#` dispatch space that R7RS leaves implementation-defined; others add strictly-additive library exports and library-declaration metadata. In every case the extension is additive: a program written against R7RS alone sees no change, while a program that depends on the extension does not port to a strict implementation.

### Arbitrary-Precision Number Literals

Wile provides reader syntax for explicitly constructing arbitrary-precision numbers. These are not part of any Scheme standard (R5RS, R6RS, R7RS, or SRFIs).

| Prefix | Type | Exactness | Backed by | Examples |
|--------|------|-----------|-----------|----------|
| `#z` | BigInteger | exact | `math/big.Int` | `#z12345678901234567890`, `#z-42`, `#z+7` |
| `#m` | BigFloat | inexact | `math/big.Float` (256-bit) | `#m3.14159265358979323846`, `#m1.5e-10`, `#m.5` |

Both prefixes are case-insensitive (`#Z`, `#M` also work), following R7RS §7.1.1 conventions.

Both are **datum introducers**: each reads one complete number datum and widens
it. The datum is read by the ordinary number reader, so radix and exactness
composition is inherited rather than enumerated:

```scheme
#z#x1f      => 31          ; the inner datum carries its own radix prefix
#z#e#x1f    => 31          ; and its own exactness prefix, in either order
#z#x#e1f    => 31
#z#b19      => error       ; because #b19 is an error
#z#z5       => 5           ; a coercion, not a container: it does not nest
#m#b101.101 => 5.625
```

**`#z` combines with a radix prefix in one order only.** `#z#x1f` reads;
`#x#z1f` does not. A radix prefix selects the digit set *before* scanning, so
its operand has to be a literal, while `#e` / `#i` are applied to an
already-read datum and therefore accept anything numeric (`#e#z9` is 9).
Radix is lexical; exactness is post-hoc.

**`#z` requires an exact integer**, which is what a BigInteger is: `#z1.5`,
`#z1e3`, `#z#x1.8`, and `#z#i#x1f` are all errors. `#m` is the prefix for the
inexact side, and requires a real.

**`#m`'s precision is the value's, not the literal's.** `#m1.2345678901234567890123456789`
keeps every digit, because it is one token the BigFloat reader sees whole.
`#m#d1.2345678901234567890123456789` goes through the introducer path, so it
widens an already-rounded `float64`. For arbitrary precision use the unprefixed
decimal spelling or the `l` marker (see below); `#m` over a prefixed datum is a
type coercion.

**Note:** R7RS requires implementations to support arbitrarily large exact integers (§6.2.3). Wile satisfies this via automatic overflow promotion from `Integer` (int64) to `BigInteger` — the `#z` prefix is a convenience for explicit construction, not a conformance requirement. Standard R7RS programs never need `#z` or `#m`.

### Exponent Markers Denote Precision

R7RS §6.2.5 makes the exponent markers `s` (short), `f` (single), `d` (double),
and `l` (long) an **optional** extension, with the escape clause that an
implementation offering fewer than four internal inexact representations "the
four size specifications are mapped onto those available". Wile has two, `Float`
(float64) and `BigFloat` (256-bit), so:

| Marker | Requests | Wile type |
|--------|----------|-----------|
| `e` | default precision | `Float` |
| `s`, `f`, `d` | short / single / double | `Float` |
| `l` | long | `BigFloat` |

```scheme
1.5d0  => 1.5      ; a Float
1.5l0  => 1.5l0    ; a BigFloat
(exact 1.2345678901234567890123456789l0)  ; keeps all 29 digits
(exact 1.2345678901234567890123456789d0)  ; rounds to the nearest float64
```

**This applies on output too.** A `BigFloat` writes with `l`, so `1e1000` reads
as a BigFloat and writes back as `1l+1000`, and `#m1.5` writes as `1.5l0`.
Writing `e` would claim default precision, and the value would read back as a
`Float` — a different type from the one written. Inside a complex number the
marker is omitted, since a component is not read back as a real.

**Wile is the only Scheme where `l` selects a different representation.** Racket,
Chez, and MIT all collapse the four markers onto one flonum type, so `1.5l0`
reads as a plain double everywhere else. Code that relies on the distinction is
not portable.

**Exponent markers are decimal-only.** `#x1e2` is 482, not 100.0, because `e` is
a hex digit. R7RS §7.1.1 places `⟨suffix⟩` only inside `⟨decimal 10⟩`, and MIT
states the same rule ("a numeric representation using a decimal point or an
exponent marker is not recognized unless radix is 10"). Racket, Chez, and MIT
nonetheless all accept `#x1s3` = 4096.0, with the exponent base being the
*radix*; Wile deliberately does not.

### Radix-Prefixed Fractions

R7RS §7.1.1 defines `⟨decimal R⟩` only for R = 10, so a fraction written under a
radix prefix is outside the standard grammar. Wile accepts it, matching Racket,
Chez, and MIT:

```scheme
#x1.8      => 1.5        ; 1 + 8/16
#o1.4      => 1.5        ; 1 + 4/8
#b101.101  => 5.625
#xF.C      => 15.75
#x.f       => 0.9375
```

The radix governs the fractional digits too, so `#b1.9` and `#o1.8` are errors.
As R7RS §6.2.4 requires, a literal written with a decimal point is inexact
whatever its radix.

A related consequence: **a numeral carrying an explicit radix prefix must end at
a delimiter.** `#b19` is an error rather than the number 1 followed by the number
9, and `(#b19)` is an error rather than the list `(1 9)`. A numeral with no
radix prefix still splits — `1abc` reads as `1` then the symbol `abc` — which is
a known inconsistency, not a decision.

### Bare Sign-Dot Identifiers: `+.` and `-.`

Wile reads `+.` and `-.` as symbols. R7RS §7.1.1's grammar does not admit them:

```
⟨peculiar identifier⟩ → ⟨explicit sign⟩
                      | ⟨explicit sign⟩ ⟨sign subsequent⟩ ⟨subsequent⟩*
                      | ⟨explicit sign⟩ . ⟨dot subsequent⟩ ⟨subsequent⟩*
                      | . ⟨dot subsequent⟩ ⟨subsequent⟩*
```

The third production requires a ⟨dot subsequent⟩ after the dot, so `+..` and
`+.a` are valid identifiers while a bare `+.` is not.

Wile accepts them anyway, and they round-trip: `write` emits `|+.|`, using
R7RS's own vertical-line syntax.

```scheme
(write '+.)   ⇒ |+.|
(write '-.)   ⇒ |-.|
(write '+..)  ⇒ +..
(write '+.a)  ⇒ +.a
```

This falls out of the number scanner being **speculative**: a run beginning with
an explicit sign is scanned as a numeral, and on any failure the whole run is
handed to the symbol scanner rather than minting a partial token. Refusing `+.`
specifically would mean deciding ⟨dot subsequent⟩ inside that fallback — a second
implementation of the identifier grammar, in the one place whose design is "if it
is not a number, it is a symbol."

Both reference implementations agree. Measured: Chez Scheme 10.4.1 reads `+.` as
a symbol and writes it `\x2B;.`; Racket reads it as a symbol and writes it bare.
Chez's escaping shows it shares Wile's judgement that the spelling needs quoting
on output, differing only in syntax.

A program relying on `+.` as an identifier does not port to an implementation
that follows the grammar strictly. A program relying on `+.` being a *read error*
does not port to Chez or Racket.

### Boxes

`#&⟨datum⟩` reads a box, the same syntax `write` already emitted for one. This
is not part of any Scheme standard, but is near-universal (Racket, Chez, Guile).

```scheme
#&5        ; a box holding 5
#&#x1f     ; => #&31 — the datum may carry its own prefixes
#&#&5      ; a box holding a box: unlike #z, #& nests
#0=#&#0#   ; a box holding itself
```

The cyclic form is accepted, following Racket; Chez rejects it. Wile has to
accept it, because Wile's own writer emits exactly that form for a box reachable
from itself, and rejecting it would leave the writer's output unreadable.

**A box in a quoted literal is mutable, by decision** — `(set-box! (car '(#&1)) 2)`
succeeds, where the enclosing list's pairs and any nested vector would refuse.
Boxes are not R7RS, so this is extension semantics rather than a deviation from
the spec: `Box` carries no immutability flag and the literal walk never reaches
one, so there is nothing to consult. Programs that must forbid it can disable
`set-box!` outright — it is already a member of the no-mutation dialect
(`pkg/wile/dialect_nomutation.go`).

### Process-Global Working Directory

**Primitive:** `set-current-directory!`

**Behavior:** Calls `os.Chdir`, which changes the working directory for the entire OS process. Multiple Wile engines in the same Go process share one working directory. Concurrent calls from different goroutines race on the same OS state. This is inherent to POSIX — there is no per-thread working directory.

**Mitigation:** The primitive is gated by `security.ResourceFile` / `security.ActionWrite` with the *destination path* as the target, so embedders can deny it via their authorizer, and a path-confining authorizer evaluates the new working directory rather than an opaque `"cwd"` target it would never inspect. When denied, all file operations should use absolute paths.

R7RS does not specify directory operations. This follows SRFI-170 conventions.

### Guard Body Multiple Values

R7RS §7.3's reference implementation of `guard` uses `(let ((result (begin e1 e2 ...))) ...)`, which binds a single value. If the body produces multiple values via `(values v1 v2 ...)`, the `let` binding triggers an arity mismatch.

Wile's `guard` uses `call-with-values` to capture all values from the body, then re-emits them via `(apply values results)`. This means `(guard (e (#f)) (values 1 2))` correctly propagates both values, whereas the R7RS reference implementation would signal an error. Observing them needs a multiple-value receiver: `(call-with-values (lambda () (guard (e (#f)) (values 1 2))) list)` is `(1 2)`, while placing the same `guard` in a single-value slot raises, per "Value-Count at a Single-Value Slot" above.

### Loss-Signal-Aware Numeric Conversion Primitives

Wile adds four primitives (in the math extension) that expose the
accuracy of `exact->inexact` conversion at the `float64` /
`complex128` boundary, surfacing Go's `big.Accuracy` three-valued
enum as `'below` / `'exact` / `'above` symbols:

| Primitive | Purpose |
|-----------|---------|
| `inexact-lossless?` | Predicate: `#t` iff conversion is lossless |
| `inexact-accuracy` | Returns accuracy symbol(s) without converting |
| `inexact-with-accuracy` | Returns `(values inexact-n acc-sym)` (real) or `(values inexact-c real-acc imag-acc)` (complex) |
| `complex-inexact-with-accuracy` | Uniform 3-value variant regardless of input domain |

R7RS `(exact->inexact)` (§6.2.6) is **unchanged** — it continues to
saturate silently to `+inf.0` / `-inf.0` on overflow. The new
primitives **expose** the rounding direction rather than gate it.
R7RS-strict programs that import only `(scheme base)` /
`(scheme inexact)` are unaffected; these primitives are reachable
only after loading the math extension (profile `Small` and above).

### FFI Numeric Argument Precision

For embedders using `Engine.RegisterFunc` with Go functions taking
`float64` or `complex128` parameters: the default conversion is now
**precision-aware**. Passing a Scheme numeric value that cannot be
exactly represented in the Go fixed-precision type (e.g. `1/3`,
`*BigInteger` exceeding 2^53, `*BigFloat` overflowing magnitude)
returns `werr.ErrLossyConversion` instead of silently truncating.

The `wile.WithLossyConversionsAllowed()` engine option restores the
silent-truncation behavior for embedders that depended on it. This
is purely an embedder-API concern; Scheme programs are unaffected.

See `docs/numeric/tower.md` §"Conversion to Fixed-Precision Go Types"
for the underlying design.

### Standard-Library Export Supersets

Some bundled standard libraries **export bindings their specification does not
assign them** — usually one R7RS Appendix A homes in a different library,
re-exported as an embedding convenience so a program can reach a common binding
without importing its formal home. The extra exports are strictly additive: a
program that imports one of these libraries and uses only the bindings the
specification assigns it behaves identically. The trade-off runs the other way —
code that imports one of the bindings below *from the convenience library* does
not load on a strict implementation, where the binding is exported only from its
home, or not at all.

| Library | Also exports | Home the specification gives it |
|---------|--------------|---------------------------------|
| `(scheme base)` | `case-lambda` | `(scheme case-lambda)` |
| `(scheme base)` | `read` | `(scheme read)` |
| `(scheme base)` | `write`, `display` | `(scheme write)` |
| `(scheme base)` | `delay`, `delay-force`, `force` | `(scheme lazy)` |
| `(scheme eval)` | `scheme-report-environment`, `null-environment` | `(scheme r5rs)` |
| `(scheme cxr)` | `caar`, `cadr`, `cdar`, `cddr` | `(scheme base)` |
| `(scheme r5rs)` | `call/cc` | `(scheme base)` |
| `(scheme r5rs)` | `unquote`, `unquote-splicing` | `(scheme base)` |
| `(srfi 13)` | `string-trim-left` | none — a Wile alias for SRFI-13 `string-trim` |

Each binding remains exported from its R7RS home as well, so portable code that
imports the home library is unaffected. `(scheme base)` also dual-homes
`finite?` / `infinite?` / `nan?` (R7RS home `(scheme inexact)`); that trio is a
legal re-export diamond, covered under "Conflicting Imports Signalled" above and
not repeated here.

The `(scheme cxr)` row is structural rather than a bare convenience: the two-deep
accessors `caar`…`cddr` are defined ambiently in the bootstrap (core procedures
such as `assoc` depend on them before any library loads), and `(scheme cxr)`
re-exports those ambient bindings rather than redefining them — so it surfaces
the two-deep accessors R7RS assigns to `(scheme base)` alongside the three- and
four-deep ones it owns.

The `(scheme lazy)` row is the same shape but was, until 2026-08-09, *incomplete*
in a way that made `(scheme base)` self-inconsistent: it exported the two promise
producers, `delay` and `delay-force`, and not the consumer, so
`(import (scheme base))` followed by `(force (delay (+ 1 2)))` failed with *no
such binding `force`*. A superset that hands out a value with no operation to
consume it is worse than either a strict surface or a complete one, so `force`
was added rather than the two producers removed — consistent with the rule that
exports are documented, never deleted.

The two `(scheme r5rs)` rows are graded differently against the authorities, and
kept for the same reason regardless. `unquote` / `unquote-splicing` are absent
from Appendix A's enumeration but are named by R5RS §4.2.6 itself, and Wile's
`(scheme base)` exports them exactly as Appendix A's `(scheme base)` does.
`call/cc` is excluded by **both** the enumeration and the library's defining
sentence — Appendix A describes `(scheme r5rs)` as the identifiers defined by
R5RS, and R7RS's own "Language changes" lists `call/cc` as an R7RS **addition**,
a synonym for `call-with-current-continuation`. Dropping it would nonetheless
break any out-of-repo program that reaches `call/cc` through `(scheme r5rs)`,
which is the break this section exists to prevent, so all three are documented
rather than removed.

Pinned by `TestLibraryExportDiff` (`pkg/wile/library_export_diff_test.go`), which
diffs every bundled `.sld`'s export list against its authority in both
directions. The table above is that test's *source* for the sanctioned-extra
column, so an extra cannot be sanctioned in code without a row here, and a row
here that no library exports fails
`TestLibraryExportDiff_DocTableHasNoStaleRows`. Each sanctioned extra is
additionally imported through `(only (scheme …) id)` — an import set that
resolves strictly through the named library's export list — so narrowing any
library back to the strict R7RS surface fails the test and forces a deliberate
update here rather than a silent public-API break.

### `(description <string>)` Library Declaration

`define-library` accepts an optional `(description <string>)` declaration that
R7RS §5.6.1 does not define among its library declarations:

```scheme
(define-library (scheme base)
  (description "R7RS base library: pairs, lists, numbers, strings, ...")
  (export ...)
  ...)
```

The string is captured at compile time into the library's summary
(`CompiledLibrary.Description` / `LibrarySummary.Description`) and surfaced by the
documentation and reflection tooling (`,doc` and `,apropos` in the REPL, `apropos`
search, and the `library-description` reflection primitive). It has no runtime
effect and does not alter the library's exports or imports. Every bundled
`.sld` file carries one.

A strict R7RS reader rejects an unrecognized library declaration, so a `.sld`
using `(description …)` does not port to such an implementation; the declaration
is Wile-internal documentation metadata, not part of any program's semantics.
Acceptance and capture are pinned by the `library_export_index` tests
(`pkg/machine/compilation`).


