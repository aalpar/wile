# Implementation Notes

This document describes implementation choices that differ from canonical R7RS reference implementations but remain semantically equivalent. These are not semantic differences — the behavior matches R7RS — but the internal implementation strategy differs.

---

## `let` / `let*` / `letrec` / `letrec*` as Core Compiled Forms

**Files:** `internal/validate/register.go` (validator registration), `machine/compilation/compile_let.go`, `machine/compilation/expander_let.go`, `machine/compilation/compile_validated.go`.

**R7RS Specification (§4.2.2):**

The R7RS canonical definitions are macro-level, each expanding to `lambda` or recursive `set!` chains. `letrec*` specifically uses a recursive nesting to enforce left-to-right evaluation:

```scheme
(define-syntax letrec*
  (syntax-rules ()
    ((letrec* () body1 body2 ...)
     (let () body1 body2 ...))
    ((letrec* ((var1 init1) (var2 init2) ...) body1 body2 ...)
     (let ((var1 <undefined>))
       (set! var1 init1)
       (letrec* ((var2 init2) ...) body1 body2 ...)))))
```

**Wile Implementation:**

All four binding forms are **core compiled**: the expander / validator / compiler pipeline handles them directly as recognized syntactic forms. They are not macros defined in `bootstrap_macros.scm`. See the comment at `registry/core/bootstrap_macros.scm:44-46`:

> Binding forms (let, let*, letrec, letrec*) are now core compiled forms,
> handled directly by the expander/validator/compiler pipeline.

(The comment also references `plans/CORE-LET.md` for "the design". That plan file has been retired; the design now lives in `docs/compiler/core-let.md`.)

**Why:**

Treating them as core forms eliminates an entire layer of macro expansion and lets the compiler apply targeted optimizations: capture analysis, escape analysis, and procedure inlining all operate directly on the `let` AST. See `docs/compiler/core-let.md` for the full design and motivation.

**Semantics preserved:**

- `let` — parallel binding; inits evaluated in an outer scope, then `OpPushEnv` allocates the frame and `StoreLocal` stores each value into the body scope.
- `let*` — sequential binding; `OpPushEnv` first, then each init is compiled followed by its `StoreLocal`, so each later init sees the prior vars.
- `letrec` / `letrec*` — `OpPushEnv` first so all slots exist before any init is evaluated; inits are then compiled in definition order, each followed by a `StoreLocal` into its slot. The ordered stores satisfy R7RS §4.2.2 for `letrec*` (strict left-to-right) and the weaker R7RS guarantee for `letrec`. See the comment summary at `machine/compilation/compile_let.go:33-37` for the opcode sequence per form.

**Reference:** R7RS §4.2.2 (Binding constructs), `docs/compiler/core-let.md`.

---

## Derived Forms Still Implemented as Macros

Not every R7RS derived form is core-compiled. The following remain `define-syntax` entries in `registry/core/bootstrap_macros.scm`, faithful to the R7RS §7.3 reference implementations except where noted:

| Form | Notes |
|------|-------|
| `and` / `or` | Standard short-circuit expansion to `if`. |
| `cond`, `case` | Standard expansion with `else` / `=>` auxiliary syntax. |
| `when`, `unless` | One-armed conditionals. |
| `delay`, `delay-force`, `force`, `make-promise` | Lazy promises via `%make-lazy-promise`. |
| `parameterize` | Uses `with-continuation-mark` rather than `dynamic-wind` — see [`r7rs-differences.md`](r7rs-differences.md) § "Parameterize Implementation". |
| `guard`, `guard-aux` | Uses `call-with-values` so the body can return multiple values — see [`r7rs-differences.md`](r7rs-differences.md) § "Guard Body Multiple Values". |
| `define-record-type` | SRFI-9 records via `make-record-type` helpers. |
| `define-opaque-record-type` | Wile extension: hidden from `record?`. |
| `let-values`, `let*-values`, `define-values` | Multiple-value binding (proper / dotted / rest patterns). |
| `do` | R7RS §4.2.4 iteration. |

**Why the split:**

Binding forms are hot paths with heavy optimization value; making them core saves expansion cost and enables the whole-program analyses in `docs/compiler/`. The derived forms in the table above are either rarely on a hot path (record definitions) or inherently tied to the continuation system (`parameterize`, `guard`) where the macro layer is exactly the right abstraction.

**Reference:** R7RS §7.3 (Derived expression types), `docs/compiler/core-let.md`, `docs/continuations/marks.md`.
