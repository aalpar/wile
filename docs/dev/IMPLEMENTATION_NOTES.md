# Implementation Notes

This document describes implementation choices that differ from canonical R7RS reference implementations but remain semantically equivalent. These are not semantic differences—the behavior matches R7RS—but the internal implementation strategy differs.

---

## `letrec*` Implementation (§4.2.2)

**File:** `registry/core/bootstrap.scm`

**R7RS Specification:**
> The `<variable>`s are bound to fresh locations, each `<variable>` is assigned in left-to-right order to the result of evaluating the corresponding `<init>`, the `<body>` is evaluated in the resulting environment.

**Canonical R7RS Definition (§7.3):**

The R7RS formal definition uses recursive nesting to enforce left-to-right evaluation:

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

```scheme
(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var init) ...) body ...)
     (letrec ((var init) ...) body ...))))
```

Simply delegates to `letrec`.

**Why This Works:**

Wile's `letrec` expands to sequential `set!` statements:

```scheme
(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var init) ...) body ...)
     (let ((var #f) ...)
       (set! var init) ...
       body ...))))
```

This expands `(letrec ((a 1) (b (+ a 1))) body)` to:

```scheme
(let ((a #f) (b #f))
  (set! a 1)
  (set! b (+ a 1))  ;; 'a' is already set when this runs
  body)
```

The `(set! var init) ...` pattern produces sequential `set!` statements, which Scheme evaluates left-to-right per R7RS §4.2.3 (sequencing). Therefore:

1. All variables are in scope for all inits (via `let ((var #f) ...)`)
2. Inits are evaluated left-to-right (via sequential `set!` statements)

**Comparison:**

| Aspect | R7RS Canonical | Wile |
|--------|---------------|------|
| Mechanism | Recursive macro nesting | Delegates to `letrec` |
| Left-to-right guarantee | Explicit in macro structure | Implicit via `letrec`'s sequential `set!` |
| Macro complexity | O(n) recursive expansions | O(1) single expansion |

**Coupling:**

This implementation couples `letrec*` to the specific behavior of Wile's `letrec`. If `letrec` were changed to evaluate inits in parallel or unspecified order (which R7RS permits for `letrec`), the `letrec*` implementation would need to be updated to the canonical recursive form.

**Reference:** R7RS §4.2.2 (Binding constructs), §7.3 (Derived expression types)
