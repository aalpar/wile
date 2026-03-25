# Core `let` Compilation

**Status:** Design complete, not yet implemented
**Date:** 2026-03-24 (updated 2026-03-25: merged Phase 2, added mutability tracking)
**Motivation:** Enable `let`, `let*`, `letrec`, `letrec*` as `ValidatedExpr` forms with per-binding mutability tracking — foundation for future inlining, ANF IR, constant propagation, dead binding elimination

## Problem

`let` is a macro that expands to `((lambda (name ...) body ...) val ...)`. Every `let` form pays:
- `SaveContinuation` + `RestoreContinuation` (continuation chain manipulation)
- `MakeClosure` (template stored in literals pool)
- `Apply` (arity check, env frame setup, parameter binding)
- ~5-6 dispatch cycles for what should be "store values in slots"

The peephole optimizer is blind to this pattern — it fuses `PushCachedBinding...PullApply` (foreign calls) and `PushLocal...PullApply` (local calls), but lambda closures stored as literals don't match either pattern.

More importantly, because `let` disappears during macro expansion, the compiler never sees binding structure. It sees a lambda application. This blocks future optimization passes (ANF, constant propagation, dead binding elimination) that need explicit binding information.

The same applies to `letrec`/`letrec*` — they chain through macro expansion today (`letrec*` → `letrec` → `let` → `lambda`), hiding recursive binding structure from the compiler. Making all four binding forms core means the compiler can distinguish "local non-recursive binding" from "recursive closure binding" — information that closure optimization and inlining both need.

## Approach

**Approach B: all binding forms become core forms.** The expander recognizes them, the validator produces `ValidatedLet`/`ValidatedLetStar`/`ValidatedLetrec`, the compiler emits direct local-slot bytecode. The macros are removed.

Alternatives considered:
- **A: Pattern recognition in validator** — fragile, depends on exact macro expansion shape
- **C: Internal `%let` form** — adds surface area without cleaning up the root issue

## Design

### New Validated Types

In `internal/validate/validated_forms.go`:

```go
type ValidatedLetBinding struct {
    Name    *syntax.SyntaxSymbol
    Init    ValidatedExpr
    Mutable bool  // true if targeted by set! in the body
}

type ValidatedLet struct {
    validatedBase
    Bindings []ValidatedLetBinding
    body     []ValidatedExpr
}

type ValidatedLetStar struct {
    validatedBase
    Bindings []ValidatedLetBinding
    body     []ValidatedExpr
}

type ValidatedLetrec struct {
    validatedBase
    Bindings   []ValidatedLetBinding
    LetrecStar bool
    Tag        *syntax.SyntaxSymbol   // non-nil for named let
    body       []ValidatedExpr
}
```

Separate types for `let`, `let*`, and `letrec` — each has distinct scoping semantics that the compiler handles differently. `letrec` and `letrec*` share a type with a `LetrecStar` flag because their compilation differs only in init evaluation order. `Tag` is non-nil for named let but is diagnostic only — the body always contains the call expression (a `ValidatedCall` of the tag with the init values). The compiler does not special-case named let.

### Expander

New file `machine/expander_let.go`. Registered in `primitive_expanders_registry.go`.

**`expandLetForm`:**
1. Parse: detect named let (second element is symbol) vs plain let
2. Named let: expand to `(letrec ((tag (lambda (names...) body...))) (tag vals...))` — same output as today's macro, but in Go. Returns the expanded letrec form for `expandLetrecForm` to handle.
3. Plain let: create fresh binding scope, add to names + body (NOT init expressions). Expand init exprs in current env. Create child env with placeholder bindings. Expand body via `ExpandBodyWithDefineSyntax`. Reconstruct `(let ...)` syntax.

**`expandLetStarForm`:**
1. Create binding scope
2. For each binding sequentially: expand init in current env, add binding to env, advance
3. Expand body in fully-populated child env
4. Reconstruct `(let* ...)` syntax

**`expandLetrecForm`:**
1. Create binding scope, add to names + body + ALL init expressions (R7RS §4.2.2: all bindings in scope during all inits)
2. Create child env with all bindings
3. Expand all init expressions in child env
4. Expand body via `ExpandBodyWithDefineSyntax`
5. Reconstruct `(letrec ...)` syntax

**`expandLetrecStarForm`:**
Same as letrec. The scoping is identical — all bindings are in scope for all inits. The difference (left-to-right evaluation, preceding bindings have values) is a runtime semantic enforced by compilation order, not by scoping.

**Hygiene — scoping precision (improvement over macro):**
- `let`: scope on names + body, NOT on init exprs (R7RS §4.2.2)
- `let*`: scope on names + body, each init sees only preceding bindings
- `letrec`/`letrec*`: scope on names + body + ALL init exprs

The current macros apply `with-binding-scope` uniformly to everything. Core forms are more precise.

### Validator

New file `internal/validate/validate_let.go`.

**Shared helper:**
```go
func validateLetBindings(
    ctx context.Context,
    env *environment.EnvironmentFrame,
    bindingsExpr syntax.SyntaxValue,
    formName string,
    result *ValidationResult,
) ([]ValidatedLetBinding, bool)
```
Parses `((name val) ...)`, validates each init, returns binding slice.

**`validateLet`:**
1. `formPrologue` — collect elements, verify proper list
2. Detect named let (second element is symbol) → delegate to `validateNamedLet`
3. Parse and validate bindings (init exprs validated in current env)
4. Create child env with bindings for body (like `createLambdaValidationEnv`)
5. `validateBodySlice` for body in child env
6. Return `*ValidatedLet`

**`validateLetStar`:**
Same, but child env built incrementally — each init validated with preceding bindings visible.

**`validateLetrec` / `validateLetrecStar`:**
1. `formPrologue`
2. Create child env with ALL bindings (all names visible)
3. Validate all init expressions in child env
4. `validateBodySlice` for body in child env
5. Return `*ValidatedLetrec` (with `LetrecStar` flag set appropriately)

**`validateNamedLet`:**
1. Parse tag symbol, bindings, body
2. Validate init expressions in outer env (don't see tag)
3. Validate body in child env (tag + binding names visible)
4. Build `ValidatedLambda` from binding names + validated body
5. Build `ValidatedCall` of tag with init values as args
6. Return `*ValidatedLetrec` with one binding `(tag, lambda)`, `Tag` set, and body = `[callExpr]`

The body is always a single `ValidatedCall` — the compiler's standard body compilation handles it with no named-let-specific logic.

### Mutability Tracking

Integrated into the validator pass — no second walk required.

**Problem:** Future optimizations (inlining, constant propagation, dead binding elimination) need to know which bindings are never `set!`-ed. Currently `validateSetBang` doesn't resolve the target binding — it stores the symbol and defers resolution to the compiler. No mutability information flows from `set!` sites to binding sites.

**Mechanism:** Binding resolution in `validateSetBang` + a mutated-bindings set on `ValidationResult` + post-body marking on `ValidatedLetBinding`.

**Step 1: Extend `ValidationResult`**

```go
type ValidationResult struct {
    Expr           ValidatedExpr
    Errors         []ValidationError
    mutatedBindings map[*environment.Binding]bool  // set! targets (pointer identity)
}

func (p *ValidationResult) markMutated(b *environment.Binding) {
    if p.mutatedBindings == nil {
        p.mutatedBindings = make(map[*environment.Binding]bool)
    }
    p.mutatedBindings[b] = true
}

func (p *ValidationResult) isMutated(b *environment.Binding) bool {
    return p.mutatedBindings[b]
}
```

Lazily initialized — zero cost when no `set!` appears.

**Step 2: Resolve and mark in `validateSetBang`**

```go
func validateSetBang(ctx context.Context, env *environment.EnvironmentFrame,
    pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
    // ... existing validation ...

    // NEW: resolve the target binding and mark it mutable.
    // Opportunistic — if resolution fails, the compiler will catch
    // the error. We don't add a validation error here.
    binding := env.GetBindingWithScopes(name.Sym, name.Scopes())
    if binding != nil {
        result.markMutated(binding)
    }

    return &ValidatedSetBang{ /* ... */ }
}
```

**Step 3: Check mutability after body validation in let validators**

After `validateBodySlice` returns, each let validator cross-references its bindings with the mutated set:

```go
// In validateLet, after body validation:
childEnv := createLetValidationEnv(env, bindings)
body, ok := validateBodySlice(ctx, childEnv, elements, 2, result)

// Mark mutable bindings
for i, b := range bindings {
    binding := childEnv.GetBindingWithScopes(b.Name.Sym, b.Name.Scopes())
    if binding != nil && result.isMutated(binding) {
        bindings[i].Mutable = true
    }
}
```

**Why this works:**

`createLetValidationEnv` creates a `*Binding` object B1 for binding `x`. Body validation (including nested `set!`) happens in the child env containing B1. When `validateSetBang` encounters `(set! x 2)`, it resolves through the env chain to B1 and adds it to the mutated set. After body validation, the let validator checks: "is B1 in the set?" — pointer identity, O(1).

**Scope isolation is automatic.** Two `let` forms with the same variable name create different `*Binding` objects in different env frames. The mutated set uses pointer identity, so marking one `x` doesn't affect the other.

**Nested `set!` targeting outer bindings works correctly:**

```scheme
(let ((x 1))           ; creates B1
  (let ((y 2))         ; creates B2
    (set! x 3))        ; resolves to B1, marks B1
  x)
```

The outer let's post-body check finds B1 in the mutated set → marks `x` as `Mutable: true`. The inner let's check finds B2 is NOT in the set → `y` stays `Mutable: false`.

**What the compiler does with it:** The `Mutable` flag is a conservative lower bound on immutability. `Mutable: false` means "no `set!` in this validation scope targets this binding" — safe for inlining and constant propagation. `Mutable: true` means "at least one `set!` targets this binding" — the compiler must not assume a fixed value. See `docs/learn/inlining-after-core-let.md`.

### New Opcode: `OpPushEnv`

In `machine/opcode.go`:

```go
OpPushEnv  // Push a new env frame with Arg local slots
```

**Semantics:** Acquire env frame from pool with `Arg` local slots. Chain to `mc.env` as parent. Set `mc.env` to new frame.

**Pairing:** `OpPopEnv` already exists and does `mc.env = mc.env.Parent()`.

**Tail position:** When a binding form is in tail position, `OpPopEnv` is not emitted — the body's final expression either returns via `RestoreContinuation` (which restores saved env) or tail-calls (which replaces env). The binding form's env frame becomes garbage.

**Implementation:** Inline in `Run()` switch — simple enough (pool acquire + two pointer sets). No `OpComplex` side table needed.

### Compiler

New file `machine/compile_let.go`.

**`CompileValidatedLet`:**
```
;; (let ((x 1) (y 2)) (+ x y))

<compile 1>  Push         ; init exprs compiled in parent env
<compile 2>  Push
OpPushEnv(2)               ; new env frame with 2 slots
StoreLocal y               ; pop from stack into slots
StoreLocal x
<compile body>             ; last expr inherits tail position
OpPopEnv                   ; only if let is NOT in tail position
```

Note: init expressions are compiled before `OpPushEnv` — they don't see the let bindings. Store order is reverse (stack is LIFO).

**`CompileValidatedLetStar`:**
```
;; (let* ((x 1) (y (+ x 1))) body)

OpPushEnv(2)               ; all slots allocated upfront
<compile 1>
StoreLocal x               ; slot 0 — x now visible at compile time
<compile (+ x 1)>          ; x resolved via compile-time env
StoreLocal y               ; slot 1
<compile body>
OpPopEnv                   ; only if not tail
```

Single env frame. Compiler makes each binding visible in compile-time env after emitting its `StoreLocal`.

**`CompileValidatedLetrec` (non-star):**
```
;; (letrec ((f (lambda (n) ...))) (f 5))

OpPushEnv(1)               ; all bindings visible immediately (for recursive refs)
<compile init-1> Push      ; f is in scope during init compilation
<compile init-2> Push      ; (if multiple bindings)
StoreLocal f-2             ; delayed assignment — all inits evaluated first
StoreLocal f-1
<compile body>
OpPopEnv                   ; only if not tail
```

All bindings are added to the compile-time env before any init is compiled (so recursive references resolve). But values are assigned only after all inits are evaluated — the "delayed assignment" pattern matches R7RS's "unspecified evaluation order" semantics.

**`CompileValidatedLetrec` (star):**
```
;; (letrec* ((x 1) (y (+ x 1))) body)

OpPushEnv(2)               ; all bindings visible immediately
<compile 1>
StoreLocal x               ; x now has its value
<compile (+ x 1)>          ; sees x's actual value
StoreLocal y
<compile body>
OpPopEnv                   ; only if not tail
```

Sequential — each init is stored immediately. Like `let*` but all bindings are in scope from the start (for forward references in lambdas).

**Named `let` compilation:**

Named let produces a standard `ValidatedLetrec` whose body is a `ValidatedCall`. No special compilation logic — the letrec compiler handles it:

```
;; (let loop ((n 5) (acc 1)) body)
;; ValidatedLetrec: binding=(loop, lambda), body=[(loop 5 1)]
;; Compiled identically to: (letrec ((loop (lambda (n acc) body))) (loop 5 1))

OpPushEnv(1)               ; slot for loop
<compile (lambda (n acc) body)>  ; loop visible for self-reference
Push
StoreLocal loop
<compile body: (loop 5 1)> ; standard call compilation
OpPopEnv                   ; only if not tail
```

The `Tag` field on `ValidatedLetrec` is diagnostic only (error messages, future self-tail-call optimization). The compiler never branches on it.

### Registration Changes

| File | Change |
|------|--------|
| `registry/core/specialforms.go` | Add `"let"`, `"let*"`, `"letrec"`, `"letrec*"` to `compileTimeBindings` |
| `machine/primitive_expanders_registry.go` | Register `expandLetForm`, `expandLetStarForm`, `expandLetrecForm`, `expandLetrecStarForm` |
| `internal/validate/register.go` | Register `validateLet`, `validateLetStar`, `validateLetrec`, `validateLetrecStar` |
| `machine/register.go` | Register typed compilers for all four forms |
| `registry/core/bootstrap_macros.scm` | Remove `let`, `let*`, `letrec`, `letrec*` macro definitions |

### Macro Removal

Remove from `bootstrap_macros.scm`:
- `let` definition (both plain and named clauses)
- `let*` definition
- `letrec` definition
- `letrec*` definition

`with-binding-scope` stays — still used by user-defined binding macros.

### What Doesn't Change

- VM execution model (continuation chain, stack, apply)
- Peephole optimizer (new bytecode doesn't use patterns it would fuse)
- `with-binding-scope` expander

### Behavioral Change: `letrec` Initial Values

The current `letrec` macro initializes bindings to `#f` before the `set!` assignments:

```scheme
(letrec ((f init)) body) → (let ((f #f)) (set! f init) body)
```

The core form uses `NewLocalEnvironment(N)` where slots default to `values.Void`. Accessing a `letrec` binding before its init completes is an error per R7RS §4.2.2, so neither value is "correct" for erroneous programs. `Void` is arguably better — it produces a clearer error than `#f` if accidentally accessed. Document in `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md`.

## Testing

**Unit tests per layer:**

| Layer | File | Coverage |
|-------|------|----------|
| Expander | `expander_let_test.go` | Plain let, named let → letrec, let*, letrec, letrec*, hygiene scopes |
| Validator | `validate_let_test.go` | All four forms, named let, duplicate bindings, missing body, malformed bindings, shadowing, mutability tracking |
| Compiler | `compile_let_test.go` | Bytecode shape (PushEnv/StoreLocal/PopEnv), let* sequential visibility, letrec recursive refs, letrec* sequential + recursive, tail position, nested binding forms |
| VM opcode | `operation_test.go` | OpPushEnv frame creation, OpPopEnv restore |

**Integration tests:**

| Test | Expected |
|------|----------|
| `(let ((x 1)) x)` | 1 |
| `(let ((x 1) (y 2)) (+ x y))` | 3 |
| `(let ((x 1) (y x)) y)` | error — x not visible to y's init in `let` |
| `(let* ((x 1) (y (+ x 1))) y)` | 2 |
| `(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))` | 120 |
| `(let ((x 1)) (let ((y 2)) (+ x y)))` | 3 |
| `(define (f) (let ((x 1)) x))` then `(f)` | 1 (tail position) |
| `(let ((x 1)) (set! x 2) x)` | 2 |
| `(let ((x 1)) (let ((f (lambda () x))) (f)))` | 1 (closure capture) |
| `call/cc` inside let | Continuation captures let env correctly |
| `(let ((if 42)) if)` | 42 (shadowing special forms) |
| `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))` | 120 |
| `(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))` | #t (mutual recursion) |
| `(letrec* ((x 1) (y (+ x 1))) y)` | 2 |
| `(letrec* ((f (lambda () g)) (g 42)) (f))` | 42 (forward reference via closure) |

**Macro-generated `let` tests (macros that expand TO `let` internally):**

| Test | Expected | Why |
|------|----------|-----|
| `(cond (#t 42))` | 42 | `cond` with true clause |
| `(cond (#f 1) (#t 2))` | 2 | `cond` multi-clause |
| `(cond ((assv 2 '((1 one) (2 two) (3 three))) => cdr))` | `two` | `cond` `=>` clause creates `let` binding for temp |
| `(case (+ 1 1) ((1) 'one) ((2) 'two) ((3) 'three))` | `two` | `case` creates `let` binding for key |
| `(do ((i 0 (+ i 1))) ((= i 5) i))` | 5 | `do` uses named `let` internally |
| `(let ((x 1)) (define y 2) (+ x y))` | 3 | internal `define` in let body (R7RS §5.3) |
| `(let ((let 42)) let)` | 42 | `let` shadowing `let` keyword |
| `(and 1 2 3)` | 3 | `and` may expand through `let` |
| `(or #f #f 42)` | 42 | `or` may expand through `let` |

These tests verify that macros which produce `let` syntax continue to work correctly when `let` is a core form handled by the expander instead of a macro.

**Mutability tracking tests:**

| Test | Expected |
|------|----------|
| `(let ((x 1)) x)` | `x.Mutable = false` |
| `(let ((x 1)) (set! x 2) x)` | `x.Mutable = true` |
| `(let ((x 1) (y 2)) (set! x 3) y)` | `x.Mutable = true`, `y.Mutable = false` |
| `(let ((x 1)) (let ((y 2)) (set! x 3)) x)` | outer `x.Mutable = true`, inner `y.Mutable = false` |
| `(let ((x 1)) (let ((x 2)) (set! x 3)) x)` | outer `x.Mutable = false`, inner `x.Mutable = true` |
| `(letrec ((f (lambda (n) ...))) (f 5))` | `f.Mutable = false` |
| `(letrec ((f (lambda (n) ...))) (set! f #f) (f 5))` | `f.Mutable = true` |

The shadow test (row 5) is critical — two bindings named `x` in nested scopes, only the inner one mutated. Pointer identity on `*Binding` objects ensures correct scope resolution.

**Benchmarks:** `make bench-gabriel` before/after for binding-heavy programs.

## Performance Expectation

Each `let`/`let*` saves ~4 dispatches (no SaveContinuation, MakeClosure, Apply, RestoreContinuation) and avoids the closure template literal. Env frame allocation is a wash (pool acquisition either way). Binding-heavy benchmarks should show 2-5% improvement.

`letrec` additionally benefits recursive closures — the closure is compiled with its own binding visible, eliminating the `(set! f <undefined>)` / `(set! f (lambda ...))` pattern that the current macro produces.

The real win is structural: the compiler sees binding pairs, not reconstructed lambdas. This is the foundation for ANF, constant propagation, and dead binding elimination.
