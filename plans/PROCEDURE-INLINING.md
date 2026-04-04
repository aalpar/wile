# Procedure Inlining for Let-Bound Closures

## Goal

Eliminate closure allocation and call dispatch for let-bound lambdas that are
only ever called (never stored, passed, or returned). When a call `(f arg1 arg2)`
targets a let binding satisfying `!Mutable && !Escapes && init is lambda`, the
compiler emits the lambda body in-place as a `let` form instead of creating a
closure and performing a full call.

## Theoretical Basis

The Lambda Papers (Steele & Sussman 1976) established that `let` is syntactic
sugar for applied lambda:

```scheme
(let ((x arg1) (y arg2)) body...)
≡
((lambda (x y) body...) arg1 arg2)
```

This transform runs the equivalence in reverse: an applied lambda becomes a let.
Chez Scheme's `cp0` pass (Waddell & Dybvig 1997, "Fast and Effective Procedure
Inlining") performs the same transform at the source IR level — inlining happens
on the structured AST, not on bytecode.

## Relationship to B1/B2

B1 (capture analysis, PR #575) and B2 (escape analysis, PR #604) computed the
predicates that gate this optimization:

| Field | Question | Source |
|-------|----------|--------|
| `Mutable` | Can the binding's value change? | `markMutableBindings` |
| `Captured` | Does the value flow into escaping closures? | `markCapturedBindings` (B1) |
| `Escapes` | Is the binding referenced in non-call position? | `markEscapedBindings` (B2) |

The inlining predicate: `!Mutable && !Escapes && init is ValidatedLambda`.
`Captured` is not checked — a captured binding that is only ever called is still
inlinable. The closure stored in the binding doesn't escape; the fact that a
lambda references it from inside a closure is irrelevant to call-site inlining.

## Design

### The Transform

When the compiler encounters a `ValidatedCall` where the proc resolves to an
inlinable let binding, it constructs a synthetic `ValidatedLet` and delegates
to `CompileValidatedLet`:

```
Source:  (let ((f (lambda (x y) body1 body2)))
           ... (f arg1 arg2) ...)

At compile time, (f arg1 arg2) becomes:

Synthetic ValidatedLet:
  Kind = LetKindLet
  Bindings = [{Name: x, Init: arg1}, {Name: y, Init: arg2}]
  Body = [body1, body2]
```

The synthetic let reuses the lambda's parameter `SyntaxSymbol` objects as binding
names. Since `CompileSymbol` resolves by name + scope set (not pre-computed depth),
and `createLetCompileEnv` registers bindings via `MaybeCreateLocalBindingWithScopes`
using those same name/scope pairs, the body's symbol references resolve correctly
in the synthetic let's environment.

### What Gets Eliminated

Per inlined call site:

| Before | After | Saving |
|--------|-------|--------|
| SaveContinuation | — | Continuation frame save/restore |
| Apply dispatch | — | Arity check, env frame setup, PC reset |
| OpPopEnv on return | OpPopEnv on body exit | Same cost |

Note: the let binding's init expression (the lambda) is still compiled and
produces a MakeClosure opcode. Dead code elimination of the unused closure
is a separate optimization (not implemented). What v1 eliminates is the
**call dispatch** overhead per inlined call site.

What remains: one `OpPushEnv` frame for parameter bindings (pool-allocated).
The body's tail calls become proper tail calls of the enclosing function.

### The Predicate

A call `(f args...)` is inlinable when ALL conditions hold:

1. `f` is a `ValidatedSymbol` resolving to a let binding
2. `!binding.Mutable` — not targeted by `set!`
3. `!binding.Escapes` — only used in call position (B2)
4. `binding.Init` is a `*ValidatedLambda` (not case-lambda)
5. `lambda.Params().Rest == nil` — not variadic
6. `len(lambda.Body()) <= threshold` — body within size limit
7. `len(args) == len(lambda.Params().Required)` — arity match
8. Binding is not currently being inlined — recursion guard

Conditions 1-6 are checked when the let binding is registered as an inline
candidate. Conditions 7-8 are checked at the call site.

### Information Flow

The inlining predicate requires data from two sites:

- **Let site** (`CompileValidatedLet`): has the `ValidatedLetBinding` with
  Mutable, Escapes, Init
- **Call site** (`compileValidatedCall`): has the `ValidatedCall` with proc
  and arguments

These are bridged by an **inline candidate registry** on `CompileTimeContinuation`,
keyed by `environment.BindingID`:

```
CompileValidatedLet                    compileValidatedCall
        │                                      │
        │ for each qualifying binding:         │ resolve proc symbol:
        │   bid = ResolveBindingID(name)       │   bid = ResolveBindingID(sym)
        │   p.inlineCandidates[bid] = lambda   │   if candidate := p.inlineCandidates[bid]
        │                                      │     → construct synthetic ValidatedLet
        │                                      │     → delegate to CompileValidatedLet
        │                                      │
        └──── compile body ────────────────────┘
```

`BindingID` is the stable identity connecting the let-site metadata to the
call-site lookup — the same mechanism B1/B2 use for escape/capture tracking.

**Why a compiler-side map, not a binding annotation:** The `environment` package
cannot import `internal/validate` (the dependency runs the other direction:
`validate` → `environment`). `machine/compilation` already imports both, making
it the natural bridge. This follows the same pattern as `internal/forms` breaking
the validate↔machine cycle.

### Recursion Guard

For `(letrec ((f (lambda () (f)))) (f))`, inlining `(f)` produces the body
`(f)`, which would try to inline again — infinite recursion.

Prevention: a `currentlyInlining` set of `BindingID`s on `CompileTimeContinuation`.
Before inlining, add the BID. After compiling the synthetic let body, remove it.
Any call to the same binding encountered while compiling the inlined body falls
through to normal call compilation.

### Configuration

The inline threshold is an Engine option:

```go
WithInlineThreshold(n int)  // default: 5 expressions; 0 disables inlining
```

Threaded from `Engine` → `CompileTimeContinuation` as a field. The threshold
counts top-level body expressions (`len(lambda.Body())`), not AST nodes or
estimated instructions. This is the simplest metric and matches how Scheme
programmers think about procedure size.

### Scope Resolution

The synthetic `ValidatedLet` works because:

1. `ValidatedSymbol` stores `*syntax.SyntaxSymbol` (name + scope set) — no
   pre-computed depth
2. `CompileSymbol` resolves depth at compile time via the current environment
3. `createLetCompileEnv` registers bindings using `MaybeCreateLocalBindingWithScopes`
   with the lambda parameter's exact name and scope set
4. Free variables in the lambda body resolve via the parent chain, which is the
   calling scope — correct because the lambda was defined in this scope

Depth is recomputed, not inherited. The body's symbols find the right bindings
regardless of whether they were originally validated inside a lambda or are now
compiled inside a synthetic let.

## v1 Restrictions

| Restriction | Rationale | Future extension |
|-------------|-----------|------------------|
| No variadic lambdas | Rest parameter needs list construction | Add rest-list codegen |
| No case-lambda | Multiple clauses need dispatch | Match on arity at compile time |
| No inter-procedural | Only let-bound, not top-level or imported | Requires top-level mutability tracking |
| No recursive self-inlining | Recursion guard prevents infinite compile | Effort counter (cp0-style) |
| Expression count only | No instruction or AST node cost model | Instruction-weighted size metric |

Each restriction is an additive extension. The core transform is correct regardless.

## Test Cases

### Predicate Tests

| Code | Inlined? | Why |
|------|----------|-----|
| `(let ((f (lambda (x) x))) (f 1))` | Yes | All conditions met |
| `(let ((f (lambda (x) x))) (set! f id) (f 1))` | No | Mutable |
| `(let ((f (lambda (x) x))) (g f))` | No | Escapes (passed as arg) |
| `(let ((f (lambda (x) x))) f)` | No | Escapes (returned) |
| `(let ((f (lambda (x . rest) rest))) (f 1 2))` | No | Variadic |
| `(let ((f (lambda (a b c d e f) ...))) (f 1 2 3 4 5 6))` | Depends | Body length vs threshold |
| `(let ((f (lambda (x) x))) (f 1 2))` | No | Arity mismatch |

### Transform Tests

| Code | Expected equivalent |
|------|---------------------|
| `(let ((f (lambda (x) (+ x 1)))) (f 42))` | `(let ((x 42)) (+ x 1))` |
| `(let ((f (lambda () 1))) (f))` | `(let () 1)` (thunk → empty let) |
| `(let ((f (lambda (x y) (+ x y)))) (f (+ 1 2) (* 3 4)))` | `(let ((x (+ 1 2)) (y (* 3 4))) (+ x y))` |

### Full-Pipeline Integration Tests

| Code | Expected result |
|------|-----------------|
| `(let ((f (lambda (x) (+ x 1)))) (f 42))` | `43` |
| `(let ((f (lambda (x) (+ x 1)))) (+ (f 1) (f 2)))` | `5` |
| `(let ((f (lambda (x y) (+ x y)))) (f 3 4))` | `7` |
| `(let ((add (lambda (a b) (+ a b))) (mul (lambda (a b) (* a b)))) (add (mul 2 3) (mul 4 5)))` | `26` |
| `(let ((f (lambda (x) (if (= x 0) 1 (* x (f (- x 1))))))) (f 5))` — letrec | `120` (not inlined — recursive) |
| `(let ((x 10)) (let ((f (lambda (y) (+ x y)))) (f 32)))` — free variable | `42` |
| Tail position: `(let ((f (lambda (x) x))) (f 42))` as tail expr | `42` (proper tail call) |

### Benchmark Verification

Gabriel benchmarks (`make bench-gabriel`) should show no regression. Benchmarks
that use let-bound helper procedures (tak, fib) may show improvement.

## Implementation Sequence

1. Add `inlineCandidates` and `currentlyInlining` maps to `CompileTimeContinuation`
2. Add `InlineThreshold` field, wire Engine option
3. Register inline candidates in `CompileValidatedLet` (check predicate)
4. Clean up candidates on scope exit
5. Detect inlinable calls in `compileValidatedCall`
6. Construct synthetic `ValidatedLet` and delegate
7. Unit tests on predicate and transform
8. Full-pipeline integration tests
9. Benchmark verification

## References

- Steele & Sussman, "Lambda: The Ultimate Imperative" (1976) — let = applied lambda
- Waddell & Dybvig, "Fast and Effective Procedure Inlining" (1997) — cp0 design
- Dybvig, *Three Implementation Models for Scheme* (1987) — direct-style compilation
- `plans/ESCAPE-ANALYSIS.md` — B2 design (Escapes field)
- `plans/CAPTURE-ANALYSIS.md` — B1 design (Captured field)
- `plans/PERFORMANCE.md` — optimization roadmap
