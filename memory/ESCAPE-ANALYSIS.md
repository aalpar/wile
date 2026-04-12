# Escape Analysis for Let-Bound Closures (B2)

## Goal

Add an `Escapes bool` field to `ValidatedLetBinding` that tracks whether a
binding is referenced in any position other than call operator. When a binding
holds a closure and `Escapes` is false, the closure is only ever called — never
stored, passed as argument, or returned — enabling downstream optimizations.

## Relationship to B1 (Capture Analysis)

B1 asks: "Is this binding's value referenced from inside an escaping closure?"
B2 asks: "Is this binding itself used anywhere other than call position?"

These are orthogonal properties of the same binding:

| B1 Captured | B2 Escapes | Meaning |
|-------------|------------|---------|
| false | false | Binding is local and call-only. Ideal for inlining. |
| false | true | Binding is local but passed around. No inlining. |
| true | false | Binding is captured by escaping closures, but only called. Enables effective capture refinement (see below). |
| true | true | Binding is captured and escapes. No optimization. |

Together they enable:
- **Procedure inlining**: `!Mutable && !Escapes && init is lambda` — replace
  `(f args...)` with the lambda body, substituting parameters
- **Effective capture refinement**: A binding marked `Captured` by B1 may be
  effectively non-captured if every lambda that references it is stored in a
  non-escaping binding. This is cross-binding analysis — future work.
- **Environment frame slimming**: Bindings that are `!Captured` (or effectively
  non-captured after refinement) can use lightweight frames

## Definitions

**Call position**: A symbol reference is in call position when it is the `Proc`
of a `ValidatedCall` or the `Proc` of a `ValidatedApply`. All other reference
positions are non-call: argument, return value, init expression, stored in a
data structure. `set!` is not a reference — it is mutation, tracked by `Mutable`.

**Escapes**: A let binding escapes when it is referenced at least once in a
non-call position anywhere in the let body (or inits, for letrec variants).

**Non-escaping**: Every reference to the binding is in call position.

## Design

### Data Model

```go
// validated_forms.go
type ValidatedLetBinding struct {
    Name     *syntax.SyntaxSymbol
    Init     ValidatedExpr
    Mutable  bool
    Captured bool   // B1: referenced from inside escaping closure
    Escapes  bool   // B2: used in non-call position
}
```

`Escapes` defaults to `false`. The walker sets it to `true` when it finds a
non-call-position reference. Same best-effort contract as B1: if binding
resolution fails (scope mismatch), the binding stays non-escaping. Must not
gate correctness-critical optimizations without re-validation.

### Walker: markEscapedBindings

Post-validation walk, called at the same 5 sites as `markCapturedBindings`:

```go
func markEscapedBindings(
    childEnv  *environment.EnvironmentFrame,
    bindings  []ValidatedLetBinding,
    body      []ValidatedExpr,
    walkInits bool,
)
```

`walkInits` semantics identical to B1: true for let\*/letrec/letrec\*, false
for plain let.

### Walk Rules

The critical difference from B1: B2 does not track closure depth. A reference
in call position inside an escaping closure is still in call position.

```scheme
(let ((f (lambda () 42)))
  (lambda () (f)))   ; f: Captured=true (B1), Escapes=false (B2)
```

The inner lambda escapes, and f is captured by it, but f is only ever *called*.
The closure stored in f does not escape.

| Context | Action |
|---------|--------|
| `ValidatedCall.Proc` is `ValidatedSymbol` matching a binding | Call position. Do NOT mark Escapes. Walk args normally. |
| `ValidatedApply.Proc` is `ValidatedSymbol` matching a binding | Call position. Do NOT mark Escapes. Walk prefix args and final list normally. |
| `ValidatedSymbol` matching a binding in any other context | Non-call position. Mark `Escapes = true`. |
| `ValidatedSetBang` | Walk the value expression only. Target mutation is tracked by `Mutable`, not `Escapes`. |
| `ValidatedLambda` | Walk body (no depth tracking needed). |
| `ValidatedCaseLambda` | Walk each clause body. |
| `ValidatedCall` with non-symbol proc or non-matching symbol | Walk proc normally (non-call context), walk args normally. |
| `ValidatedIf` | Walk test, consequent, alternative. |
| `ValidatedBegin` | Walk body. |
| `ValidatedLet` (nested) | Walk inits and body. |
| `ValidatedDefine` (function) | Walk body. |
| `ValidatedDefine` (value) | Walk sub-expression. |
| `ValidatedDynamicWind` | Walk before, thunk, after. |
| `ValidatedWithContinuationMark` | Walk key, val, body. |
| `ValidatedQuote`, `ValidatedLiteral`, `ValidatedQuasiquote` | Skip. |

### Position Detection

B2 does not need a "position" parameter threaded through the walk. Call position
is detected structurally: when processing a `ValidatedCall`, check whether
`Proc` is a `ValidatedSymbol` referencing a tracked binding *before* recursing.

```go
case *ValidatedCall:
    if sym, ok := e.Proc().(*ValidatedSymbol); ok {
        bid, resolved := p.env.ResolveBindingID(sym.Symbol.Sym, sym.Symbol.Scopes())
        if resolved {
            if _, tracked := p.idToIdx[bid]; tracked {
                // Call position — don't mark Escapes.
                // Fall through to walk args only.
                goto walkArgs
            }
        }
    }
    // Proc is not a tracked binding — walk it as non-call context
    p.walkExpr(e.Proc())
walkArgs:
    for _, arg := range e.Body() {
        p.walkExpr(arg)
    }
```

(Pseudocode — actual implementation should avoid goto if a cleaner structure
exists. The point is: call-position detection is local to the `ValidatedCall`
case, not threaded as context.)

### Interaction with set!

`(set! f expr)` does NOT mark `Escapes = true`. Mutation is already tracked by
`Mutable` — duplicating that signal in `Escapes` would violate the implicational
base: each field should carry information not derivable from the others.

The three fields form an orthogonal basis:
- `Mutable`: can the binding's value change?
- `Captured`: does the value flow into escaping closures?
- `Escapes`: is the binding referenced in non-call position?

The compiler combines them: `!Mutable && !Escapes && init is lambda` enables
inlining. Each conjunct rules out a different failure mode.

The `ValidatedSetBang` case still walks the *value expression* (the right-hand
side of `set!`) to detect non-call references to other tracked bindings.

### Non-Lambda Bindings

The analysis is syntactic: it does not inspect whether the init is a lambda.
For a binding like `(let ((x 1)) (+ x 1))`, `x` is used as an argument to `+`
(non-call position), so `Escapes = true`. This is correct but irrelevant — the
compiler would only attempt inlining when the init is a `ValidatedLambda` or
`ValidatedCaseLambda`. Computing `Escapes` for all bindings keeps the analysis
general and decoupled from init types.

### Separate Pass vs Combined Walk

B2 could be fused into the B1 `captureWalker` (both walk the same tree, same
binding map). Keeping them separate:

1. **Independence** — testable and understandable in isolation
2. **Orthogonality** — B1 tracks closure depth, B2 tracks call position
3. **Cost** — two O(n) walks are still O(n); the AST is small relative to
   compilation and expansion cost

If profiling shows the double walk matters, fuse later.

## What This Does NOT Do

- The compiler does not use `Escapes` yet. Infrastructure only.
- No procedure inlining. Requires `!Mutable && !Escapes && init is lambda`
  plus size heuristics — separate work.
- No effective capture refinement. Determining that a `Captured` binding is
  effectively non-captured because the capturing lambda lives in a non-escaping
  binding requires cross-binding analysis. Future work.
- No inter-procedural escape analysis. Tracing a closure passed to a known
  non-escaping parameter (e.g., `map`'s function argument) requires
  whole-program or modular analysis. Out of scope.

## Test Cases

| Code | Expected |
|------|----------|
| `(let ((f (lambda () 1))) (f))` | `Escapes: false` |
| `(let ((f (lambda () 1))) f)` | `Escapes: true` — returned |
| `(let ((f (lambda () 1))) (g f))` | `Escapes: true` — passed as argument |
| `(let ((f (lambda () 1))) (f) f)` | `Escapes: true` — one call, one non-call |
| `(let ((f (lambda () 1))) (apply f '()))` | `Escapes: false` — apply proc position |
| `(let ((f (lambda () 1)) (g (lambda () 2))) (f) (g))` | `f: false, g: false` |
| `(let ((f (lambda () 1)) (g (lambda () 2))) (f) g)` | `f: false, g: true` |
| `(let ((f (lambda () 1))) (set! f (lambda () 2)))` | `Escapes: false` — set! is mutation (`Mutable`), not escape |
| `(let ((f (lambda () 42))) (lambda () (f)))` | `Escapes: false` — call inside closure |
| `(let ((f (lambda () 42))) (lambda () f))` | `Escapes: true` — non-call inside closure |
| `(let ((x 1)) x)` | `Escapes: true` — non-call (analysis is syntactic) |
| `(let ((f (lambda () 1))) (if #t (f) (f)))` | `Escapes: false` |
| `(let ((f (lambda () 1))) (if #t f (f)))` | `Escapes: true` |
| Named let: `(let loop ((x 1)) (if (= x 0) x (loop (- x 1))))` | `loop: false, x: true` |
| `(letrec ((f (lambda () (f)))) (f))` | `Escapes: false` — self-recursive call |
| `(let* ((f (lambda () 1)) (g f)) (g))` | `f: true` — f used as init for g (non-call) |

## Implementation Sequence

1. Add `Escapes bool` to `ValidatedLetBinding` (zero-value compatible)
2. Write `escapeWalker` in `validate_escape.go` with unit tests
3. Wire `markEscapedBindings` into the 5 call sites in `validate_let.go`
4. Full-pipeline integration tests (named let, letrec self-recursion)
5. Benchmark verification (no regression from the additional walk)
