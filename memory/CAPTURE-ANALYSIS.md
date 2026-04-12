# Capture Analysis for Let Bindings

## Goal

Add a `Captured bool` field to `ValidatedLetBinding` that tracks whether a
binding is referenced from inside an escaping closure. This enables downstream
optimizations (constant propagation, environment frame elimination, stack-slot
bindings) that require knowing whether a binding's lifetime extends beyond the
let form.

## Definitions

**Captured**: A let binding is captured when it is referenced from inside a
`lambda` or `case-lambda` that is not immediately applied. "Immediately applied"
means the lambda appears as the operator of a `ValidatedCall` — the syntactic
pattern `((lambda ...) args...)`.

**Non-captured**: A binding referenced only from the let body (outside any
lambda boundary) or from inside an immediately-applied lambda.

## Design

### Data Model

```go
// validated_forms.go
type ValidatedLetBinding struct {
    Name     *syntax.SyntaxSymbol
    Init     ValidatedExpr
    Mutable  bool
    Captured bool  // true if referenced from inside an escaping closure
}
```

### Walker: markCapturedBindings

A post-validation walk over the `ValidatedExpr` body, called at the same 4
sites as `markMutableBindings` in `validate_let.go`:

1. `validateLetBindingsAndBody`
2. `validateLetStarFlat`
3. `validateLetStarNested` (per-binding)
4. `validateLetrecBindingsAndBody`

Signature:

```go
func markCapturedBindings(
    childEnv  *environment.EnvironmentFrame,
    bindings  []ValidatedLetBinding,
    body      []ValidatedExpr,
    walkInits bool,
)
```

`walkInits` controls whether init expressions are walked for capture analysis.
Must be `true` for `let*`, `letrec`, and `letrec*` (where inits see the
bindings) and `false` for plain `let` (where inits are in the outer scope).
See `plans/CAPTURE-ANALYSIS-IMPL.md` "Design Refinement" for the full rationale.

The function builds a `map[BindingID]int` mapping each let binding's ID to its
index in `bindings[]`, then walks the body (and optionally inits) with
`closureDepth = 0`.

### Walk Rules

| Node type | Action |
|---|---|
| `ValidatedSymbol` | Resolve via `ResolveBindingID`. If it maps to a let binding and `closureDepth > 0`, set `Captured = true`. |
| `ValidatedLambda` | Walk body at `closureDepth + 1`. |
| `ValidatedCaseLambda` | Walk each clause body at `closureDepth + 1`. |
| `ValidatedCall` with lambda proc | **Immediately applied**: walk proc's body at current depth, walk args at current depth. |
| `ValidatedCall` with non-lambda proc | Walk proc and args at current depth. |
| `ValidatedIf` | Walk test, consequent, alternative at current depth. |
| `ValidatedBegin` | Walk body expressions at current depth. |
| `ValidatedSetBang` | Walk the value expression at current depth. |
| `ValidatedLet` (nested) | Walk init expressions at current depth. Walk body at current depth (the nested let does its own capture analysis). |
| `ValidatedDynamicWind` | Walk before, thunk, after at current depth. |
| `ValidatedWithContinuationMark` | Walk key, val, body at current depth. |
| `ValidatedApply` | Walk proc and args at current depth. |
| `ValidatedQuote`, `ValidatedLiteral` | No sub-expressions — skip. |
| `ValidatedQuasiquote` | No sub-expressions at validation level (deferred to compiler) — skip. |

### Immediately-Applied Lambda Detection (B1)

```go
switch proc := call.Proc().(type) {
case *ValidatedLambda:
    // Non-escaping: walk body at current depth, args at current depth
case *ValidatedCaseLambda:
    // Non-escaping: walk each clause body at current depth, args at current depth
default:
    // Walk proc and args at current depth (lambda case handles depth increment)
}
```

The lambda's body is walked at current depth (not depth+1) because the closure
is called inline and does not escape. The call arguments execute in the let's
scope, also at current depth.

### Named Let

`(let loop ((x init)) body)` compiles as `(letrec ((loop (lambda (x) body)))
(loop init))`. The loop tag's init is a lambda, so when `walkInits=true`
processes it, the lambda body is walked at `closureDepth + 1` naturally. The
outer body `(loop init)` is walked at `closureDepth = 0` — no special-casing
needed. The tag binding is always captured (referenced inside its own lambda
init via recursive calls).

### Shadowing

Nested `ValidatedLet` inside the body may introduce bindings that shadow the
outer let's bindings. `ResolveBindingID` resolves to the innermost binding, so
shadowed outer bindings simply won't be found. The inner let performs its own
capture analysis independently.

## What This Does NOT Do

- The compiler does not use `Captured` yet. This is infrastructure only.
- No codegen changes. Frame elimination, constant propagation, and stack-slot
  bindings are separate follow-on work.
- No B2 escape analysis (tracking whether closures stored in let bindings
  escape). See TODO.md.

## Test Cases

| Code | Expected |
|---|---|
| `(let ((x 1)) x)` | `Captured: false` |
| `(let ((x 1)) (lambda () x))` | `Captured: true` |
| `(let ((x 1)) ((lambda () x)))` | `Captured: false` |
| `(let ((x 1)) ((lambda () (lambda () x))))` | `Captured: true` |
| `(let ((x 1)) (+ x 1))` | `Captured: false` |
| `(let ((x 1) (y 2)) (lambda () x))` | `x: true`, `y: false` |
| `(let ((x 1)) (let ((f (lambda () x))) (f)))` | `Captured: true` |
| `(let loop ((x 1)) (if (= x 0) x (loop (- x 1))))` | `Captured: true` |
| `(let* ((x 1) (y (lambda () x))) y)` | `x: true` |
| `(letrec ((f (lambda () x)) (x 1)) (f))` | `x: true` |
