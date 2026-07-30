# How Hygienic Macros Work in Wile

> This is a pedagogical guide. For the technical reference (data structures,
> file index, bytecode table), see [`docs/compiler/macro-system.md`](../compiler/macro-system.md).

---

## The Problem: Macros That Break Code

Let's start with why macros are tricky. Suppose we want a `swap!` macro that
exchanges two variables:

```scheme
(define-syntax swap!
  (syntax-rules ()
    ((swap! x y)
     (let ((tmp x))
       (set! x y)
       (set! y tmp)))))
```

That looks fine. But now use it like this:

```scheme
(let ((tmp 5) (a 1) (b 2))
  (swap! a b)
  tmp)
```

What should `tmp` be after the swap? The user's `tmp` is `5` and was never
touched. It should still be `5`.

But a naïve macro expander just does textual substitution. It replaces
`(swap! a b)` with `(let ((tmp a)) (set! a b) (set! b tmp))` and pastes
that into the surrounding code. Now there are *two* variables named `tmp` in
the same scope, and the inner one shadows the outer one. The expression
returns `2` (the new value of `b`), not `5`.

This is called **variable capture**. The macro accidentally stole the user's
name. Every Lisp programmer who wrote macros in the 1980s knew about this. The
standard defensive workaround was to use a `gensym` — a globally unique symbol
name like `tmp##47231` — inside the macro. It worked, but it was manual,
error-prone, and made macro writing tedious.

There had to be a better way.

---

## The Key Insight: Names Wear Badges

Here's the idea that solves everything. Instead of making macro-introduced
names *different strings*, we keep the string "tmp" but attach an invisible
**badge** to it — a tag that says *which macro expansion created this name*.

The user's `tmp` has no badge. The macro's `tmp` has badge `{S1}` (where `S1`
is a unique token minted for this particular invocation of `swap!`).

During variable resolution, the rule is: a reference to a name resolves to
a binding only if **the binding's badge set is a subset of the reference's
badge set**.

- The macro's `tmp` was created with badge `{S1}`. The user's reference to
  `tmp` has no badges. Is `{S1} ⊆ {}`? **No.** No match.
- The user's `tmp` was created with no badge. The user's reference to `tmp`
  also has no badges. Is `{} ⊆ {}`? **Yes.** Match.

So each `tmp` finds its own binding. Problem solved.

These badges are called **scopes**. Each name carries a *set* of scopes. The
resolution rule — subset check — is Flatt's "sets of scopes" model (POPL
2016).

---

## What a Scope Actually Is

A scope is just a unique integer wrapped in a struct. There is nothing
structurally complex about it:

```go
// pkg/values/scope.go
type Scope struct {
    id          uint64  // unique identity
    IsRebinding bool    // for let-syntax / letrec-syntax
    Label       string  // optional human-readable tag for debugging
}
```

Every time a macro is invoked, the expander mints a fresh scope by
atomically incrementing a counter (`nextScopeID`). Pointer equality is used
to test scope identity — two scope objects are the same scope if and only if
they are the same pointer.

The scope is not a name, not an environment, not a nesting level. It is purely
an identity token. You can think of it as a sticky note saying "this identifier
came from macro invocation #47."

---

## Where Scopes Live

Every identifier in the parsed program is not a bare string. It is a
`SyntaxSymbol` — a symbol bundled with a *source context* that carries,
among other things, a list of scopes:

```go
// pkg/syntax/syntax_symbol.go
type SyntaxSymbol struct {
    Sym             *values.Symbol
    syntaxBase                      // holds *SourceContext (with Scopes []*Scope)
    ResolvedBinding ResolvedRef     // pre-resolved for cross-library hygiene
}
```

When code is first parsed, every identifier has an empty scope set — it has
not been through any macro yet. Scopes accumulate as macros expand.

Syntax objects are **immutable**. `AddScope` does not modify the existing
object; it returns a new one:

```go
// pkg/syntax/syntax_symbol.go
func (p *SyntaxSymbol) AddScope(scope *Scope) SyntaxValue {
    newCtx := p.SourceContext().WithScope(scope)
    if newCtx == p.SourceContext() {
        return p  // nothing changed — structural sharing
    }
    return &SyntaxSymbol{
        Sym:             p.Sym,
        syntaxBase:      values.NewSyntaxBase(newCtx),
        ResolvedBinding: p.ResolvedBinding,
    }
}
```

This immutability matters: the original syntax tree is never corrupted by
expansion. You can re-expand the same tree safely.

---

## How Expansion Works Step by Step

Here is what happens when `(swap! a b)` is expanded, starting from the moment
the expander sees it:

**Step 1 — Detect the macro.**  
The expander looks up `swap!` in the environment and finds a binding of type
`BindingTypeSyntax`. That tells it: this is a transformer, not a value.

**Step 2 — Invoke the transformer.**  
The transformer closure (compiled earlier from the `syntax-rules` form) is
called with the whole form `(swap! a b)` as its argument.

**Step 3 — Match the pattern.**  
The pattern `(swap! x y)` is matched against `(swap! a b)`. This is done by a
bytecode-based pattern-matching VM (see `pkg/internal/match/`). On success, the
matcher has captured `x → a` and `y → b`.

**Step 4 — Mint the intro scope.**  
A fresh scope `S1` is created for *this invocation*:

```go
// pkg/machine/compilation/operation_syntax_rules_transform.go
//   (*OperationSyntaxRulesTransform).Apply
introScope := syntax.NewScopeWithLabel("intro")
```

**Step 5 — Expand the template.**  
The template `(let ((tmp x)) (set! x y) (set! y tmp))` is expanded with the
captured bindings substituted in. Every identifier introduced *by the macro
itself* (like `tmp`) gets `S1` added to its scope set. Pattern variables that
came *from the call site* (like `x → a` and `y → b`) keep their original
scopes — they must NOT get `S1`, because they belong to the user, not the
macro.

After expansion, the syntax tree looks roughly like this:

```
(let ((tmp{S1} a{}) ...)
  (set! a{} b{})
  (set! b{} tmp{S1}))
```

**Step 6 — Variable resolution.**  
When the compiler later resolves `tmp` in the user's surrounding `let`, it
sees `tmp{}` (no scopes). The binding for the macro's `tmp` has scope set
`{S1}`. Is `{S1} ⊆ {}`? No — they don't match. So the user's `tmp` finds
the user's binding. Hygiene is maintained.

The resolution check itself is five lines:

```go
// pkg/values/scope.go
func ScopesMatch(useScopes, bindingScopes []*Scope) bool {
    if len(bindingScopes) > len(useScopes) {
        return false
    }
    for _, bindScope := range bindingScopes {
        if !slices.Contains(useScopes, bindScope) {
            return false
        }
    }
    return true
}
```

Several bindings of the same name can pass that check at once, so the resolver
keeps the candidate with the *largest* scope set, the most specific one. If two
candidates tie on size and neither one's scope set contains the other's, there is
no most-specific answer, and the resolver raises `werr.ErrAmbiguousBinding` rather
than picking arbitrarily (`scopedBestOf` in `pkg/environment/best_of.go` flags the
tie; `EnvironmentFrame.GetBinding` and its siblings raise on it).

---

## The Subtle Part: Free Identifiers

Not every identifier in a macro template is pattern-variable output. Some are
references that the *macro itself* needs — like `let`, `set!`, or a helper
function the macro calls:

```scheme
(define (helper) 'skipped)

(define-syntax my-when
  (syntax-rules ()
    ((my-when condition body ...)
     (if condition (begin body ...) (helper)))))
```

Here `if`, `begin`, and `helper` are **free identifiers** — they are not pattern
variables, and they are not introduced by the macro to be bound. They are
references to existing definitions.

The intro scope alone does not protect them. Adding `S1` to a reference is
harmless for a top-level binding, because that binding's scope set is `{}` and
`{} ⊆ {S1}` holds. But that same subset rule is the exposure: a top-level binding
of the same name at the *use* site also has scope set `{}`, so it satisfies the
check just as well as the definition-site one does. A user who defines their own
top-level `helper` would capture the macro's.

So the compiler identifies free identifiers at macro-definition time (everything
in the template that is neither a pattern variable nor a literal) and resolves
each one against the *definition* environment right there
(`collectFreeIdentifiersWithEllipsis` in
`pkg/machine/compilation/compile_syntax_rules.go`). What happens next depends on
what it found:

- A binding in the macro's own lexical context: the template identifier is
  rebuilt carrying that binder's scope set, and no intro scope, so it still names
  the same variable after expansion.
- A global binding: the identifier keeps the intro scope and additionally carries
  the resolved binding itself (`ResolvedBinding` on `SyntaxSymbol`) plus the
  defining library's scope. The compiler consults that pin ahead of the use-site
  global, so the use site cannot hijack the name. (It still sits below the
  scope-set match against local bindings, so a binder the same template
  introduced can shadow it.)

That pin is also what gives cross-library hygiene: a macro defined in library A
that references `car` resolves to the `car` that library A saw, even when the
macro is used in library B, and even if B has its own `car`.

---

## Three Layers, One System

The macro system is split into three layers so each can be simple:

```
┌───────────────────────────────────────────────────────┐
│ Layer 3: Hygiene                                      │
│   Mints the intro scope for each invocation and       │
│   resolves free identifiers against the macro's       │
│   definition environment.                             │
│   Files: pkg/machine/compilation/                     │
│            compile_syntax_rules.go,                   │
│            operation_syntax_rules_transform.go        │
├───────────────────────────────────────────────────────┤
│ Layer 2: Syntax Adapter                               │
│   Scope-aware literal matching, plus the template     │
│   expander that stamps the intro scope and keeps      │
│   captured pattern variables' original syntax.        │
│   Files: pkg/internal/match/syntax_adapter.go,        │
│          pkg/internal/match/syntax_expand.go          │
├───────────────────────────────────────────────────────┤
│ Layer 1: Pattern Matching VM                          │
│   Bytecode pattern compiler and matcher. Operates     │
│   on syntax values but knows nothing about hygiene.   │
│   Files: pkg/internal/match/syntax_compiler.go,       │
│          pkg/internal/match/match.go                  │
└───────────────────────────────────────────────────────┘
```

Layer 1 can be tested and debugged without thinking about scopes: it captures
pattern variables and never mints or adds a scope. Layer 2 is where scopes enter
the expanded output. Layer 3 is where the invariant — *macro-introduced names get the
intro scope, captured names keep their original scopes* — is enforced.

---

## What Would Break Without This

Suppose we removed the intro-scope step entirely. Every identifier in the
macro expansion would have empty scope sets. Then:

```scheme
(let ((tmp 5) (a 1) (b 2))
  (swap! a b)
  tmp)
```

Would expand to a context with two `tmp` bindings both carrying `{}`. The
resolution rule (`bindingScopes ⊆ useScopes`) would match *both* — and the
inner one (from the macro) would shadow the outer one (from the user). Result:
`2` instead of `5`. Variable capture, the original bug.

Alternatively, suppose we stamped *all* identifiers — including captured
pattern variables — with the intro scope. Then `a{}` would become `a{S1}`,
and the user's binding `a{}` (with scope set `{}`) would fail to match
(`{} ⊆ {S1}` is true, but the reference has `{S1}` and the binding has
`{}` — wait, that would actually match since `{} ⊆ {S1}`). Let me be more
precise: the binding for `a` at the call site has scopes `{}`. If the
reference inside the macro gets `{S1}`, then `ScopesMatch({S1}, {})` checks
`{} ⊆ {S1}` — which is **true** — so it would still work for simple cases.
But it would break cases where the user's binding has a non-empty scope set of
its own (e.g., `a` defined inside another macro). The correct rule is:
captured identifiers must keep *exactly* the scope set they arrived with,
unmodified.

---

## Seeing It Live

If you have Wile built (`make build`), you can observe hygiene directly:

```scheme
;; hygiene-demo.scm
(define-syntax swap!
  (syntax-rules ()
    ((swap! x y)
     (let ((tmp x))
       (set! x y)
       (set! y tmp)))))

(let ((tmp 5) (a 1) (b 2))
  (swap! a b)
  (display tmp) (newline))   ;; should print 5
```

```bash
./dist/$(go env GOOS)/$(go env GOARCH)/wile hygiene-demo.scm
```

Change `swap!`'s `tmp` to be captured-from-pattern (break the macro), or
remove the `syntax-rules` wrapping to observe the failure modes.

---

## Further Reading

- [`docs/compiler/macro-system.md`](../compiler/macro-system.md) — technical reference: data structures, bytecode table, file index
- [`BIBLIOGRAPHY.md`](../../BIBLIOGRAPHY.md) — Flatt 2016, "Binding as Sets of Scopes"
- R7RS §4.3 — the language specification for `syntax-rules`
