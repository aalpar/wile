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
(let ((tmp 5) (other 6))
  (swap! tmp other)
  (list tmp other))
```

The user's variable happens to be called `tmp` too. After the swap, `tmp`
should be `6` and `other` should be `5`, so the expression should return
`(6 5)`.

But a naïve macro expander just does textual substitution. It replaces
`(swap! tmp other)` with `(let ((tmp tmp)) (set! tmp other) (set! other tmp))`
and pastes that into the surrounding code. Inside that `let`, the macro's `tmp`
shadows the user's: `(set! tmp other)` writes the macro's temporary instead of
the user's variable, `(set! other tmp)` writes `other`'s own value back, and
nothing is swapped. The expression returns `(5 6)`.

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

Every time a macro is invoked, the expander mints fresh scopes (two of them,
as the walkthrough below shows), each by atomically incrementing a counter
(`nextScopeID`). Pointer equality is used
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
not been through any macro yet. Scopes accumulate as macros and binding forms
(`let`, `lambda`, …) expand.

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

Here is what happens when `(swap! tmp other)` is expanded, starting from the
moment the expander sees it. (The scopes the two `let` forms add are left out
to keep the picture small.)

**Step 1: Detect the macro.**  
The expander checks that no local variable named `swap!` shadows it, then looks
up `swap!` and finds a binding of type `BindingTypeSyntax`. That tells it: this
is a transformer, not a value.

**Step 2: Stamp the input with two fresh scopes.**  
A use-site scope `U` and an intro scope `S1` are created for *this invocation*,
and both are added to every identifier in the whole form:

```go
// pkg/machine/compilation/expander_time_continuation.go
//   (*ExpanderTimeContinuation).expandMacroInvocation
inputForm := p.withUseSiteScope(syntax.NewSyntaxCons(sym, expr, sym.SourceContext()))
introScope := syntax.NewScopeWithLabel("intro")
inputForm = syntax.AddScopeToSyntax(inputForm, introScope)
```

The transformer receives `(swap!{U,S1} tmp{U,S1} other{U,S1})`.

**Step 3: Invoke the transformer and match the pattern.**  
The transformer closure (compiled earlier from the `syntax-rules` form) is
called with that form. The pattern `(swap! x y)` is matched against it by a
bytecode-based pattern-matching VM (see `pkg/internal/match/`). On success, the
matcher has captured `x → tmp{U,S1}` and `y → other{U,S1}`.

**Step 4: Expand the template.**  
The template `(let ((tmp x)) (set! x y) (set! y tmp))` is expanded with the
captures substituted in unchanged. Identifiers the template itself introduces
(`let`, `tmp`, `set!`) keep the scopes they were written with at the macro's
definition, which here is none.

**Step 5: Flip the intro scope.**  
The expander flips `S1` on the result: an identifier that has it loses it, and
one that lacks it gains it. Everything that came from the call site carried
`S1` and loses it; everything the macro introduced gains it. `U` stays on the
call-site identifiers (it is removed only from the name a macro-generated
definition binds, so code after the macro use can see that definition).

After expansion, the syntax tree looks roughly like this:

```
(let{S1} ((tmp{S1} tmp{U}))
  (set!{S1} tmp{U} other{U})
  (set!{S1} other{U} tmp{S1}))
```

**Step 6: Variable resolution.**  
Two bindings named `tmp` are in reach: the user's, with scope set `{}`, and the
macro's, with `{S1}`. For the user's `tmp{U}` in `(set! tmp other)`: is
`{} ⊆ {U}`? Yes. Is `{S1} ⊆ {U}`? No. So it finds the user's binding. For the
macro's `tmp{S1}` on the last line, both `{} ⊆ {S1}` and `{S1} ⊆ {S1}` hold,
and the larger set, the macro's, wins (see below). Hygiene is maintained, and
the result is `(6 5)`. `U` changes none of these answers; it is one more badge
on the user's identifiers that no macro-introduced binding carries.

The resolution check itself is a few lines:

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
keeps the candidate with the *largest* scope set, the most specific one (for
global bindings, a tier ranking comes first and size breaks ties within a tier).
If two candidates tie on size and neither one's scope set contains the other's,
there is no most-specific answer, and the resolver raises
`werr.ErrAmbiguousBinding` rather than picking arbitrarily (`scopedBestOf` in
`pkg/environment/best_of.go` flags the tie for lexical frames, `rankedArgmax` in
`pkg/environment/global_environment_frame.go` for globals;
`EnvironmentFrame.GetBinding` and its siblings raise on it).

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
`{} ⊆ {S1}` holds. But that same subset rule is the exposure: if `my-when` is
exported from a library and used in a program that defines its own top-level
`helper`, that binding also has scope set `{}`, so it satisfies the check just as
well as the library's does, and the program's `helper` would capture the macro's.
(Within a single top level there is only one `helper`: a later
`(define (helper) …)` there redefines the same variable, R7RS §5.3.1, and the
macro sees the new definition.)

So the compiler identifies free identifiers at macro-definition time (everything
in the template that is neither a pattern variable nor a literal) and resolves
each one against the *definition* environment right there
(`collectFreeIdentifiersWithEllipsis` in
`pkg/machine/compilation/compile_syntax_rules.go`). Two things protect them:

- Every template identifier keeps the scope set it was written with, never the
  use site's. That alone covers a binding in the macro's own lexical context: a
  reference written inside `(let ((x …)) …)` carries that `let`'s scope, so after
  expansion it is still a superset of the binder's scope set and still names the
  same variable.
- A global binding found at definition time is attached to the identifier itself
  (`ResolvedBinding` on `SyntaxSymbol`), along with the defining library's scope.
  The compiler consults that pin ahead of the use-site global, so the use site
  cannot hijack the name. (It still sits below the scope-set match against local
  bindings, so a binder the same template introduced can shadow it.)

That pin is what gives cross-library hygiene: a macro defined in library A
that references `car` resolves to the `car` that library A saw, even when the
macro is used in library B, and even if B has its own `car`.

---

## Three Layers, One System

The macro system is split into three layers so each can be simple:

```
┌───────────────────────────────────────────────────────┐
│ Layer 3: Hygiene                                      │
│   Stamps a use-site scope and a fresh intro scope     │
│   on each invocation's input, flips the intro scope   │
│   on the output, and resolves free identifiers        │
│   against the macro's definition environment.         │
│   Files: pkg/machine/compilation/                     │
│            expander_time_continuation.go,             │
│            compile_syntax_rules.go,                   │
│            operation_syntax_rules_transform.go        │
├───────────────────────────────────────────────────────┤
│ Layer 2: Syntax Adapter                               │
│   Scope-aware literal matching, plus the template     │
│   expander that substitutes captures unchanged and    │
│   gives template identifiers their definition-site    │
│   scopes and pins.                                    │
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
pattern variables and never mints or adds a scope. Layer 2 decides which scopes
each template identifier starts from, but adds no intro scope. Layer 3 enforces the
invariant, *macro-introduced names get the intro scope, captured names do not*,
by the flip.

---

## What Would Break Without This

Suppose we removed the intro-scope step entirely. Then:

```scheme
(let ((tmp 5) (other 6))
  (swap! tmp other)
  (list tmp other))
```

The macro's `tmp` would carry no scope that the user's `tmp` lacks, so nothing
would stop the user's `tmp`, passed in as `x`, from satisfying the resolution
rule (`bindingScopes ⊆ useScopes`) against the macro's binder too. That is
variable capture, the original bug; textual substitution shows where it leads:
`(5 6)` instead of `(6 5)`.

Alternatively, suppose we stamped *all* identifiers, including captured pattern
variables, with the intro scope and never flipped it off. Adding a scope to a
reference never stops it matching its own binder (`{} ⊆ {S1}` still holds), so
the user's `other` would still resolve. The damage runs the other way: the
user's `tmp` would arrive as `tmp{S1}`, the macro's binder `tmp{S1}` would now
be a subset of it and the larger candidate, and the user's variable would be
captured again. The correct rule is: captured identifiers must not leave the
expansion carrying the intro scope.

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

(let ((tmp 5) (other 6))
  (swap! tmp other)
  (display (list tmp other)) (newline))   ;; prints (6 5)
```

```bash
./dist/$(go env GOOS)/$(go env GOARCH)/wile hygiene-demo.scm
```

To observe the failure mode, replace the `(swap! tmp other)` line with its
textual substitution, `(let ((tmp tmp)) (set! tmp other) (set! other tmp))`;
the program then prints `(5 6)`.

---

## Further Reading

- [`docs/compiler/macro-system.md`](../compiler/macro-system.md) — technical reference: data structures, bytecode table, file index
- [`BIBLIOGRAPHY.md`](../../BIBLIOGRAPHY.md) — Flatt 2016, "Binding as Sets of Scopes"
- R7RS §4.3 — the language specification for `syntax-rules`
