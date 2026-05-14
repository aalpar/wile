# `internal/` package structural reduction

**Date**: 2026-05-07
**Source**: `/structural-reduction ./internal` analysis
**Status**: ✅ **All 7 findings shipped** (2026-05-12). Findings 7, 4, 3, 2, 6
batched in PR #739 (`feat/internal-sr-phases-1-5`); Finding 5 in PR #740
(`feat/internal-sr-finding5`); Finding 1 in PR #741 (`feat/internal-sr-finding1`).
See impl plans `plans/2026-05-12-internal-sr-phases-1-5-impl.md`,
`plans/2026-05-12-internal-sr-finding5-impl.md`,
`plans/2026-05-12-internal-sr-finding1-impl.md`.
**Priority**: Medium-High (Tier 5 tech debt) — completed.

## Scope analyzed

`internal/` (11 sub-packages, ~14K LOC production Go excluding tests):
- `internal/syntax/` (17 files, 2406 LOC) — foundational `SyntaxValue` type
- `internal/validate/` (21 files, 3019 LOC) — special-form validation
- `internal/match/` (10 files, 2669 LOC) — pattern-match VM for `syntax-rules`
- `internal/parser/` (6 files, 2048 LOC) — token → syntax
- `internal/tokenizer/` (9 files, 2388 LOC) — rune → token
- `internal/forms/` (2 files, 148 LOC) — interface mediator
- `internal/schemeutil/` (3 files, 273 LOC) — small string/predicate helpers
- `internal/bootstrap/` (2 files, 378 LOC) — extension/profile wiring
- `internal/extensions/{io,envvars,namespace,iotest,all}/` (5 sub-packages)
- `internal/testutil/` (2 files, 87 LOC)

Companion `machine/` and `machine/compilation/` were treated as consumers; not
re-analyzed here (see `plans/2026-05-06-machine-structural-reduction.md`).

## Dependency map

```
                       ┌────────────────────────┐
                       │ syntax  Ca=14  Ce=3     │   I=0.18  (stable foundation)
                       │ "the SyntaxValue type"  │
                       └─────────┬───────┬───────┘
                                 ▲       ▲
              ┌──────────────────┘       └──────────────────┐
              │                                              │
   ┌──────────┴────────┐  ┌──────────────┐  ┌────────────────┴─────┐
   │ tokenizer Ca=2    │  │ schemeutil   │  │ forms     Ca=2 Ce=5  │
   │ Ce=3   I=0.60     │  │ Ca=4 Ce=3    │  │ I=0.71               │
   └──────────┬────────┘  └──────┬───────┘  └────────┬─────────────┘
              ▲                  ▲                    ▲
              │                  │                    │
              ├──────────────────┤                    │
              │                  │                    │
   ┌──────────┴────────┐         │           ┌────────┴─────────────┐
   │ parser    Ca=7    │─────────┘           │ validate  Ca=1 Ce=8  │
   │ Ce=8   I=0.53     │                     │ I=0.89               │
   └──────────┬────────┘                     └────────┬─────────────┘
              ▲                                       ▲
              │                                       │
              │           machine/compilation ────────┤
              │                                       │
              ▲                                       ▲
   ┌──────────┴────────┐                     ┌────────┴─────────────┐
   │ match     Ca=1    │                     │ extensions/io        │
   │ Ce=4   I=0.80     │                     │ extensions/envvars   │
   │ (compile-only)    │                     │ extensions/namespace │
   └───────────────────┘                     │ extensions/iotest    │
                                              │ extensions/all (Ce=15│
                                              │ aggregator, expected)│
                                              └──────────────────────┘
```

**Observations on the graph**

- **No cycles.** The graph is a clean DAG. `syntax` is the foundational stable
  type with 14 dependents; everything flows downstream from it.
- The `validate → forms ← machine/compilation` shape (where `forms` is a
  neutral mediator with `Ce=5`) is the **Dependency Inversion Principle** in
  concrete form: an interface (`forms.ValidatedExpr`) in a stable package
  mediates between two unstable peers, breaking what would otherwise be a
  cycle. Worth preserving as positive prior art.
- `internal/validate` (I=0.89, Ca=1) and `internal/match` (I=0.80, Ca=1) are
  highly unstable but each have **a single consumer** (`machine/compilation`).
  The Stable Dependencies Principle does not flag this: high `I` is a problem
  only when paired with high `Ca`. These are private satellites of compilation
  and should remain so.
- `internal/extensions/all` (Ce=15) is an aggregator with high `Ce` by design —
  expected and not a violation.

## Findings

### Finding 1 — `SyntaxPair` admits two representations of the empty list

**Principle**: State Tightness
**Where**: `internal/syntax/syntax_pair.go:218-223` (`IsEmptyList`),
`:33` (`SyntaxEmptyList` singleton); 7 production guard sites at `:50-67`,
`:130-134`, `:156-158`, `:289-291`, `:325-327`, `:348-350`, `:218-223`
**Theory**: Type precision (Minsky, "Effective ML"; Pierce, *TAPL* §11). A
type's precision is `|valid states| / |representable states|`. Multiple
representations of the same semantic value reduce precision; every consumer
must defensively check for both forms. By type algebra, the **canonical empty
list** should be a sum-type sibling, not a sub-state of the pair product.

**Current state**:
- `SyntaxEmptyList SyntaxTuple = &syntaxEmptyListType{}` is the canonical
  singleton (`syntax_pair.go:33`).
- `*SyntaxPair` *also* represents the empty list when
  `Values[0] == nil && Values[1] == nil` (`syntax_pair.go:218-223`).
- `IsEmptyList()` is checked defensively in 7+ production sites:
  `AddScope`, `Unwrap`, `Append`, `SchemeString`, `AsVector`, `AsSyntaxVector`,
  `IsList`, plus indirect via `UnwrapAllShared` (`syntax_value.go:212`).
- `grep -rEn "NewSyntaxCons\(\s*nil\s*,\s*nil"` confirms the empty-via-nil-nil
  representation is constructed *only in tests* (`syntax_pair_test.go:33,85`,
  `coverage_test.go:549`) — not in production. The empty-via-nil-nil state is
  dead-on-write but live-on-read.
- Comment at `syntax_pair_test.go:148` reads
  `"*Pair.IsEmptyList() always returns false now that EmptyList is a separate type"`
  — confirming a parallel migration in `values/` already completed; `syntax/`
  is mid-migration.

**Problem**: Semantic state space = `{void, empty-list, proper-pair}` = 3
states. Representable space for `*SyntaxPair`:
- `nil` pointer (void, via `IsVoid`)
- `Values = (nil, nil)` (empty list)
- `Values = (X, nil)` (malformed)
- `Values = (nil, X)` (malformed)
- `Values = (X, Y)` (proper pair)

That's `3/5 = 60%` precision. Two malformed states are unguarded. The
empty-via-nil-nil state is redundant with `SyntaxEmptyList` and forces every
consumer to know about it.

**Proposed direction**: Finish the migration that `values/` already completed.
1. Audit `NewSyntaxCons` callers; replace any that produce `(nil, nil)` with
   `SyntaxEmptyList`.
2. Make `NewSyntaxCons` reject `(nil, nil)` (panic via `werr.WrapForeignErrorf`)
   or coerce to `SyntaxEmptyList` at construction time.
3. Hardcode `*SyntaxPair.IsEmptyList()` to `return false` (matching the
   `values.Pair` migration).
4. Delete the 7 defensive guards in `syntax_pair.go`.
5. Update `UnwrapAllShared` to switch only on `IsVoid`/`IsEmptyList(via tuple)`,
   not the embedded check.

**Impact**: 7 callers shed redundant guards; type precision rises from 60% →
100% for the proper-pair case; `*SyntaxPair` becomes a proper sum-type sibling
of `SyntaxEmptyList` instead of being conflated with it. Future readers stop
having to learn the dual representation.
**Estimated size**: M (touches 7 production sites, several tests; mechanical
once `NewSyntaxCons` validates).

### Finding 2 — Three hand-unrolled "parse `((name init) ...)`" loops in `validate/`

**Principle**: Composability
**Where**:
- `internal/validate/validate_let.go:184-212` (let* parse loop)
- `internal/validate/validate_let.go:386-420` (letrec parse loop)
- `internal/validate/validate_let.go:622-654`
  (`validateLetBindingPairs` — couples parse with init validation)
**Theory**: "Same verbs, not same nouns" (CLAUDE.md "Refactoring"). When N
call sites perform the same sequence of operations differing only in what they
do *after* parsing, the parsing is the **irreducible core operation**. The
algebraic move is **factoring**: extract the common subexpression. The three
sites need different env-construction phases (let validates inits in outer
env, let* validates incrementally with growing env, letrec validates
all-at-once after binding all names) — but they all need the same
`[(name *SyntaxSymbol, initStx SyntaxValue)]` slice as input.

**Current state**: Each function inlines the same shape:
```go
for _, bindingExpr := range bindingsListRaw {
    bPair, ok := bindingExpr.(*syntax.SyntaxPair)        // type-assert
    if !ok || syntax.IsSyntaxEmptyList(bPair) { ... }
    elems, imp := collectList(bPair)                      // collect
    if imp || len(elems) != 2 { ... }
    nameSym, symOk := asSyntaxSymbol(elems[0])            // extract symbol
    if !symOk { ... }
    // diverges here:
    //   - let*:    raw = append(raw, letStarRawBinding{name, initStx})
    //   - letrec:  nameSyms = append(...); initExprs = append(...)
    //   - let:     also calls validateExpr on elems[1]
}
```
The `validateLetBindingPairs` version additionally couples in
`validateExpr(ctx, env, elems[1], result)` — but that coupling is exactly what
prevents reuse for letrec, where the env doesn't exist yet.

**Problem**: ~80 lines of duplicated parse logic across three functions. A bug
fix in the parsing (e.g., better error message for non-pair bindings) requires
editing three places — and one will be missed. CLAUDE.md "Refactoring"
explicitly warns: *"When fixing compile errors during refactoring, check if
the same pattern applies elsewhere. Inconsistency = incomplete thinking."*

**Proposed direction**:
```go
type rawLetBinding struct {
    name *syntax.SyntaxSymbol
    init syntax.SyntaxValue
}

// parseLetBindingPairs parses ((name init) ...) into [(name, initStx)].
// Does NOT validate init — caller validates in the appropriate env.
func parseLetBindingPairs(
    bindingsPair *syntax.SyntaxPair,
    formName string,
    result *ValidationResult,
) ([]rawLetBinding, bool)
```

Then:
- `validateLetBindingPairs` becomes parse-then-validate (calls helper, then
  loops `init = validateExpr(ctx, env, raw.init, result)`).
- `validateLetStarBindingsAndBody` calls the helper, supplies its own
  evolving-env init validation.
- `validateLetrecBindingsAndBody` calls the helper, builds env from raw
  names, validates inits in that env.

**Impact**: ~80 LOC removed; one source of truth for the binding-form syntax;
easier to add features (e.g., R7RS `define-values`-style binding patterns) by
editing one parser. The structural difference between the four binding forms
becomes *only* the post-parse env strategy — which is what the existing
`LetKind` enum was designed to encode.
**Estimated size**: S.

### Finding 3 — `bindingIdentity` duplicate-detection hand-unrolled at 5 sites

**Principle**: Composability (monoid identification)
**Where**:
- `internal/validate/validate_define.go:131-184` (rest param + required param dup check, interleaved)
- `internal/validate/validate_let.go:222-234` (let* dup check)
- `internal/validate/validate_let.go:430-447` (letrec dup check)
- `internal/validate/validate_let.go:596-611` (`checkDuplicateBindingNames` — already factored, but only for `[]ValidatedLetBinding`)

**Theory**: Set union with disjoint-check is a **partial monoid**. The
associated check — "given a sequence of identifiers, find duplicates by
name+scope-set" — is a fold over `[]*SyntaxSymbol` with
`Set BindingIdentity` as accumulator. Five separate inlinings is a missed
opportunity to express it as one pure operation. Algebraically: the closure
operation is `S ∪ {id(x)}` if `id(x) ∉ S`, else fail. Once factored, every
caller is a single function call.

**Current state**: Each call site has the shape:
```go
seen := make(map[bindingIdentity]bool, len(xs))
for _, x := range xs {
    id := bindingIdentity{
        key:      x.Sym.Key,
        scopeKey: scopeFingerprint(x.Scopes()),
    }
    if seen[id] { /* error */ }
    seen[id] = true
}
```
`checkDuplicateBindingNames` (`validate_let.go:588-612`) factors this for
`[]ValidatedLetBinding`, but the let* and letrec validators run on raw
`*SyntaxSymbol` slices *before* validation, so they reinline.
`validate_define.go` does it twice (once for required params, once for rest
param) interleaved with parsing.

**Problem**: 5 inlinings of the same fold; `bindingIdentity` and
`scopeFingerprint` are factored out but the loop is not. By the **pigeonhole
principle**: 5 places to fix means 5 places to forget to fix when changing
behavior (e.g., adding source location to the error, or sorting duplicates).

**Proposed direction**:
```go
// detectDuplicateSymbols returns the duplicates in order of second appearance.
// Equality is by (name, scope-fingerprint) tuple. Empty result = no duplicates.
func detectDuplicateSymbols(syms []*syntax.SyntaxSymbol) []*syntax.SyntaxSymbol
```
Each call site decides how to report. The existing
`checkDuplicateBindingNames` becomes a thin wrapper that maps
`[]ValidatedLetBinding` → `[]*SyntaxSymbol` and delegates.

**Impact**: 5 sites collapse to 1 helper. Duplicate detection becomes
self-evidently correct (one place to test). `validate_define.go` parameter
validation cleanly separates parse from dup-detect.
**Estimated size**: S.

### Finding 4 — Two near-identical "create child env with N bindings" sites

**Principle**: Composability
**Where**:
- `internal/validate/validate_lambda.go:56-87` (`createLambdaValidationEnv`)
- `internal/validate/validate_let.go:672-690` (`createLetValidationEnv`)
- Plus inlined versions at `validate_let.go:316-322` (let* nested),
  `:380-419` (letrec), `:514-521` (named let tag binding) — 5 sites total.

**Theory**: Same algebraic operation `(env, [Symbol]) → env'`. Forms a
**partial monoid** under env-extension (associative if you compose
left-to-right; identity = empty bindings). The two abstractions exist because
the input shape differs: lambda's `*ValidatedParams` (Required + Rest) vs
let's `[]ValidatedLetBinding`. The output is identical: a child frame with N
variable bindings, each created via `MaybeCreateLocalBinding` with the same
four arguments.

**Current state**:
```go
// lambda
lenv := environment.NewLocalEnvironment(0)
childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)
for _, paramSym := range params.Required {
    childEnv.MaybeCreateLocalBinding(paramSym.Sym, BindingTypeVariable,
                                     paramSym.Scopes(), paramSym.SourceContext())
}
if params.Rest != nil { childEnv.MaybeCreateLocalBinding(...) }

// let
lenv := environment.NewLocalEnvironment(0)
childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)
for _, b := range bindings {
    childEnv.MaybeCreateLocalBinding(b.Name.Sym, BindingTypeVariable,
                                     b.Name.Scopes(), b.Name.SourceContext())
}
```

**Problem**: 5 sites; the inlined versions in let* and letrec exist because
they need to interleave with init-validation in evolving env frames. Even
those sites repeat the same four-argument
`MaybeCreateLocalBinding(sym.Sym, BindingTypeVariable, sym.Scopes(), sym.SourceContext())`
call.

**Proposed direction**:
```go
// bindLocalSymbol calls MaybeCreateLocalBinding with the canonical four-arg
// pattern for variable bindings.
func bindLocalSymbol(env *environment.EnvironmentFrame, sym *syntax.SyntaxSymbol)

// extendEnvWithSymbols creates a child frame and binds all symbols as
// variables. Returns env unchanged if syms is empty.
func extendEnvWithSymbols(env *environment.EnvironmentFrame, syms []*syntax.SyntaxSymbol) *environment.EnvironmentFrame
```
`*ValidatedParams` gains a `Symbols() []*SyntaxSymbol` method (or
`AppendTo([]*SyntaxSymbol)`) so lambda can use the batch helper. The 3
inlined versions in let* / letrec / named-let call `bindLocalSymbol` per
iteration as their env evolves.

**Impact**: 5 sites collapse to 1 primitive + 1 batch helper. Tests become
simpler (one helper to verify, not 5). Future "what does it mean to create a
binding?" changes (e.g., adding a binding-creation hook, recording locations,
adding a fast path) get done in one place.
**Estimated size**: S.

### Finding 5 — `captureWalker` and `escapeWalker` share boilerplate

**Principle**: Composability
**Where**:
- `internal/validate/validate_capture.go:33-69` setup
- `internal/validate/validate_escape.go:34-70` setup
- `internal/validate/validate_let.go:714-728` (`markMutableBindings`, simpler but related)

**Theory**: The setup phase (build `idToIdx`, optionally walk inits, then
walk body) is structurally identical; only the per-symbol predicate differs.
This is a textbook **strategy pattern**: shared traversal + pluggable
predicate. Algebraically, both walkers are **catamorphisms** (folds over the
algebraic data type `ValidatedExpr`) parameterized by the leaf-handler.
`WalkSubExprs` (`validate/walk_sub_exprs.go:47`) is a *one-level* unfolder —
the right primitive but missing its higher-arity sibling.

**Current state**: Both walkers duplicate ~30 lines of "build idToIdx,
return-if-empty, optionally walk inits, walk body."
`markCapturedBindings` walks with `depth` tracking;
`markEscapedBindings` walks tracking call-position. The per-symbol logic
differs:
```go
// capture: any reference inside a closure (depth > 0) marks Captured
// escape:  any reference NOT in call-proc position marks Escapes
```

**Problem**: Adding a third analysis (e.g., "find references that cross a
continuation boundary", "find references that survive into a returned
lambda") requires copying the same setup code. The setup is incidental
scaffolding; the real content is the per-symbol predicate. Comment at
`validate_escape.go:24-27` documents that
*"The three fields (Mutable, Captured, Escapes) form an implicational base"*
— suggesting more analyses are anticipated.

**Proposed direction**: A higher-order traversal:
```go
type RefRole int
const (
    RefInBody         RefRole = iota  // direct body of let/begin/if-arm
    RefInCallProc                     // operator position of call/apply
    RefInClosureBody                  // body of lambda or case-lambda clause
    RefSetBangTarget                  // target of set! (mutation, not read)
)

// WalkBindingRefs walks expr, calling visit for every ValidatedSymbol
// reference. The depth parameter counts closure boundaries crossed (0 = same
// closure; 1 = inside one lambda; etc.). Set! targets are visited with role
// RefSetBangTarget rather than as plain symbol references.
func WalkBindingRefs(
    expr ValidatedExpr,
    visit func(sym *syntax.SyntaxSymbol, role RefRole, depth int),
)
```
Both `markCaptured` and `markEscaped` become 6-line functions that filter on
role + depth. `markMutable` already uses a different mechanism
(`ValidationResult.isMutated`) and stays as-is.

**Impact**: 1 traversal helper, 2-3 callers each ~10 LOC. Future analyses
(unused bindings, free-variable extraction for closure conversion, purity
analysis for tail-call optimization heuristics) get the traversal for free.
**Estimated size**: S–M (depends on how many existing tests depend on the
specific walker shapes).

### Finding 6 — Telescoping constructors in `match`

**Principle**: Composability / API surface area
**Where**: `internal/match/match.go:78-94`
**Theory**: Telescoping constructor antipattern (Bloch, *Effective Java*,
Item 2). When constructor N has parameters `(a, b, c)` and constructor N+1
has `(a, b, c, d)`, the chain forms a partial order on argument tuples. Go
has no default arguments, so the chain is the language's substitute — but
each link adds maintenance load. In type-algebra terms, the constructors
encode a *path* through the product space `(variables × codes × ellipsisVars
× ellipsisDepths × ellipsisID)`, not its structure.

**Current state**:
```go
NewMatcher(variables, codes)
NewMatcherWithEllipsisVars(variables, codes, ellipsisVars)
NewMatcherFull(variables, codes, ellipsisVars, ellipsisID)
NewMatcherFullWithDepths(variables, codes, ellipsisVars, ellipsisDepths, ellipsisID)
```
Each delegates to the next, supplying the missing parameter as a default
(`nil`, `nil`, `DefaultEllipsis`). 4 entry points, 1 used path under each.

**Problem**: Adding a 5th option requires either renaming the deepest one or
adding `NewMatcherFullWithDepthsAndX` — nameflation. Callers must guess which
constructor matches their need. The set of valid constructions is
`(variables, codes, ellipsisVars?, ellipsisDepths?, ellipsisID?)` — a
partially-optional product. The constructor chain encodes one *traversal* of
the product space, not the space itself.

**Proposed direction**: Wile already uses option-functions for `Engine`
(`WithProfile`, `WithSandbox`, `WithSourceFS`, etc.). Apply the same idiom:
```go
type MatcherOption func(*Matcher)

func WithEllipsisVars(v map[int]map[string]struct{}) MatcherOption
func WithEllipsisDepths(d map[int]int) MatcherOption
func WithEllipsisID(id string) MatcherOption

func NewMatcher(variables map[string]struct{}, codes []SyntaxCommand, opts ...MatcherOption) *Matcher
```
4 entry points → 1 + N options. Adding a new tunable becomes one new `WithFoo`
function instead of a new constructor variant. Discoverability improves
because `gopls`/IDE tooling lists all `With*` options together.

**Impact**: API surface drops from 4 constructors to 1 + 4 small options.
Discoverability up. `match` aligns with the rest of the codebase's option
pattern. Existing callers update mechanically. The `MatcherConfig` struct
alternative is also viable; the option-function shape matches the existing
Wile idiom and is preferred.
**Estimated size**: S.

### Finding 7 — `SyntaxObject.IsPair()` is dead by construction

**Principle**: State Tightness (dead branch elimination)
**Where**: `internal/syntax/syntax_value.go:122-136` (constructor),
`:160-164` (`IsPair`), `:166-169` (`IsEmptyList`)
**Theory**: A type's predicates should reflect achievable states. If
`NewSyntaxObject` panics on `*values.Pair`, then `IsPair()` cannot return
true for any legitimately-constructed value. The dead branch is a
**phantom** — it exists in the type but has no code path that handles it
non-trivially. By **closure under construction**, every reachable
`*SyntaxObject` value has a non-pair, non-vector, non-symbol `datum` —
so `IsPair()` is structurally `return false`.

**Current state**:
```go
func NewSyntaxObject(v values.Value, sctx *SourceContext) *SyntaxObject {
    switch v.(type) {
    case *SyntaxObject, *SyntaxVector, *SyntaxPair, *SyntaxSymbol: panic(...)
    case *values.Vector, *values.Pair, *values.Symbol:             panic(...)
    }
    ...
}

func (p *SyntaxObject) IsPair() bool {
    _, ok := p.Datum().(*values.Pair)
    return ok
}
```
A grep for production callers of `SyntaxObject.IsPair` returns zero hits.

**Problem**: A consumer reading the code sees `IsPair()` and assumes it might
return true. The method exists but is unreachable in practice. Cognitive
overhead with no payoff.

**Proposed direction**: Two options.
- (a) Delete `SyntaxObject.IsPair()` and `SyntaxObject.IsEmptyList()`.
  Callers should switch on the syntax-level type (`*SyntaxPair`,
  `*SyntaxObject`, etc.), which is the correct dispatch site anyway.
- (b) Keep them but make them compile-time `return false` and document them
  as required by the `values.Value` interface (if the interface requires
  these, which should be confirmed). Add a doc comment:
  `// always false; IsPair on SyntaxObject is unreachable by construction.`

Preference: (a) unless the `values.Value` interface mandates otherwise.

**Impact**: Dead code removed. The remaining callers (if any reappear) get
pushed up to `SyntaxValue`-level type-switching, which is the correct
dispatch site.
**Estimated size**: XS.

## Opportunities (sort-package style)

### Opportunity: `WalkSubExprs`-driven analysis pipeline

- **Replaces**: `markCapturedBindings`, `markEscapedBindings`, and any
  future binding-reference analysis.
- **Core operation**: traverse a `ValidatedExpr` tree, classify each
  `ValidatedSymbol` reference by structural role (closure-body / call-proc /
  normal / set!-target), and let a caller-supplied predicate decide what to
  do at each leaf.
- **Algebraic structure**: This is a **catamorphism** (fold over the
  algebraic data type `ValidatedExpr`). The existing `WalkSubExprs` is a
  *one-level* unfolder — the right primitive, but the multi-level traversal
  pattern is missing. With the role parameter, it's a fold parameterized by
  an effect (the caller's mutation).
- **Proposed shape**:
  ```go
  type RefRole int
  const (
      RefInBody RefRole = iota
      RefInCallProc
      RefInClosureBody    // walked through one or more closure boundaries
      RefSetBangTarget
  )

  func WalkBindingRefs(
      expr ValidatedExpr,
      visit func(sym *syntax.SyntaxSymbol, role RefRole, depth int),
  )
  ```
- **Reuse sites**: `markCaptured`, `markEscaped`, future "free-variable
  extraction" for closure conversion, future "binding usage stats" for
  unused-variable warnings, future "purity analysis" for tail-call
  optimization heuristics. ≥5 potential consumers, currently 0 + 2
  hand-unrolled.

### Opportunity: `parseLetBindingPairs` + `extendEnvWithSymbols` as the let-validation kernel

- **Replaces**: 3 inlined parse loops (Finding 2) + 5 env-construction sites
  (Finding 4) + 5 dup-detection sites (Finding 3) — though dup-detection has
  its own helper (Opportunity below).
- **Core operation**: Two pure transformations:
  1. `*SyntaxPair → []rawLetBinding` (parse, no validation)
  2. `(*EnvironmentFrame, []*SyntaxSymbol) → *EnvironmentFrame` (extend, no
     resolution)
- **Algebraic structure**: (1) is a parser-combinator fragment. (2) is a
  partial monoid (env-extension is associative under disjoint binding sets;
  the empty list is the identity).
- **Proposed shape**:
  ```go
  type rawLetBinding struct {
      name *syntax.SyntaxSymbol
      init syntax.SyntaxValue
  }
  func parseLetBindingPairs(*syntax.SyntaxPair, formName string, *ValidationResult) ([]rawLetBinding, bool)
  func extendEnvWithSymbols(*environment.EnvironmentFrame, []*syntax.SyntaxSymbol) *environment.EnvironmentFrame
  func bindLocalSymbol(*environment.EnvironmentFrame, *syntax.SyntaxSymbol)
  ```
- **Reuse sites**: `validateLetBindingsAndBody` (let), `validateLetStarFlat`,
  `validateLetStarNested`, `validateLetrecBindingsAndBody`,
  `validateNamedLet`, `createLambdaValidationEnv`, `createLetValidationEnv`.
  Seven sites collapse onto two helpers.

### Opportunity: `detectDuplicateSymbols` fold

- **Replaces**: 5 inlined `seen[bindingIdentity]` loops (Finding 3).
- **Core operation**: Given `[]*SyntaxSymbol`, return the subset that is the
  duplicates (second-and-later appearances).
- **Algebraic structure**: A fold over `[]*SyntaxSymbol` with `Set
  BindingIdentity` as accumulator. Set union with disjoint-check is a partial
  monoid.
- **Proposed shape**:
  ```go
  // detectDuplicateSymbols returns the duplicates in order of second
  // appearance. Equality is by (name, scope-fingerprint) tuple.
  // Empty result = no duplicates.
  func detectDuplicateSymbols(syms []*syntax.SyntaxSymbol) []*syntax.SyntaxSymbol
  ```
- **Reuse sites**: param dup-check (define), let* dup-check, letrec
  dup-check, named-let, and `checkDuplicateBindingNames` (which becomes a
  thin wrapper that maps `[]ValidatedLetBinding` → `[]*SyntaxSymbol`).

### Opportunity: Option-function constructor for `match.Matcher`

- **Replaces**: 4 telescoping constructors (Finding 6).
- **Core operation**: Construct a `Matcher` with required + optional fields.
- **Algebraic structure**: An optional-product of fields, expressed as a
  closure-pipeline rather than a position-tuple chain.
- **Proposed shape**: see Finding 6.
- **Reuse sites**: Aligns `match` with `wile.Engine`'s existing option
  pattern (`WithProfile`, `WithSandbox`, `WithSourceFS`, etc.); the same
  idiom can apply to any future fluent constructor in the package.

## What's already done well (preserve)

Several pieces of architecture are textbook good and should be preserved or
imitated when addressing the findings:

1. **`LetKind` 2×2 enum** (`validate/validated_forms.go:268-302`). The four
   binding forms (`let`, `let*`, `letrec`, `letrec*`) collapse to a 4-state
   enum encoding two orthogonal boolean dimensions
   (`InitsInScope() × Sequential()`). 100% type precision; the comment table
   *(`| Kind | Inits see bindings? | Eval order |`)* documents the algebra
   directly. Imitate this pattern when collapsing future enum-vs-flag-pair
   designs.

2. **`forms` package as DIP mediator** (`internal/forms/`, 148 LOC). Breaks
   the would-be cycle between `validate` and `machine/compilation` by
   providing a stable `ValidatedExpr` interface that both sides depend on.
   `Ce=5`, `Ca=2`, neutral. Proper application of the **Dependency
   Inversion Principle**.

3. **`WalkSubExprs` + `ChildRole`** (`validate/walk_sub_exprs.go`). Exactly
   the right level of abstraction for one-level traversal of validated
   forms. The opportunity (Finding 5) is that *callers* of this primitive
   duplicate the per-binding bookkeeping; the primitive itself is the right
   shape.

4. **`SyntaxEmptyList` singleton + `SyntaxValue` interface hierarchy**
   (`syntax/syntax_pair.go:33`, `syntax/syntax_tuple.go`). A proper sum-type
   sibling for the empty list (parallel to `values.EmptyList`). Type-level
   enforcement of `(pair? '()) → #f`. Finding 1 finishes the migration that
   already established this pattern.

5. **`ci`-table-driven primitive registration**
   (`internal/extensions/all/register.go:163-176`,
   `:181-192`). `stringCiCompareSpecs` and `charCiCompareSpecs` drive
   `r.AddPrimitives` via a loop instead of inlining N copies of the spec.
   Imitate this pattern when registering families of related primitives.

## Closing summary

**State-space**: Of the high-traffic types examined,
- `LetKind` is a model citizen: 4 enum values for a 2×2 product, 100%
  precision, no defensive checks.
- `*SyntaxPair` is the worst offender: representable space ~5, valid space
  ~3, **~60% precision**. Migrating to "non-empty pair only" (Finding 1)
  lifts to 100%.
- `SyntaxObject.IsPair()` is a *zero-width* method — representable but
  unreachable. Remove (Finding 7).
- The 13 `Validated*` types each pair `validatedBase` with a tight per-form
  payload; the embedding eliminates ~36 boilerplate methods. Good prior
  art — the let composition issues are not in the *types* but in the
  *construction code*.
- `TokenizerState` is a 75-state flat enum that semantically encodes a
  sparse subset of `(sign × kind × base) ≈ 2×10×5 = 100`. The flat
  encoding is the right call: not every combination is valid, and a tuple
  representation would require runtime validation. Acceptable as-is.

**Dependency count**: 11 internal packages, **0 cycles**, instability range
`[0.18 (syntax), 0.91 (bootstrap)]`. The high-`I` packages are
aggregator/glue layers and have low `Ca` — no Stable Dependencies Principle
violations. Of ~50 internal-internal edges, none could be eliminated
without re-architecting. The "dependency reduction" win in this scope is
not removing imports but reducing *internal* hand-unrolling.

## Recommended phasing

Sequence from highest impact-per-effort to lowest:

| Phase | Finding(s) | Size | Gating                                        |
|-------|------------|------|-----------------------------------------------|
| 1     | 7          | XS   | None (dead-code delete)                       |
| 2     | 4          | S    | None                                          |
| 3     | 3          | S    | None (also unblocks cleaner Finding 2 sites)  |
| 4     | 2          | S    | After Finding 3 (dup-detect helper exists)    |
| 5     | 6          | S    | None (callers update mechanically)            |
| 6     | 5          | S–M  | None                                          |
| 7     | 1          | M    | Audit `NewSyntaxCons` callers in tests first  |

Phases 1–6 are mostly independent and can be picked off in any order.
Phase 7 is the largest because it touches `syntax/`, which has 14 dependent
packages — confirm migration completeness with the same approach
`values/` used (search for `(nil, nil)` cons producers; reject in the
constructor; delete defensive guards in lockstep).

## Cross-references

- `plans/2026-05-07-structural-reduction-roadmap.md` — **gating**.
  Recommends Tier A analyses (`values/`, `environment/`, `registry/`) before
  this plan's implementation. Phase 7 (the `*SyntaxPair`/`SyntaxEmptyList`
  migration) follows a precedent set in `values/`; confirm that precedent's
  closure first to avoid re-exporting a flawed pattern.
- `plans/2026-05-06-machine-structural-reduction.md` — companion analysis
  of `machine/`. Findings are independent; sequence either ahead.
- `internal/CLAUDE.md`, `internal/syntax/CLAUDE.local.md`,
  `internal/validate/CLAUDE.local.md`, `internal/match/CLAUDE.local.md`,
  `internal/parser/CLAUDE.local.md`, `internal/tokenizer/CLAUDE.local.md`
   — architectural references used to validate findings against existing
  invariants and migration history.
- `memory/2026-04-05-walk-sub-exprs-design.md` — original design of the
  `WalkSubExprs` primitive. Finding 5 / Opportunity 1 builds on it.
- TODO.md Tier 5 "FCA-Derived" — the analogous `vmCore sub-struct
  extraction` for `machine/` is a peer cleanup; sequencing is independent.
