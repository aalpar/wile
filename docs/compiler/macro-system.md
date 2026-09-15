# Macro System Design

This document describes the design and implementation of Wile's hygienic macro system.

## Overview

Wile implements R7RS `syntax-rules` macros using Flatt's "sets of scopes" hygiene model (POPL 2016). The system consists of three layers:

```
┌─────────────────────────────────────────────────────────────┐
│  Layer 3: Hygiene Layer                                     │
│  - Use-site and intro scope per invocation; intro flip      │
│  - Free-identifier resolution at definition time            │
│  - Variable resolution with scope matching                  │
│  - Files: pkg/values/scope.go, pkg/syntax/scope_utils.go,   │
│    pkg/machine/compilation/                                 │
│      expander_time_continuation.go, expander_use_site.go,   │
│      compile_syntax_rules.go,                               │
│      operation_syntax_rules_transform.go                    │
├─────────────────────────────────────────────────────────────┤
│  Layer 2: Syntax Adapter                                    │
│  - Scope-aware literal matching (R7RS §4.3.2)               │
│  - Template expansion: captures substituted unchanged,      │
│    template identifiers get definition-site scopes + pins   │
│  - Files: pkg/internal/match/syntax_adapter.go,             │
│    pkg/internal/match/syntax_expand.go                      │
├─────────────────────────────────────────────────────────────┤
│  Layer 1: Pattern Matching VM                               │
│  - Bytecode pattern matcher over syntax values              │
│  - Captures bindings, handles ellipsis repetition           │
│  - Files: pkg/internal/match/match.go,                      │
│    pkg/internal/match/syntax_compiler.go                    │
└─────────────────────────────────────────────────────────────┘
```

## Processing Pipeline

```
Source Code
    │
    ▼
┌─────────┐
│ Parser  │ → Syntax objects with source locations
└────┬────┘
     │
     ▼
┌──────────┐
│ Expander │ → Detects macro invocations, invokes transformers
└────┬─────┘
     │ (recursive until no macros remain)
     ▼
┌──────────┐
│ Compiler │ → Bytecode operations
└────┬─────┘
     │
     ▼
┌────┐
│ VM │ → Execution
└────┘
```

## Key Data Structures

### Syntax Objects (`pkg/syntax/`)

Every parsed expression is wrapped in a syntax object that carries:

- **Source location**: File, line, column for error reporting
- **Scope set**: List of scopes for hygiene (see below)
- **Origin chain**: Tracks which macros produced this syntax

```go
type SyntaxSymbol struct {
    Sym             *values.Symbol
    syntaxBase                     // Contains SourceContext with Scopes []*Scope
    ResolvedBinding ResolvedRef    // Definition-time *environment.GlobalIndex pin for a free template identifier
}
```

### Scopes (`pkg/values/scope.go`, re-exported as `syntax.Scope`)

A scope is a unique identifier created at specific points:

- **Intro scope**: Created for each macro invocation; added to the whole input
  form and flipped on the output, so only transformer-introduced identifiers
  keep it
- **Use-site scope**: Created for each macro invocation and added to the input
  form, never flipped; pruned only from the binder of a definition the output
  makes (`pruneUseSiteScopes`, `expander_use_site.go`)
- **Binding scope**: Marks binding forms (`let`, `letrec`, `lambda`,
  `let-syntax`, `with-binding-scope`)
- **Library scope**: Minted per `define-library` and registered with the
  namespace, so an identifier carrying it can reach that library's environment

```go
type Scope struct {
    id          uint64 // ensures unique pointer identity
    IsRebinding bool   // true for let-syntax/letrec-syntax scopes
    Label       string // optional human-readable tag (e.g. "lambda",
                       // "intro", "use-site", "library:(wile kanren)")
}
```

### Transformer Closure (`pkg/machine/compilation/compile_syntax_rules.go`)

`CompileSyntaxRules` turns a `syntax-rules` form into a `MachineClosure` whose
template literals carry a `*ClausesWrapper`, holding one clause per
`(pattern template)` pair:

- **Compiled clauses**: Pattern bytecode + template for each pair
- **Literals set**: Symbols that match literally, not as pattern variables
- **Free identifiers**: Template symbols resolved at macro-definition time

The clause type lives in `pkg/machine/compilation/syntax_bridge_types.go` (the
compiler writes it, `OperationSyntaxRulesTransform` reads it at expansion
time, so its fields are exported):

```go
type SyntaxRulesClause struct {
    Template         syntax.SyntaxValue
    Bytecode         []match.SyntaxCommand
    Matcher          *match.SyntaxMatcher
    PatternVarSyntax syntax.PatternVarSymbols
    EllipsisVars     map[int]values.StringSet
    FreeIds          map[string]*FreeIdResolution
    Ellipsis         string
    LiteralSyntax    syntax.LiteralSymbols
}
```

## Hygiene Model: Sets of Scopes

The hygiene algorithm ensures that:
1. Macro-introduced bindings don't capture user variables
2. User bindings don't capture macro-introduced references
3. Recursive macros work correctly

### Core Invariant

A binding is a candidate for a reference if and only if:

```
bindingScopes ⊆ useScopes
```

The binding's scope set must be a **subset** of the reference's scope set. Among
candidates the one with the largest scope set wins (for globals, after a binding
tier ranking); an equal-size tie between different sets has no answer and raises
`werr.ErrAmbiguousBinding`.

### Example: The `swap!` Macro

```scheme
(define-syntax swap!
  (syntax-rules ()
    ((swap! x y)
     (let ((tmp x))
       (set! x y)
       (set! y tmp)))))

(let ((tmp 5) (other 6))
  (swap! tmp other)
  (list tmp other))  ; => (6 5)
```

**Without hygiene**: textual substitution gives
`(let ((tmp tmp)) (set! tmp other) (set! other tmp))`; the macro's `tmp`
captures the user's `tmp` passed as `x`, nothing is swapped, and the result is
`(5 6)`.

**With hygiene** (scopes added by the `let` forms omitted):
1. When `swap!` expands, a fresh intro scope `S1` and a use-site scope `U` are created
2. The macro's `tmp` ends with scope set `{S1}`
3. The user's `tmp`, passed through as `x`, ends with `{U}` (no `S1`)
4. Resolution of the user's `tmp`: `{S1} ⊆ {U}` is FALSE, so the macro's binder is not a candidate
5. Each `tmp` resolves to its own binding

### Implementation in Code

**Scope creation and flip** (`ExpanderTimeContinuation.expandMacroInvocation`,
`pkg/machine/compilation/expander_time_continuation.go`). One mechanism serves
every closure transformer (`syntax-rules`, and `syntax-case` via a `lambda`
transformer); `OperationSyntaxRulesTransform` passes no intro scope of its own.
`er-macro-transformer` stays outside the flip: its hygiene is the rename
closure, which carries its own fresh scope (`invokeERTransformer`).
```go
inputForm := p.withUseSiteScope(syntax.NewSyntaxCons(sym, expr, sym.SourceContext()))
introScope := syntax.NewScopeWithLabel("intro")
inputForm = syntax.AddScopeToSyntax(inputForm, introScope)
// ... invoke the transformer on inputForm ...
stx = syntax.FlipScope(stx, introScope)
stx = p.pruneUseSiteScopes(stx)
```

**Scope matching** (`pkg/values/scope.go`; `syntax.ScopesMatch` wraps it, and
`ScopesCompatible` is the entry point most resolution sites call, since a binding
with no scopes matches any reference):
```go
func ScopesMatch(useScopes, bindingScopes []*Scope) bool {
    // bindingScopes ⊆ useScopes
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

## Pattern Matching VM (Layer 1)

The pattern matcher (`Matcher`) is a stack-based VM that walks syntax values
directly (`MatchSyntaxWithLiterals` takes a `*syntax.SyntaxPair`) and captures
`syntax.SyntaxValue`s, so no strip/re-wrap step exists. For a pattern literal it
requires the same spelling and then defers the binding comparison to a
`LiteralMatcher` callback supplied by Layer 2.

### Bytecode Instructions

| Instruction | Purpose |
|-------------|---------|
| `ByteCodeCompareCar` | Compare car with literal value |
| `ByteCodeCompareCdr` | Compare a dotted tail with a literal value |
| `ByteCodeCaptureCar` | Capture car as pattern variable binding |
| `ByteCodeCaptureCdr` | Capture a dotted tail (`(_ a . rest)`) |
| `ByteCodeDiscardCar` | Match an element without binding (`_`) |
| `ByteCodeDiscardCdr` | Consume a dotted tail without binding (`(_ a . _)`) |
| `ByteCodeVisitCar` | Navigate into car (descend into nested list) |
| `ByteCodeVisitCarAsVector` | Descend into a vector's elements as a pair chain |
| `ByteCodeVisitCarAsBox` | Descend into a box's content as a one-element chain |
| `ByteCodeVisitCdr` | Navigate to cdr (next element) |
| `ByteCodeDone` | End current level, pop value stack |
| `ByteCodePushContext` | Start ellipsis iteration (push capture context) |
| `ByteCodePopContext` | End ellipsis iteration (pop capture context) |
| `ByteCodeSkipIfEmpty` | Skip loop body if list is empty (zero iterations) |
| `ByteCodeSkipIfTailCount` | Exit an ellipsis loop when exactly N elements remain (`(a ... b c)`) |
| `ByteCodeJump` | Unconditional jump for loops |
| `ByteCodeRequireCarEmptyVector` | Assert car is an empty vector (`#()` pattern) |

### Ellipsis Handling

Ellipsis patterns (`...`) match zero or more repetitions:

```scheme
(pattern element ...)  ; Matches zero or more elements
```

The compiler generates a loop structure (`emitEllipsisLoop`, `syntax_compiler.go`):
```
SkipIfEmpty +N      ; Jump past loop if empty (zero iterations OK);
                    ;   SkipIfTailCount when pattern elements follow the ellipsis
PushContext         ; New capture context for this iteration
<pattern bytecode>  ; Match one element
VisitCdr            ; Advance (omitted when the element is a nested pattern ending in Done)
PopContext          ; Save this iteration's captures
Jump -M             ; Loop back to check for more
```

Each iteration's captures are stored in child contexts, enabling template expansion to produce repeated output.

## Syntax Adapter (Layer 2)

`SyntaxMatcher` (`pkg/internal/match/syntax_adapter.go`) wraps `Matcher` with
hygiene; template expansion lives in `pkg/internal/match/syntax_expand.go`.
Neither adds the intro scope; the expander's flip does (Layer 3).

### Key Operations

**Literal matching** (`literalScopesMatchWithDef`): R7RS §4.3.2 compares the
pattern literal's binding at the definition site with the input identifier's
binding at the use site. The definition side is a `LiteralPin` captured when the
`syntax-rules` form was compiled (`resolveLiteralDefinitions`); the use side
comes from the `BindingChecker` built over the use-site environment
(`envBindingChecker`, `operation_syntax_rules_transform.go`).

**Pattern-variable substitution** (`expandSymbol`): a template symbol with a
capture under its name is replaced by that capture unless
`TemplateDenotesPatternVariable` refuses, i.e. the pattern variable's scopes are
not a subset of the occurrence's (an outer macro introduced the occurrence). The
capture is the syntax object that was matched, scopes unchanged.

**Template identifiers** (`applyHygieneToSymbol`): every other template symbol is
rebuilt with the use site's source position but its own definition-site scope
set, then given its free-identifier resolution (below). `capturedValueToSyntax`
only wraps a capture that is not already a syntax value.

## Macro Expansion (Layer 3)

### Expander Flow (`pkg/machine/compilation/expander_time_continuation.go`)

1. **Check for shadowing**: Does a local variable shadow the head? (R7RS §4.2.2)
2. **Check for macro**: Does `lookupMacroBinding` find a `BindingTypeSyntax` binding?
3. **Invoke transformer** (`expandMacroInvocation`): stamp the input with a use-site
   scope and a fresh intro scope, call the transformer, flip the intro scope on the
   result, and prune use-site scopes from definition binders
4. **Recursive expansion**: Expand the result (macros can expand to macros)

### Free Identifier Handling

Free identifiers in a template are symbols that are NOT pattern variables. They refer to bindings outside the macro:

- The macro's own name (for recursive macros like `and`, `or`)
- References to special forms and other macros (`if`, `lambda`, `cond`)
- References to primitives and library functions

Each is resolved once, at macro-definition time, and the resolution travels with
the expansion rather than being redone at the use site (R7RS §4.3.2 referential
transparency):

```go
// In compileClauseWithEllipsisAndLiterals:
freeIds := make(map[string]*FreeIdResolution)
collectFreeIdentifiersWithEllipsis(env, template, varSyntax, freeIds, ellipsis, libraryScope)
```

The map is keyed by `FreeIdKey(name, definitionScopes)`, not by name alone: two
template identifiers spelled the same but carrying different definition-site
scope sets are different free identifiers.

Every template identifier keeps its definition-site scope set, never the use
site's; that alone lets a template name a binder in the macro's own lexical
context (a `let` variable, a sibling `define-syntax` in the same body), because
the identifier's scopes are a superset of that binder's. `applyHygieneToSymbol`
then applies the resolution, and the flip in `expandMacroInvocation` adds the
intro scope to the result in every case:

- **Global binding**: attach the resolved `*GlobalIndex` via
  `WithResolvedBinding`, plus the library scope when there is one. The intro
  scope is what lets a binder co-introduced by the same template shadow the pin;
  the pin is consulted below that local match, and is what keeps a program's
  top-level `(define car 42)` from capturing the `car` in a library or bootstrap
  macro's template. (A macro defined in the same top level sees that
  redefinition.)
- **Local binding at the definition site**: substitute the binder's scopes. This
  arm fires only for `(syntax-rules …)` in expression position inside a phase-0
  local frame: a `define-syntax`, `let-syntax`, or `letrec-syntax` right-hand side
  compiles one phase up (`compileTransformerValue`), in a phase view with no
  lexical locals (see `CompileSyntaxRulesExpr`).
- **Unresolvable at definition time**: nothing beyond the definition-site scopes.

### Let Bindings Shadow Macros

Per R7RS §4.2.2, local variable bindings shadow macros:

```scheme
(define-syntax foo (syntax-rules () ((foo) 1)))
(let ((foo 2))
  (foo))  ; => Error: application: expected a procedure, got 2
```

The expander checks for local variable bindings before macro lookup (`hasLocalVariableBinding`).

### Phase Tower (relative phase accessors)

A `define-syntax` at phase *N* binds a macro usable in phase-*N* code, and its
transformer *expression* is compiled and run at phase *N+1*. When that transformer
body itself defines and uses macros, those climb to *N+1*, *N+2*, … The climb is
realized by making six call sites, in four roles, *relative* to the expanding
frame's own `phaseLevel` via `EnvironmentFrame.NextPhase()`
(`pkg/environment/environment_frame.go`), rather than the absolute `Expand()`:

- transformer-body compilation (`compile_transformer.go`);
- `define-syntax` storage — both the top-level path (`compile_define_syntax.go`)
  and the internal-body path (`compileDefineSyntaxFromSyntax`, `expander_body.go`);
- macro lookup during expansion: arm 2 of `lookupMacroBinding`
  (`expander_time_continuation.go`); arm 1 reads the current phase, the
  definition-site pin names a fixed slot, and arms 2b and 3 read fixed rungs
  (below), so none of those climbs;
- `begin-for-syntax` / `eval-when` (`executeFormsAtCompileTime`,
  `compile_helpers.go`) and `define-for-syntax`
  (`compile_define_for_syntax.go`) body execution. `(import (for-syntax …))`
  placement is relative by a different route: `ResolveAndInstallImportSet`
  composes the importing frame's `env.PhaseLevel()` with the import set's shift
  via `composePhaseShift` (`library_bindings.go`).

One compile-time reader deliberately stays absolute: `LookupPrimitiveExpander`
(`env.Expand()`), because primitive expanders are registry fixtures at
`(1, sealed)`. Syntax compilers are `(0, sealed)` slots
(`RegisterSyntaxCompilers`); a higher phase reaches them through bulk rows declared
at that phase, not through a phase-blind coordinate (the ambient tier is gone).
Three more readers are absolute without being fixtures: `CompileMeta` reads
`env.Expand()`, so it pins phase 1 even when the defining frame is higher;
`lookupMacroBinding` arm 2b reads the owner's sealed phase-1 tier from phase 2 up
(and when arm 2 found only a registry primitive), which keeps startup macros such
as `and` reachable from a transformer nested in a transformer; arm 3 reads the
symbol's library env at `Expand()`. `er-macro-transformer` is not absolute: it
stores the frame its right-hand side compiled in (`env.NextPhase()`, via
`CompileERMacroTransformerExpr` in `compile_transformer_forms.go`), and its rename
closure reads that store at the phase of the expansion that uses it
(`invokeERTransformer`).

At `phaseLevel 0` (top level) `NextPhase() == Expand()`, so top-level macro
expansion is byte-for-byte unchanged (the *level-0 identity* safety property);
the climb fires only inside a nested compile-time form: a transformer body, or a
`begin-for-syntax` / `define-for-syntax` / `eval-when` inside one. This
distinguishes two macro shapes: a **declarative** macro places its inner
`define-syntax` in its expansion *output* (the same phase as its use, always
consistent, so the tower does not fire), while a **procedural** macro whose
*transformer body* defines and uses macros genuinely climbs. The pinning case is
a name reused at two phases
(`TestClimbingTower_CrossPhaseCollision`): pre-tower it collapsed into the single
expand frame and the higher definition clobbered the lower; the tower keeps them
separate.

The tower is observable from Scheme, not just from Go. Running under
`--strict=no-bindings` (nothing ambient, so every name must be imported at a
stated phase) makes the rung a program lands on visible:

```scheme
;; program A
(import (for-meta 2 (scheme base)))                  ; car bound at phase 2
(begin-for-syntax (begin-for-syntax (car '(1 2))))   ; body runs at phase 2 => ok

;; program B, a separate file: the imports do not accumulate
(import (for-syntax (scheme base)))                  ; car bound at phase 1
(begin-for-syntax (begin-for-syntax (car '(1 2))))   ; body runs at phase 2
;; => no such local or global binding "car" at phase 2 of this unit's macro tower
```

Bindings are **shared single objects** across phases, not re-instantiated per
phase (Tier 1). Phase views stay hermetic as the climb reaches higher phases:
a read at phase *N* is a candidate only against slots and bulk rows at exactly
phase *N*, never any other phase, so the climb never makes
a higher phase see a lower one's bindings — key disjointness in the one store,
not a missing parent link (`TestPhaseRegistry_ExpandPhaseIsHermetic` guards
this). Hermeticity holds for every owner of a store, and a `NewChildRuntime`
library environment is one: it has its own store and its own sealed-write
views over the same `sealedAxis` rows a namespace has, so phase 1 there does
**not** see phase 0. See [environment/system.md](../environment/system.md#invariants)
invariant 6.
The bounded `int8` phase index caps a runaway self-referential macro with a
wrapped error rather than wrapping to −128.

The `GetGlobalIndexAcrossPhases` phase-0 carve-out (R7RS §4.3 macro-generating-macro
resolution: `jabberwocky`/`march-hare`) is unaffected by the climb — it resolves
*unmutated* cross-phase references and continues to search, lowest first, every
non-negative phase the owner has either instantiated a view for or holds a slot
at (`PresentPhases()`), not a fixed `[0,1,2]`.

**Q4 — mutable state across a phase climb (resolved: hermetic by rejection).**
Tier 1 shares bindings as single objects, so the question arose whether a
`set!`-mutated binding could be *silently shared* across a climb, computing wrong
compile-time values. It cannot: hermeticity as key disjointness in the store
already severs the cross-phase path, so a mutable binding defined at phase N is
**not visible** at phase N+1 — observing it there is a loud compile-time
`no such binding`, not a silent share
(`TestClimbingTower_CrossPhaseMutationIsHermetic`).

This argument was sound at the top level and **false inside a `define-library`
body** until 2026-08-05: there, `GetGlobalIndexFromLibraryScopes` searched the
library env's phases `{0, 1, 2}` regardless of the referring phase, so a
phase-1 body resolved a phase-0 define — silently, as the `#!void` of a
predeclared-but-unwritten slot — and a phase-0 body resolved a
`begin-for-syntax` define, silently, as a wrong value. That arm is now
phase-relative and the library env owns its own store with its own sealed
coordinates, so the argument is sound in both places. Pinned by
`TestLibraryPhaseIsolation{Downward,Upward}`
(`pkg/wile/library_phase_isolation_test.go`), each of which asserts the library
answer *against its top-level control* rather than on its own.

The only surviving cross-phase
reach, `GetGlobalIndexAcrossPhases`, resolves a free template identifier to a
single binding location *per reference scope set* (the R7RS §4.3 carve-out). The
name alone does not name a location: since globals became scope-keyed, two
expansions of a macro-generating macro each bind under their own intro scope and
resolve to their own slot. A `set!` of a resolved location is still
observed coherently, with no per-phase divergence
(`TestClimbingTower_MarchHareMutatedIsCoherent` — mutated `march-hare` still
yields its mutated value). So no `ErrCrossPhaseMutation` check is needed: the
hermetic rejection *is* the loud failure, at no cost and with no false positive on
the carve-out. The boundary is enforced by rejection (nearer Racket's
rejection-by-construction) rather than a bespoke diagnostic.

**Not built (Tier 2):** separate per-phase *instantiation* (independent mutable
state per phase) is not implemented. This is a **capability** gap (it would
*enable* independent per-phase mutable counters for a procedural/`syntax-parse`
authoring pivot), not a soundness gap — there is no unsound program for it to fix,
per Q4 above. See `memory/2026-07-10-climbing-tower-q4-mutation-boundary-note.md` §7
and the Tier 2 sketch in the design doc.

## Bootstrap Macros

R7RS derived expressions are implemented as macros loaded during bootstrap. The sources are embedded in `pkg/registry/core/bootstrap.go` and loaded by `LoadBootstrapCore` (`pkg/internal/bootstrap/bootstrap_core.go`). Binding forms (`let`, `let*`, `letrec`, `letrec*`) are *not* listed here; they are core compiled forms handled by the expander/validator/compiler pipeline; see [`core-let.md`](core-let.md) for the design.

By default there are two macro sources, and the split is load order. (The opt-in Scheme syntax layer, `WithSchemeSyntaxForms` or `WILE_SYNTAX_FORMS=scheme`, prepends `bootstrap_syntax_procedures.scm` and `bootstrap_syntax.scm`, which define `syntax-case`, `syntax`, `with-syntax`, `quasisyntax`, `unsyntax`, `unsyntax-splicing`, `syntax-rules`, and `er-macro-transformer` in Scheme in place of the Go forms this document describes.) `bootstrap_macros.scm` loads first; `bootstrap_macros_late.scm` (`unless`, `guard`, `guard-aux`) loads *after* `bootstrap_procedures.scm`, because those templates reference bootstrap procedures (`not`, `with-exception-handler`) rather than Go primitives. Loading them early would leave those free identifiers with a nil definition-time pin, which a use-site redefinition could then capture.

The following forms are defined as `define-syntax` entries across the two files:

| Macro | Sketch |
|-------|--------|
| `and`, `or` | Short-circuit boolean expansion |
| `cond`, `case` | Conditional forms with `else` / `=>` auxiliary syntax |
| `when` | One-armed conditional (`unless` is in the late file) |
| `delay`, `delay-force` | Lazy evaluation via `%make-lazy-promise` |
| `parameterize` | Dynamic binding via `with-continuation-mark` (see [`r7rs-differences.md`](../reference/r7rs-differences.md)) |
| `unless`, `guard`, `guard-aux` | Late file: one-armed conditional; exception handling via the R7RS §7.3 two-continuation pattern (`call-with-exit` for the outer escape, `call/cc` for the re-raise) |
| `define-record-type`, `define-opaque-record-type`, `define-record-type-impl` | SRFI-9 records and the opaque-record variant |
| `let-values`, `let*-values`, `define-values` | Multiple-value binding |
| `do` | R7RS §4.2.4 iteration |
| `with-continuation-barrier`, `with-baffle` | Continuation-barrier forms |

These are loaded during environment initialization and use the same macro system as user-defined macros.

## File Reference

| File | Purpose |
|------|---------|
| `pkg/internal/match/match.go` | Pattern matching VM |
| `pkg/internal/match/syntax_compiler.go` | Pattern → bytecode compiler |
| `pkg/internal/match/syntax_adapter.go` | `SyntaxMatcher`: scope-aware literal matching |
| `pkg/internal/match/syntax_expand.go` | Template expansion, hygiene, free-identifier resolution |
| `pkg/values/scope.go` | `Scope` type, `ScopesMatch` / `ScopesCompatible`, `ScopeSet` |
| `pkg/syntax/scope_utils.go` | Re-exports of the above, syntax-tree scope operations |
| `pkg/syntax/syntax_symbol.go` | Symbol with scopes |
| `pkg/syntax/syntax_pair.go` | Pair with recursive scope propagation |
| `pkg/machine/compilation/compile_syntax_rules.go` | `syntax-rules` compilation |
| `pkg/machine/compilation/syntax_bridge_types.go` | `SyntaxRulesClause`, `FreeIdResolution`, `ClausesWrapper` |
| `pkg/machine/compilation/operation_syntax_rules_transform.go` | `syntax-rules` transformer runtime: clause selection, template expansion |
| `pkg/machine/compilation/expander_time_continuation.go` | Expansion-phase walker; intro-scope mint and flip (`expandMacroInvocation`) |
| `pkg/machine/compilation/expander_use_site.go` | Use-site scopes and their pruning from definition binders |
| `pkg/registry/core/bootstrap.go` | Embeds `bootstrap_macros.scm` and `bootstrap_macros_late.scm` |
| `pkg/internal/bootstrap/bootstrap_core.go` | Bootstrap load order (`LoadBootstrapCore`) |

## References

- **Flatt 2016**: "Binding as Sets of Scopes" - https://www.cs.utah.edu/plt/scope-sets/
- **R7RS §4.3**: Macros - https://small.r7rs.org/attachment/r7rs.pdf
- See `BIBLIOGRAPHY.md` at project root for complete references
