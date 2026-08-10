# Macro System Design

This document describes the design and implementation of Wile's hygienic macro system.

## Overview

Wile implements R7RS `syntax-rules` macros using Flatt's "sets of scopes" hygiene model (POPL 2016). The system consists of three layers:

```
┌─────────────────────────────────────────────────────────────┐
│  Layer 3: Hygiene Layer                                     │
│  - Scope creation and propagation                           │
│  - Variable resolution with scope matching                  │
│  - Files: values/scope.go, syntax/scope_utils.go,           │
│    machine/compilation/                                     │
│      operation_syntax_rules_transform.go                    │
├─────────────────────────────────────────────────────────────┤
│  Layer 2: Syntax Adapter                                    │
│  - Bridges syntax objects ↔ raw values                      │
│  - Preserves syntax for captured pattern variables          │
│  - File: internal/match/syntax_adapter.go                   │
├─────────────────────────────────────────────────────────────┤
│  Layer 1: Pattern Matching VM                               │
│  - Unhygienic bytecode-based pattern matcher                │
│  - Captures bindings, handles ellipsis repetition           │
│  - Files: internal/match/match.go,                          │
│    internal/match/syntax_compiler.go                        │
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

### Syntax Objects (`syntax/`)

Every parsed expression is wrapped in a syntax object that carries:

- **Source location**: File, line, column for error reporting
- **Scope set**: List of scopes for hygiene (see below)
- **Origin chain**: Tracks which macros produced this syntax

```go
type SyntaxSymbol struct {
    Sym             *values.Symbol
    syntaxBase                     // Contains SourceContext with Scopes []*Scope
    ResolvedBinding any            // Pre-resolved binding for cross-library macro hygiene
}
```

### Scopes (`values/scope.go`, re-exported as `syntax.Scope`)

A scope is a unique identifier created at specific points:

- **Intro scope**: Created for each macro invocation
- **Use-site scope**: Marks identifiers from the macro call site
- **Binding scope**: Marks binding positions (let, lambda parameters)

```go
type Scope struct {
    id          uint64 // ensures unique pointer identity
    IsRebinding bool   // true for let-syntax/letrec-syntax scopes
    Label       string // optional human-readable tag (e.g. "lambda",
                       // "intro:my-macro", "library:(wile kanren)")
}
```

### Transformer Closure (`machine/compilation/compile_syntax_rules.go`)

`CompileSyntaxRules` turns a `syntax-rules` form into a `MachineClosure` whose
template literals carry a `*ClausesWrapper`, holding one clause per
`(pattern template)` pair:

- **Compiled clauses**: Pattern bytecode + template for each pair
- **Literals set**: Symbols that match literally, not as pattern variables
- **Free identifiers**: Template symbols resolved at macro-definition time

The clause type lives in `machine/compilation/syntax_bridge_types.go` (the
compiler writes it, `OperationSyntaxRulesTransform` reads it at expansion
time, so its fields are exported):

```go
type SyntaxRulesClause struct {
    Template         syntax.SyntaxValue
    Bytecode         []match.SyntaxCommand
    Matcher          *match.SyntaxMatcher
    PatternVars      map[string]struct{}
    PatternVarSyntax map[string]*syntax.SyntaxSymbol
    EllipsisVars     map[int]map[string]struct{}
    FreeIds          map[string]*FreeIdResolution
    Ellipsis         string
    LiteralSyntax    map[string]*syntax.SyntaxSymbol
}
```

## Hygiene Model: Sets of Scopes

The hygiene algorithm ensures that:
1. Macro-introduced bindings don't capture user variables
2. User bindings don't capture macro-introduced references
3. Recursive macros work correctly

### Core Invariant

A reference resolves to a binding if and only if:

```
bindingScopes ⊆ useScopes
```

The binding's scope set must be a **subset** of the reference's scope set.

### Example: The `swap!` Macro

```scheme
(define-syntax swap!
  (syntax-rules ()
    ((swap! x y)
     (let ((tmp x))
       (set! x y)
       (set! y tmp)))))

(let ((tmp 5) (a 1) (b 2))
  (swap! a b)
  tmp)  ; => 5, NOT captured by macro's tmp
```

**Without hygiene**: The macro's `tmp` would capture the user's `tmp`.

**With hygiene**:
1. When `swap!` expands, a fresh intro scope `S1` is created
2. The macro's `tmp` gets scope set `{S1}`
3. The user's `tmp` has scope set `{}` (no macro scopes)
4. Resolution: `{S1} ⊆ {}` is FALSE, so they don't match
5. Each `tmp` resolves to its own binding

### Implementation in Code

**Scope creation** (`machine/compilation/operation_syntax_rules_transform.go`):
```go
introScope := syntax.NewScopeWithLabel("intro")
```

**Scope addition** (`applyHygieneToSymbol`, `internal/match/syntax_expand.go`):
```go
if opts.IntroScope != nil {
    newSym = newSym.AddScope(opts.IntroScope).(*syntax.SyntaxSymbol)
}
```

**Scope matching** (`values/scope.go`; `syntax.ScopesMatch` wraps it, and
`ScopesCompatible` is the entry point resolution actually calls, since a binding
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

The pattern matcher is a stack-based VM that operates on raw `values.Value` types.

### Bytecode Instructions

| Instruction | Purpose |
|-------------|---------|
| `ByteCodeCompareCar` | Compare car with literal value |
| `ByteCodeCaptureCar` | Capture car as pattern variable binding |
| `ByteCodeVisitCar` | Navigate into car (descend into nested list) |
| `ByteCodeVisitCdr` | Navigate to cdr (next element) |
| `ByteCodeDone` | End current level, pop value stack |
| `ByteCodePushContext` | Start ellipsis iteration (push capture context) |
| `ByteCodePopContext` | End ellipsis iteration (pop capture context) |
| `ByteCodeSkipIfEmpty` | Skip loop body if list is empty (zero iterations) |
| `ByteCodeJump` | Unconditional jump for loops |
| `ByteCodeRequireCarEmptyVector` | Assert car is an empty vector (`#()` pattern) |

### Ellipsis Handling

Ellipsis patterns (`...`) match zero or more repetitions:

```scheme
(pattern element ...)  ; Matches zero or more elements
```

The compiler generates a loop structure:
```
SkipIfEmpty +N      ; Jump past loop if empty (zero iterations OK)
PushContext         ; New capture context for this iteration
<pattern bytecode>  ; Match one element
PopContext          ; Save this iteration's captures
Jump -M             ; Loop back to check for more
```

Each iteration's captures are stored in child contexts, enabling template expansion to produce repeated output.

## Syntax Adapter (Layer 2)

The adapter bridges syntax objects and the unhygienic VM. The conceptual
operations below describe the role; the concrete implementation lives in
`internal/match/syntax_expand.go` (with helpers in `syntax_compiler.go`
and pattern-capture storage in `SyntaxMatcher`).

### Key Operations

**Strip syntax wrappers for pattern matching**:
```go
SyntaxPair → values.Pair
SyntaxSymbol → values.Symbol
SyntaxObject → underlying value
```

**Re-wrap expanded values with syntax** (`SyntaxMatcher.capturedValueToSyntax`,
`internal/match/syntax_expand.go`):
```go
values.Pair → SyntaxPair (with intro scope)
values.Symbol → SyntaxSymbol (with intro scope, unless free identifier)
```

**Preserve original syntax for captured variables**:

When a pattern variable is captured, the adapter stores a mapping from the raw value back to its original syntax object. During expansion, captured values are looked up in this map to preserve their original scopes (they should NOT get the intro scope).

## Macro Expansion (Layer 3)

### Expander Flow (`machine/compilation/expander_time_continuation.go`)

1. **Check for macro**: Is the head symbol bound to a `BindingTypeSyntax`?
2. **Check for shadowing**: Does a local variable shadow the macro? (R7RS §4.2.2)
3. **Invoke transformer**: Call the compiled transformer closure
4. **Recursive expansion**: Expand the result (macros can expand to macros)

### Free Identifier Handling

Free identifiers in a template are symbols that are NOT pattern variables. They refer to bindings outside the macro:

- The macro's own name (for recursive macros like `and`, `or`)
- References to other macros (`if`, `let`, `lambda`)
- References to primitives and library functions

Each is resolved once, at macro-definition time, and the resolution travels with
the expansion rather than being redone at the use site (R7RS §4.3.2 referential
transparency):

```go
// In compileClauseWithEllipsisAndLiterals:
freeIds := make(map[string]*FreeIdResolution)
collectFreeIdentifiersWithEllipsis(env, template, variables, freeIds, ellipsis, libraryScope)
```

The map is keyed by `FreeIdKey(name, definitionScopes)`, not by name alone: two
template identifiers spelled the same but carrying different definition-site
scope sets are different free identifiers.

`applyHygieneToSymbol` then does one of three things with the resolution:

- **Local binding at the definition site**: substitute the definition-site
  scopes, no intro scope. A scope-less reference would not be a superset of a
  binder keyed on the enclosing `let`'s scope.
- **Global binding**: keep the intro scope *and* attach the resolved
  `*GlobalIndex` via `WithResolvedBinding`, plus the library scope when there is
  one. The intro scope is what lets a binder co-introduced by the same template
  shadow the pin; the pin is consulted below that local match, and is what keeps
  a user's top-level `(define car 42)` from capturing a template's `car`.
- **Unresolvable at definition time**: fall through to the intro scope.

### Let Bindings Shadow Macros

Per R7RS §4.2.2, local variable bindings shadow macros:

```scheme
(define-syntax foo (syntax-rules () ((foo) 1)))
(let ((foo 2))
  (foo))  ; => Error: foo is not a procedure (it's 2, not the macro)
```

The expander checks for local variable bindings before macro lookup (`hasLocalVariableBinding`).

### Phase Tower (relative phase accessors)

A `define-syntax` at phase *N* binds a macro usable in phase-*N* code, and its
transformer *expression* is compiled and run at phase *N+1*. When that transformer
body itself defines and uses macros, those climb to *N+1*, *N+2*, … The climb is
realized by making six call sites, in four roles, *relative* to the expanding
frame's own `phaseLevel` via `EnvironmentFrame.NextPhase()`
(`environment/environment_frame.go`), rather than the absolute `Expand()`:

- transformer-body compilation (`compile_transformer.go`);
- `define-syntax` storage — both the top-level path (`compile_define_syntax.go`)
  and the internal-body path (`compileDefineSyntaxFromSyntax`, `expander_body.go`);
- macro lookup during expansion — arm 2 of `lookupMacroBinding`
  (`expander_time_continuation.go`); arm 1 reads the current phase and arm 3 the
  library env, so neither climbs;
- `begin-for-syntax` (`compile_helpers.go`) and `define-for-syntax`
  (`compile_define_for_syntax.go`) body execution. `(import (for-syntax …))`
  placement is relative by a different route: `ResolveAndInstallImportSet`
  composes the importing frame's `env.PhaseLevel()` with the import set's shift
  via `composePhaseShift` (`library_bindings.go`).

Two compile-time readers deliberately stay absolute — `LookupSyntaxCompiler`
(`env.Compile()`) and `LookupPrimitiveExpander` (`env.Expand()`) — because syntax
compilers and primitive expanders are registry fixtures on the sealed axis, not
user macros. Two more are absolute without being fixtures: `CompileMeta` and the
definition-site env `er-macro-transformer` stores (`compile_er_macro.go`) both
read `env.Expand()`, so they pin phase 1 even when the defining frame is higher.

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
;; => no such local or global binding "car"
```

Bindings are **shared single objects** across phases, not re-instantiated per
phase (Tier 1). Phase views stay hermetic as the climb reaches higher phases:
a read at phase *N* is a candidate only against slots at exactly phase *N* or
the ambient coordinate, never any other exact phase, so the climb never makes
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
*unmutated* cross-phase references and continues to search every phase the
owner's registry has actually instantiated (`PresentPhases()`), not a fixed
`[0,1,2]`.

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
per Q4 above. See `plans/2026-07-10-climbing-tower-q4-mutation-boundary-note.local.md` §7
and the Tier 2 sketch in the design doc.

## Bootstrap Macros

R7RS derived expressions are implemented as macros loaded during bootstrap. The sources are embedded in `registry/core/bootstrap.go` and loaded by `internal/bootstrap/bootstrap.go`. Binding forms (`let`, `let*`, `letrec`, `letrec*`) are *not* listed here — they are core compiled forms handled by the expander/validator/compiler pipeline; see [`core-let.md`](core-let.md) for the design.

There are two macro sources, and the split is load order. `bootstrap_macros.scm` loads first; `bootstrap_macros_late.scm` (`unless`, `guard`, `guard-aux`) loads *after* `bootstrap_procedures.scm`, because those templates reference bootstrap procedures (`not`, `with-exception-handler`) rather than Go primitives. Loading them early would leave those free identifiers with a nil definition-time pin, which a use-site redefinition could then capture.

The following forms are defined as `define-syntax` entries across the two files:

| Macro | Sketch |
|-------|--------|
| `and`, `or` | Short-circuit boolean expansion |
| `cond`, `case` | Conditional forms with `else` / `=>` auxiliary syntax |
| `when` | One-armed conditional (`unless` is in the late file) |
| `delay`, `delay-force` | Lazy evaluation via `%make-lazy-promise` |
| `parameterize` | Dynamic binding via `with-continuation-mark` (see [`r7rs-differences.md`](../reference/r7rs-differences.md)) |
| `unless`, `guard`, `guard-aux` | Late file: one-armed conditional; exception handling via the R7RS §7.3 double-`call/cc` pattern |
| `define-record-type`, `define-opaque-record-type`, `define-record-type-impl` | SRFI-9 records and the opaque-record variant |
| `let-values`, `let*-values`, `define-values` | Multiple-value binding |
| `do` | R7RS §4.2.4 iteration |
| `with-continuation-barrier`, `with-baffle` | Continuation-barrier forms |

These are loaded during environment initialization and use the same macro system as user-defined macros.

## File Reference

| File | Purpose |
|------|---------|
| `internal/match/match.go` | Pattern matching VM |
| `internal/match/syntax_compiler.go` | Pattern → bytecode compiler |
| `internal/match/syntax_adapter.go` | Syntax ↔ value conversion |
| `internal/match/syntax_expand.go` | Template expansion, hygiene, free-identifier resolution |
| `values/scope.go` | `Scope` type, `ScopesMatch` / `ScopesCompatible`, `ScopeSet` |
| `syntax/scope_utils.go` | Re-exports of the above, syntax-tree scope operations |
| `syntax/syntax_symbol.go` | Symbol with scopes |
| `syntax/syntax_pair.go` | Pair with recursive scope propagation |
| `machine/compilation/compile_syntax_rules.go` | `syntax-rules` compilation |
| `machine/compilation/syntax_bridge_types.go` | `SyntaxRulesClause`, `FreeIdResolution`, `ClausesWrapper` |
| `machine/compilation/operation_syntax_rules_transform.go` | Runtime macro expansion |
| `machine/compilation/expander_time_continuation.go` | Expansion-phase walker |
| `registry/core/bootstrap.go` | Embeds `bootstrap_macros.scm` and `bootstrap_macros_late.scm` |
| `internal/bootstrap/bootstrap.go` | Bootstrap load order |

## References

- **Flatt 2016**: "Binding as Sets of Scopes" - https://www.cs.utah.edu/plt/scope-sets/
- **R7RS §4.3**: Macros - https://small.r7rs.org/attachment/r7rs.pdf
- See `BIBLIOGRAPHY.md` at project root for complete references
