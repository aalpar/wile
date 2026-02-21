# Plan: Dialect System

**Status**: Proposed
**Date**: 2026-02-20
**Related**: `SANDBOXING_MODEL.md` (prerequisite patterns), `docs/EXTENSIONS.md`

## Motivation

Wile's engine is language-agnostic below the surface. The bytecode VM, continuation model, scope system, and eval stack don't encode R7RS — they encode "a Lisp with hygienic macros." The R7RS personality lives in four configurable layers on top. Three of these layers are already per-engine configurable. One isn't.

Making all four layers configurable turns Wile from "an R7RS implementation" into "a Lisp platform that ships with R7RS as the default dialect." An embedder could use R6RS, a custom DSL, or a Clojure-flavored Lisp — same VM, different personality.

This aligns with "embedding is the product": embedders choose not just which capabilities their engine has, but which *language* it speaks.

## Architecture: Four Layers of Language Personality

```
┌──────────────────────────────────────────────────────┐
│ Layer 1: Runtime Primitives           [per-registry] │
│ What: names, arities, implementations                │
│ Examples: car/cdr, +/-, string-ref, eval             │
│ Configurable today: YES (WithRegistry, WithExtension)│
└──────────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────────┐
│ Layer 2: Bootstrap Macros             [per-registry] │
│ What: derived forms as Scheme source text             │
│ Examples: and, or, let, cond, guard, define-values   │
│ Configurable today: YES (AddMacroSource)             │
└──────────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────────┐
│ Layer 3: Syntax Compilers + Expanders [per-environment]│
│ What: how special forms are expanded and compiled     │
│ Examples: define-syntax, import, syntax-case          │
│ Configurable today: YES (RegisterSyntaxCompilers is  │
│   a data table bound per-environment)                │
└──────────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────────┐
│ Layer 4: Forms Registry                    [GLOBAL]  │
│ What: core form validators + compilers               │
│ Examples: if, lambda, define, set!, quote, begin     │
│ Configurable today: NO — package-level global in     │
│   internal/forms/form_spec.go:58                     │
│ THIS IS THE ONLY STRUCTURAL BLOCKER                  │
└──────────────────────────────────────────────────────┘
```

## The Blocker: Global Forms Registry

`internal/forms/form_spec.go:58`:

```go
var registry = make(map[string]*FormSpec)
```

Populated by `init()` in `machine/register.go`. Maps form names (`"if"`, `"lambda"`, `"define"`, `"set!"`, `"quote"`, `"begin"`, `"quasiquote"`, `"dynamic-wind"`) to validator + compiler function pairs. Every engine in the same process shares this map.

**Why it's global:** The forms registry is used by both the validator (`internal/validate/`) and the compiler (`machine/compile_validated.go:53`). The validator needs to know "is this name a special form?" to produce the right `ValidatedExpr` type. The compiler needs to know "which compiler handles this form?" to emit bytecode. Both currently call `forms.Lookup()` which hits the global.

**Two-tier dispatch** (`machine/syntax_compilers_registry.go:29-34`):

- **Tier 1 (Validated Forms)**: `if`, `lambda`, `define`, `set!`, `quote`, `begin`, `quasiquote`, `dynamic-wind`, `case-lambda` — go through the validation layer to produce typed `ValidatedExpr` nodes, then compile via `compileValidated*` methods. Registered in the global forms registry.
- **Tier 2 (Registry Forms)**: `define-syntax`, `import`, `syntax-case`, `include`, `cond-expand`, etc. — pass through validation as `ValidatedLiteral`, dispatched via the per-environment syntax compiler table.

Tier 2 is already per-environment. Only Tier 1 is global.

## What a Dialect Bundles

```go
// Dialect defines a complete language personality for a Wile engine.
type Dialect struct {
    // Layer 1: runtime primitives and their names
    Registry func(*registry.Registry) error

    // Layer 2: derived forms as Scheme source
    MacroSources []string

    // Layer 3: syntax compilers (Tier 2 forms)
    SyntaxCompilers func(*environment.EnvironmentFrame) error

    // Layer 3: primitive expanders
    PrimitiveExpanders func(*environment.EnvironmentFrame) error

    // Layer 4: core form validators + compilers (Tier 1 forms)
    FormSpecs map[string]*forms.FormSpec

    // Compile-time binding names (special forms + auxiliary syntax)
    CompileTimeBindings []string

    // Features for cond-expand
    Features []string
}
```

Usage:

```go
eng, err := wile.NewEngine(ctx,
    wile.WithDialect(r7rs.Dialect),
    wile.WithExtension(files.Extension),
    wile.WithLibraryPaths("./lib"),
)
```

`r7rs.Dialect` is the default — identical to today's behavior. If no dialect is specified, `NewEngine` uses R7RS.

## Dialect Variations: What Changes Where

### R6RS

| What changes | Layer | Detail |
|-------------|-------|--------|
| Library syntax | 3 | R6RS `(library ...)` has fixed-order declarations (export, import, body). Different syntax compiler for `library`. R7RS `define-library` allows any-order declarations. |
| Condition system | 1+2 | `&condition`, `&assertion`, `condition`, `make-assertion-violation` — new primitives + macros replacing R7RS `error` objects |
| Error strictness | 1 | R6RS "must raise" vs R7RS "is an error" (undefined). Primitives check a strictness flag or are separate implementations. |
| Records | 1+2 | R6RS `define-record-type` with inheritance, sealed, opaque — richer than R7RS SRFI-9 style |
| `syntax-case` emphasis | already supported | Both `syntax-rules` and `syntax-case` work today |
| Hashtables | 1 | R6RS `(rnrs hashtables)` has different API from Wile's current hashtables |
| Tail context strictness | 4 | R6RS requires tail position in more contexts (e.g., `let-values`) |
| `import` semantics | 3 | R6RS `import` only inside `library`, not at top-level (different expander rule) |

### Custom Lisp / DSL

| What changes | Layer | Detail |
|-------------|-------|--------|
| Rename special forms | 4 | `fn` instead of `lambda`, `def` instead of `define` |
| Rename primitives | 1 | `first`/`rest` instead of `car`/`cdr` |
| Different derived forms | 2 | Clojure-style `defn`, `let` with vector bindings |
| Remove mutation | 1+4 | No `set!`, `set-car!`, `set-cdr!` — remove from forms registry + primitives |
| Add new types | values pkg | Persistent vectors, atoms, keywords — deeper change, outside dialect system |

## Concrete Work Items

### Phase 1: De-globalize the forms registry

Replace the global `map[string]*FormSpec` in `internal/forms/form_spec.go` with a `FormRegistry` type that can be instantiated per-engine.

**Current:**

```go
// internal/forms/form_spec.go:58
var registry = make(map[string]*FormSpec)

func Lookup(name string) *FormSpec {
    return registry[name]
}
```

**Proposed:**

```go
type FormRegistry struct {
    specs map[string]*FormSpec
}

func NewFormRegistry() *FormRegistry { ... }
func (r *FormRegistry) Register(spec *FormSpec) { ... }
func (r *FormRegistry) Lookup(name string) *FormSpec { ... }
func (r *FormRegistry) Clone() *FormRegistry { ... }
```

A default `FormRegistry` is built once (equivalent to today's `init()`) and cloned per-engine.

**Blast radius:**

| What | Where | Change |
|------|-------|--------|
| Global registry | `internal/forms/form_spec.go` | Replace with `FormRegistry` type |
| Init registration | `machine/register.go` `init()` | Build a default `FormRegistry` instead of mutating global |
| Compiler dispatch | `machine/compile_validated.go:38` (`compileValidated`) | `forms.Lookup(...)` → `p.formRegistry.Lookup(...)` |
| CompileTimeContinuation | `machine/compile_time_continuation.go` | Add `formRegistry *forms.FormRegistry` field |
| Validator | `internal/validate/` | Validators call `forms.Lookup()` to check special-form status; need registry passed in |
| Expander | `machine/expander_time_continuation.go` | May reference forms registry for form detection |
| Engine construction | `engine.go` | Build/clone forms registry, pass to compiler |

The validator is the subtlest part. `internal/validate/` uses `forms.Lookup()` to decide whether an identifier names a special form (which determines which `ValidatedExpr` type to produce). The registry needs to flow into the validation path, likely through the validation context.

### Phase 2: Dialect type and WithDialect option

Define the `Dialect` type and `WithDialect()` engine option.

```go
// wile/dialect.go
type Dialect struct {
    Name               string
    BuildRegistry      func(*registry.Registry) error
    MacroSources       []string
    FormSpecs          *forms.FormRegistry
    SyntaxCompilers    func(*environment.EnvironmentFrame) error
    PrimitiveExpanders func(*environment.EnvironmentFrame) error
    CompileTimeBindings []string
    Features           []string
}
```

`NewEngine` uses `Dialect.FormSpecs` instead of the global forms registry. If no dialect is specified, it uses `r7rs.DefaultDialect`.

### Phase 3: Extract R7RS as the default dialect

Move all current R7RS-specific configuration into `r7rs.DefaultDialect`:

- `core.AddToRegistry` → `Dialect.BuildRegistry`
- `core.compileTimeBindings` → `Dialect.CompileTimeBindings`
- `core.bootstrapMacroSource` → `Dialect.MacroSources`
- `machine.RegisterSyntaxCompilers` table → `Dialect.SyntaxCompilers`
- `machine.RegisterPrimitiveExpanders` table → `Dialect.PrimitiveExpanders`
- `machine/register.go` `init()` registrations → `Dialect.FormSpecs`
- `machine.Features()` → `Dialect.Features`

After this phase, Wile behaves identically but R7RS is a configuration, not hardcoded.

### Phase 4: Second dialect (validates the abstraction)

Implement one non-R7RS dialect to prove the abstraction works. Candidates:

**Option A: R7RS-minimal** — R7RS without mutation. Removes `set!`, `set-car!`, `set-cdr!`, `vector-set!`, `string-set!`, `list-set!`, `hashtable-set!`, `set-box!` from primitives and removes `set!` from the forms registry. Simple, verifiable, useful for sandboxing. Connects to `SANDBOXING_MODEL.md` Phase 4 (registry filtering).

**Option B: R6RS-core** — R6RS library syntax + condition system. More ambitious, proves the library system is dialect-configurable. Requires a new `CompileR6RSLibrary` syntax compiler and condition type primitives.

**Option C: Custom DSL** — Minimal Lisp with renamed forms (`fn`, `def`, `let`). Proves form-name independence. Least useful for production but fastest to implement.

Recommendation: **Option A first** (validates the forms registry de-globalization with minimal risk), then **Option B** (validates the full dialect system with real semantic differences).

## Scope Boundaries

**Covered by this plan:**
- De-globalizing the forms registry
- `Dialect` type and `WithDialect()` option
- Extracting R7RS as the default dialect
- Validating with a second dialect

**Not covered:**
- New value types (persistent vectors, condition types) — these require `values/` package changes, orthogonal to the dialect system
- Parser changes — the tokenizer/parser is Scheme-generic (s-expressions). A dialect that needs different syntax (e.g., Clojure's `[]`, `{}`) would need parser extensions, which is a separate effort
- REPL personality — prompt strings, error formatting, help text. Cosmetic, handled by `cmd/scheme/` configuration
- R6RS full compliance — would be a large effort; the dialect system enables it but doesn't deliver it

## Dependencies

- Phase 1 (forms registry): None — internal refactor
- Phase 2 (Dialect type): Phase 1
- Phase 3 (extract R7RS): Phase 2
- Phase 4 (second dialect): Phase 3
- `SANDBOXING_MODEL.md` Phase 4 (`Registry.Without`) composes with dialect system for mutation-free dialects

## Relationship to Sandboxing

The dialect system and sandboxing address orthogonal axes:

- **Sandboxing**: Which *capabilities* does code have? (filesystem, eval, system)
- **Dialects**: Which *language* does code speak? (R7RS, R6RS, custom)

They compose: an embedder can use an R6RS dialect with sandboxed capabilities, or an R7RS dialect with full access. The `WithRegistry()` / `Without()` / `WithExtension()` API works identically regardless of dialect — dialects configure the compiler and macro layer, extensions configure the runtime primitive layer.

## Decision Log

| Decision | Rationale |
|----------|-----------|
| De-globalize forms registry, not per-environment | Per-environment would require the forms registry in `EnvironmentFrame`, adding weight to every environment. Per-engine (via `CompileTimeContinuation`) is sufficient — all code in one engine speaks one dialect. |
| Dialect as struct, not interface | Dialects are data (tables of specs), not behavior. A struct with function fields is simpler than an interface with methods. |
| R7RS as default, not special | After extraction, R7RS is just `r7rs.DefaultDialect`. No special-casing in the engine. |
| Validator needs registry access | Validators produce different `ValidatedExpr` types based on whether a name is a special form. The forms registry must flow into validation, not just compilation. |
| Phase 4 starts with mutation-free variant | Smallest delta from R7RS. Validates the abstraction without requiring new primitives, types, or library system changes. |
| Parser not included | S-expression syntax is shared across all Lisp dialects. Dialects that need different surface syntax (brackets, braces) are a parser concern, not a dialect concern. |
