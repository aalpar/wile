# Special Form & Macro Docstrings — Design

**Goal:** Make `,doc` display documentation for compiler special forms and bootstrap macros. No new Scheme-level primitives — `,doc` in the REPL is the only consumer.

**Scope:** ~20 compiler special forms + ~15 bootstrap macros = ~35 doc entries.

## Mechanism

### Data Model

**`BindingMeta` gets a `Doc` field** (`environment/binding.go`):

```go
type BindingMeta struct {
    Scopes []*syntax.Scope
    Source *syntax.SourceContext
    Doc    string              // documentation for this binding
}
```

Add `Doc() string` and `SetDoc(string)` accessors on `Binding`. Lazy-allocate `meta` on write (same pattern as `SetScopes`/`SetSource`). `Clone()` already copies `meta` — `Doc` comes along for free. The pointer indirection means zero cost for bindings without documentation.

**`BindingSpec` type** (`registry/registry.go`):

```go
type BindingSpec struct {
    Name string
    Doc  string
}
```

Replaces `[]string` for compile-time binding registration. `AddBindings([]string)` wraps each string into `BindingSpec{Name: s}` for backward compatibility. New `AddBindingSpecs([]BindingSpec)` method accepts specs with docs.

**`DocEntry` type** (`registry/registry.go`):

```go
type DocEntry struct {
    Name string
    Doc  string
}
```

For post-bootstrap doc injection. `AddDocumentation(name, doc string)` stores entries. `Docs() []DocEntry` returns a defensive copy.

### Registration Flow

Two populations with different timing:

**Compiler special forms** (~20) — attached at compile-time binding creation:

```
BindingSpec{Name: "if", Doc: "..."}
  → Registry.AddBindingSpecs()
    → Apply() → registerCompileTimeBinding(env, spec)
      → MaybeCreateOwnGlobalBinding() → bnd.SetDoc(spec.Doc)
```

`registerCompileTimeBinding` changes signature from `(env, name string)` to `(env, spec BindingSpec)`. After creating the binding, calls `bnd.SetDoc(spec.Doc)` when non-empty.

**Bootstrap macros** (~15) — injected after bootstrap loads:

```
DocEntry{Name: "and", Doc: "..."}
  → Registry.AddDocumentation()
    → Engine calls ApplyDocs(env) after bootstrap
      → for each entry: find binding across phases → bnd.SetDoc(doc)
```

`ApplyDocs(env)` searches compile env first, then expand env, then runtime env. This runs after bootstrap macro loading, so all `define-syntax` bindings exist in the expand environment.

### Lookup Flow

No new lookup paths. The existing `,doc` phase environment walk in `meta.go` already finds bindings — it just has no content to show. The only change is in `formatBindingDoc`:

```go
func formatBindingDoc(w *strings.Builder, name string, bnd *environment.Binding, phase int) {
    phaseName := phaseLabel(phase)

    switch bnd.BindingType() {
    case environment.BindingTypePrimitive:
        fmt.Fprintf(w, "%s: special form (%s)\n", name, phaseName)
    case environment.BindingTypeSyntax:
        fmt.Fprintf(w, "%s: syntax transformer (%s)\n", name, phaseName)
    case environment.BindingTypeVariable:
        val := bnd.Value()
        fmt.Fprintf(w, "%s: %s (%s)\n", name, val.SchemeString(), phaseName)
    default:
        fmt.Fprintf(w, "%s: bound in %s\n", name, phaseName)
    }

    if doc := bnd.Doc(); doc != "" {
        fmt.Fprintf(w, "\n%s\n", doc)
    }
}
```

## Changes by Package

| Package | File | Change |
|---------|------|--------|
| `environment` | `binding.go` | Add `Doc string` to `BindingMeta`, `Doc()`/`SetDoc()` on `Binding` |
| `registry` | `registry.go` | Add `BindingSpec`, `DocEntry` types; `AddBindingSpecs`, `AddDocumentation`, `Docs` methods; `docs []DocEntry` field |
| `registry` | `apply.go` | `registerCompileTimeBinding` takes `BindingSpec`; new `ApplyDocs(env)` method |
| `registry/core` | `specialforms.go` | Convert to `[]BindingSpec` with docs; add `[]DocEntry` for macro docs |
| `internal/repl` | `meta.go` | `formatBindingDoc` displays `bnd.Doc()` |
| `wile/` (engine) | engine call site | Call `ApplyDocs(env)` after bootstrap macro loading |

## What Doesn't Change

- `procedure-documentation` primitive — special forms aren't procedures, returns `#f`
- `RegistryDocProvider` — still handles foreign procedure docs via `PrimitiveSpec.Doc`
- `DocProvider` interface — unchanged
- `NativeTemplate.Doc()` / `ForeignClosure.Doc()` — unchanged

## Content: Compiler Special Forms

All entries in `specialforms.go` as `[]BindingSpec`:

| Form | Doc focus |
|------|-----------|
| `if` | Conditional expression; test, consequent, optional alternate |
| `lambda` | Anonymous procedure; formals, body with implicit begin |
| `case-lambda` | Multi-clause dispatch by argument count |
| `quote` | Literal datum, suppresses evaluation |
| `define` | Variable or procedure definition; body scoping |
| `define-syntax` | Macro definition; transformer expression |
| `set!` | Mutation; variable must already be bound |
| `begin` | Sequence; splicing vs. expression context |
| `include`, `include-ci` | File inclusion at expand time |
| `cond-expand` | Feature-based conditional expansion |
| `quasiquote`, `unquote`, `unquote-splicing` | Template construction with escapes |
| `dynamic-wind` | Before/after thunks around body |
| `apply` | Procedure application with argument list (compile-time binding) |
| `with-continuation-mark` | Attach key-value mark to current continuation frame |
| `let`, `let*`, `letrec`, `letrec*` | Binding forms; scope and evaluation order differences |
| `syntax-rules` | Pattern-based macro transformer |
| `syntax-error` | Compile-time error in macro expansion |
| `else`, `=>`, `...`, `_` | Auxiliary syntax — role in patterns, not callable |
| `define-for-syntax`, `begin-for-syntax`, `eval-when` | Phase-specific evaluation |

## Content: Bootstrap Macros

All entries via `AddDocumentation` as `[]DocEntry`:

| Macro | Doc focus |
|-------|-----------|
| `and` | Short-circuit boolean conjunction; returns last true or `#f` |
| `or` | Short-circuit boolean disjunction; returns first true or `#f` |
| `cond` | Multi-way conditional; `else` and `=>` clauses |
| `case` | Dispatch on datum equality; `eqv?`-based matching |
| `when`, `unless` | One-armed conditionals; implicit `begin` in body |
| `do` | Iteration with variable bindings, step expressions, and exit test |
| `guard` | Exception handling with re-raise semantics |
| `parameterize` | Dynamic binding of parameter objects |
| `delay`, `delay-force` | Lazy evaluation; promise creation |
| `define-record-type` | Record type definition with constructor, predicate, accessors |
| `let-values`, `let*-values` | Multiple-value binding forms |
| `define-values` | Multiple-value definition |

## What This Enables Later

- **User `define-syntax` docs**: Any mechanism that calls `binding.SetDoc()` works. A future `define-syntax` docstring convention (e.g., metadata form) can use the same storage.
- **`apropos` search**: Can scan bindings across all phases for doc content — `BindingMeta.Doc` is the single source.
- **Unify `,doc` with `procedure-documentation`**: `formatBindingDoc` now has real content, narrowing the gap between the two paths.

## Out of Scope

- No new Scheme-level primitive (`syntax-documentation` or `documentation` by name)
- No Guile-style docstring convention for `syntax-rules` bodies
- No library-level documentation
- No `apropos` search (separate TODO item)
- No changes to `procedure-documentation` behavior
