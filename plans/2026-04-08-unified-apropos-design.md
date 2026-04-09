# Unified Apropos Search

Consolidate the two apropos search paths (Scheme `(apropos)` and REPL `,apropos`)
into a single search function so both produce identical results.

## Problem

`PrimApropos` in `registry/core/prim_reflection.go` only searches registry
primitives. The REPL `,apropos` (via `RegistryDocProvider.Search` +
`searchBindings` + `searchLibraries`) also searches binding specs, doc entries,
environment bindings, and loaded libraries. Keywords on library-defined Scheme
functions are invisible to the Scheme-level `(apropos)`.

## Design

### New: `registry/search.go`

Add a unified search function and result type to `registry/`:

```go
type DocSearchResult struct {
    Name     string
    Doc      string
    Category string
    Keywords []string
}

func SearchDoc(reg *Registry, env *environment.EnvironmentFrame,
    libReg *compilation.LibraryRegistry, pattern string) []DocSearchResult
```

`SearchDoc` searches five sources in order:
1. Registry primitives (`reg.Primitives()`)
2. Registry binding specs (`reg.BindingSpecs()`)
3. Registry doc entries (`reg.Docs()`)
4. Environment bindings (walk phase environments from `env`)
5. Loaded libraries (from `libReg`)

Primitives take precedence over non-primitives with the same name (existing
behavior). Results sorted by name. Match logic: case-insensitive substring
against name, doc text, category, or any keyword.

Helper functions (`matchesFields`, keyword matching) move here as unexported.

### New imports for `registry/`

- `machine/compilation` — for `*LibraryRegistry` (no cycle; verified)
- `docparse/` — for `ParseDocstring` (already transitively available)

`environment/` is already imported in `apply.go`.

### Changes to `repl/`

**Delete:** `repl.DocSearchResult` — use `registry.DocSearchResult` instead.

**Delete:** `matchesFields`, `containsKeywordLower` — moved to `registry/search.go`.

**Delete:** `searchBindings`, `searchLibraries` methods on `MetaCommandHandler` —
absorbed into `registry.SearchDoc`.

**Modify:** `RegistryDocProvider` gains env and libReg fields:
```go
type RegistryDocProvider struct {
    reg    *registry.Registry
    env    *environment.EnvironmentFrame
    libReg *compilation.LibraryRegistry
}
```

`NewRegistryDocProvider` signature changes to accept all three. Callers updated.

**Modify:** `RegistryDocProvider.Search` delegates to `registry.SearchDoc`:
```go
func (p *RegistryDocProvider) Search(pattern string) []registry.DocSearchResult {
    return registry.SearchDoc(p.reg, p.env, p.libReg, pattern)
}
```

**Modify:** `DocSearchProvider` interface uses `registry.DocSearchResult`:
```go
type DocSearchProvider interface {
    DocProvider
    Search(pattern string) []registry.DocSearchResult
    Categories() []string
    ByCategory(category string) []registry.DocSearchResult
}
```

**Modify:** `cmdApropos` — remove `searchBindings`/`searchLibraries` calls
and `mergeSearchResults`. The provider's `Search` now returns everything.

**Keep:** `DocInfo`, `DocProvider`, `LookupDoc`, `Categories`, `ByCategory`,
all formatting functions — unchanged.

### Changes to `registry/core/prim_reflection.go`

`PrimApropos` calls `registry.SearchDoc` instead of hand-rolling the search:
```go
func PrimApropos(mc machine.CallContext) error {
    // ... extract pattern, registry ...
    env := mc.EnvironmentFrame()
    libReg := extractLibraryRegistry(env)
    results := registry.SearchDoc(reg, env, libReg, pattern)
    // convert to sorted symbol list as before
}
```

Delete the local `containsKeyword` helper.

### What does NOT change

- Scheme-level API: `(apropos pattern)` still returns a sorted list of symbols
- REPL output format: `,apropos` still shows name/category/description columns
- MCP tool: delegates to `,apropos`, no changes needed
- `LookupDoc`, `Categories`, `ByCategory` — unchanged
- `DocInfo` type in `repl/` — display-oriented, not search-related

## Testing

- Existing `repl/registry_doc_provider_test.go` search tests continue to pass
  (adapted for `registry.DocSearchResult`)
- New tests in `registry/search_test.go` for `SearchDoc` covering all five
  sources, keyword matching, deduplication, and nil env/libReg
- Existing `PrimApropos` tests continue to pass
