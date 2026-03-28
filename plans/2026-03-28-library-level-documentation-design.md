# Library-Level Documentation Design

**Date**: 2026-03-28
**Status**: Approved
**Scope**: Parser, CompiledLibrary, REPL, reflection primitive, stdlib content

## Problem

No mechanism exists to document what a library provides. `.sld` files are pure `define-library` forms with no body to hang a description on. `,doc` can describe individual bindings but cannot answer "what does `(scheme base)` provide?"

## Decision Summary

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Authoring first, REPL second | Yes | Storage/parsing is foundational; REPL is a thin layer on top |
| Metadata entry | Non-standard `(description ...)` clause in `.sld` | Couples doc to source; these are Wile's own files, portability not a constraint |
| Metadata fields | `description` only (v1) | Version/author/license are package-manager concerns Wile doesn't have |
| Runtime storage | Field on `CompiledLibrary` | Struct already mixes compilation + metadata; one field is coherent |

## Design

### 1. Parser: New `description` clause

`processLibraryDeclaration()` in `machine/compile_library_forms.go` gains a case for `"description"`:

```scheme
(define-library (scheme base)
  (description "Core Scheme bindings per R7RS §6-7.")
  (export car cdr cons ...)
  (begin ...))
```

**Rules:**
- Clause body: exactly one string literal
- Multiple `description` clauses: last one wins
- Non-string argument: compile-time error
- Clause position: anywhere among other library declarations (order-independent, like `export`)

### 2. Storage: `CompiledLibrary.Description`

`machine/library_registry.go`:

```go
type CompiledLibrary struct {
    Name        LibraryName
    Description string  // from (description ...) clause; "" if absent
    Env         *environment.EnvironmentFrame
    Exports     map[string]string
    SourceFile  string
    Template    *NativeTemplate
}
```

No changes to `LibraryRegistry` — it stores `*CompiledLibrary`, so the field comes along for free.

### 3. REPL: `,doc (library-name)`

Extend `,doc` in `internal/repl/meta.go` to detect parenthesized library names:

1. If argument starts with `(` and ends with `)`, parse as library name
2. Look up in `LibraryRegistry` via the environment's namespace
3. Display:
   - Library name
   - Description (if present)
   - Source file
   - Export count
   - Sorted export list

**Dependency:** `MetaCommandHandler` needs access to `LibraryRegistry`. The registry is reachable via `EnvironmentFrame.Namespace()` → the library loader's registry. Determine the exact path during implementation.

### 4. Scheme primitive: `library-description`

`registry/core/reflection.go`:

```scheme
(library-description '(scheme base))  ;=> "Core Scheme bindings per R7RS §6-7."
(library-description '(nonexistent))  ;=> #f
```

- Takes a quoted library name (list of symbols)
- Returns string or `#f`
- Needs `MachineContext` access to `LibraryRegistry`

### 5. Stdlib content

Add `(description ...)` to all `.sld` files in `stdlib/lib/`. One sentence per library. Examples:

- `(scheme base)` — "Core Scheme bindings: pairs, lists, numbers, strings, vectors, control, exceptions, I/O."
- `(scheme write)` — "Output procedures: write, display, write-shared, write-simple."
- `(scheme time)` — "Time-related procedures: current-second, current-jiffy, jiffies-per-second."
- `(wile algebra)` — "Algebraic structures: partial orders, lattices, monoids, semirings, groups, rings, fields."

## Testing

| Test | Location | What it verifies |
|------|----------|------------------|
| Description parsed and stored | `machine/` test | `CompiledLibrary.Description` populated from `(description ...)` |
| Missing description | `machine/` test | `Description` is `""` when clause absent |
| Invalid description | `machine/` test | Non-string argument produces compile error |
| Multiple descriptions | `machine/` test | Last one wins |
| `,doc (scheme base)` | `internal/repl/` test | Output contains description and export list |
| `library-description` | `registry/core/` test | Returns string for known library, `#f` for unknown |

## Non-goals

- Per-binding documentation within libraries (already handled by procedure-documentation + docstrings)
- Version, author, license metadata (no package manager to consume it)
- Portable `.sld` files (Wile is the only consumer)
- Documentation rendering or HTML generation
