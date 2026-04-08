# Closing REPL Documentation Gaps

**Date**: 2026-04-07
**Status**: Phases 1-4 Complete
**Scope**: Extension library descriptions, global value docs, missing procedure/macro docs,
algebra docstrings (2 remaining), structured docstring metadata infrastructure

## Current State

| Layer | Total | Documented | Gap |
|-------|-------|------------|-----|
| Go-registered primitives | 399 | 399 | 0 |
| Special forms & auxiliary syntax | 41 | 41 | 0 |
| Bootstrap macros | 15 | 15 | 0 |
| Bootstrap procedures | 45 | 45 | 0 |
| Extension library descriptions | 11 | 0 | **11** |
| Global port parameters | 3 | 0 | **3** |
| Extension Scheme procs/macros | 3 | 0 | **3** |
| Algebra procedures | 63 | 61 | **2** |
| Structured metadata (bootstrap) | ~30 | 0 | **~30** |
| Wile stdlib procs (control/kanren) | ~29 | ~29 | 0 |
| Third-party stdlib procs (SRFI-1, chibi) | ~242 | 0 | **~242** |

Core primitives and special forms are at 100%. The wile-original stdlib
libraries (algebra, control, kanren, microkanren) already have Guile-style
docstrings with structured metadata. The actionable work falls into five
tiers of decreasing impact-per-effort.

## Bug: RegistryDocProvider holds frozen clone

**Status:** Confirmed via Go test + MCP debugging.

The import observer (`makeDocRegistrationObserver` in `engine.go`) correctly
registers Scheme-defined procedures into the live `*registry.Registry` when
libraries are imported. The Scheme primitives `(doc-topics)` and `(apropos ...)`
read from the live registry via `ns.Registry()` and **work correctly**.

However, both the CLI REPL and MCP server create a `RegistryDocProvider` at
startup from `eng.Registry()`, which returns a **Clone** — a frozen snapshot.
The `,topics` and `,apropos` meta commands read from this snapshot and never
see procedures registered by the import observer.

### Affected sites

| Site | File | Line | Impact |
|------|------|------|--------|
| CLI REPL | `cmd/wile/main.go` | 406 | `,topics` and `,apropos` miss imported library procedures |
| MCP server | `cmd/wile/mcp.go` | 229 | MCP `topics`/`apropos` tools miss imported library procedures |

### Fix options

**Option A: Pass live registry pointer, not clone.**
Change `eng.Registry()` to not clone, or add `eng.LiveRegistry()`.
Risk: breaks the defensive-copy contract that protects callers from
concurrent mutation.

**Option B: RegistryDocProvider reads live registry on each call.**
Change `RegistryDocProvider` to hold a pointer to the original `*Registry`
(not a clone). Since the registry is mutex-protected, concurrent reads
are safe. The only reason `Registry()` clones is to avoid holding the
lock across caller code — but `RegistryDocProvider` methods are short.

**Option C: Pass the registry directly from the namespace.**
Instead of `eng.Registry()`, use `eng.Environment().Namespace().Registry()`
which returns the original pointer. Then `RegistryDocProvider` gets the
live registry.

**Recommended: Option C** — minimal change, no new API, no contract change.
Two lines in `main.go` and `mcp.go`.

### Verification

Go test confirms the Scheme primitive path works. After the fix, verify:
1. `(import (wile algebra group))` then `,topics` shows `algebra`
2. `(import (wile algebra group))` then `,apropos group-op` finds it

## Dependencies

The structured docstring metadata infrastructure
(`plans/2026-04-06-structured-docstring-metadata-impl.md`) is **already
implemented**: `internal/docparse/` exists (as `docparse/`),
`RegisterSchemeDocstrings` runs at bootstrap, and `makeDocRegistrationObserver`
hooks into library imports. The original audit incorrectly listed this as
pending.

Phases 1, 2, and 4 are independent of each other.

## Phase 1: Extension Library Descriptions (11 libraries)

**Effort:** Small (one function change + 11 strings)
**Impact:** High — every user sees blank descriptions in `,doc (wile io)`

### Problem

`registerExtensionLibraries` in `engine.go:617` creates `CompiledLibrary`
objects without setting the `Description` field. The field exists on
`CompiledLibrary` (added in PR #581 for `.sld` libraries) but extension-backed
libraries have no mechanism to populate it.

### Approach: `Describer` interface

Add an optional interface to `registry/`:

```go
// Describer is implemented by extensions that provide a library description.
type Describer interface {
    Description() string
}
```

In `registerExtensionLibraries`, after creating the `CompiledLibrary`, check
if the extension implements `Describer`:

```go
if d, ok := snap.ext.(Describer); ok {
    lib.Description = d.Description()
}
```

Each extension in `internal/extensions/` and `extensions/` gains a one-line
`Description() string` method.

### Content

| Library | Description |
|---------|-------------|
| `(wile all)` | All Wile extensions combined. |
| `(wile eval)` | Code evaluation: eval, load, include, macroexpand. |
| `(wile files)` | Filesystem operations: file I/O, directory traversal, temporary files. |
| `(wile gointerop)` | Go interop: Go value wrapping, struct access, method calls. |
| `(wile introspection)` | Runtime introspection: procedure metadata, disassembly, environment inspection. |
| `(wile io)` | I/O ports: reading, writing, string/bytevector ports, display, write. |
| `(wile math)` | Extended math: trigonometry, logarithms, bitwise operations. |
| `(wile namespace)` | Namespace management: environment creation, binding inspection. |
| `(wile process)` | OS processes: command execution, exit, environment variables. |
| `(wile system)` | System primitives: time, sleep, features, command line. |
| `(wile threads)` | Concurrency: SRFI-18 threads, mutexes, condition variables, channels. |

### Verification

`,doc (wile io)` shows description. All 11 libraries show non-blank in `,libraries`.

---

## Phase 2: Global Values + Missing Extension Docs (6 bindings)

**Effort:** Small (6 doc registrations)
**Impact:** Medium — R7RS standard bindings show no documentation

### 2a: Port Parameters (3 bindings)

`current-input-port`, `current-output-port`, `current-error-port` are registered
via `AddGlobalValue` which has no `Doc` field. Rather than expanding `GlobalValue`,
use the existing `AddDocumentation` mechanism (already used for bootstrap macros).

In `internal/extensions/io/register.go`, after the `AddGlobalValue` calls, add:

```go
p.AddDocumentation("current-input-port",
    "Parameter holding the default input port.\nParameterize to redirect standard input within a dynamic extent.\n\nCategory: ports")
p.AddDocumentation("current-output-port",
    "Parameter holding the default output port.\nParameterize to redirect standard output within a dynamic extent.\n\nCategory: ports")
p.AddDocumentation("current-error-port",
    "Parameter holding the default error port.\nParameterize to redirect standard error within a dynamic extent.\n\nCategory: ports")
```

### 2b: `call-with-port` (1 procedure)

Defined in `internal/extensions/io/port_procs.scm`. Add a Guile-style docstring:

```scheme
(define (call-with-port port proc)
  "Call PROC with PORT as its sole argument, then close PORT.\nThe port is closed whether PROC returns normally or raises\nan exception. Returns the value returned by PROC.\n\nParameters:\n  port : port\n  proc : procedure\nReturns: any\nCategory: ports\n\nSee also: `call-with-input-file', `call-with-output-file'."
  (let-values ((results (proc port)))
    (close-port port)
    (apply values results)))
```

### 2c: `with-input-from-file` and `with-output-to-file` (2 macros)

Defined as `syntax-rules` macros in `extensions/files/with_file_macros.scm`.
Macros can't carry Guile-style docstrings. Use `AddDocumentation` in
`extensions/files/register.go`:

```go
p.AddDocumentation("with-input-from-file",
    "Open FILENAME for input and call THUNK with current-input-port\nbound to the opened port. Closes the port when THUNK returns.\n\nCategory: files")
p.AddDocumentation("with-output-to-file",
    "Open FILENAME for output and call THUNK with current-output-port\nbound to the opened port. Closes the port when THUNK returns.\n\nCategory: files")
```

### Verification

`,doc current-input-port` shows description and category.
`,doc call-with-port` shows structured output.
`,doc with-input-from-file` shows description.

---

## Phase 3: Fix RegistryDocProvider frozen clone

**Effort:** Trivial (2 lines)
**Impact:** High — unblocks all doc registration for imported libraries
**Prerequisite for:** Phase 5 (third-party docstrings visible in `,topics`)

The structured docstring metadata infrastructure is already implemented.
The only remaining issue is the frozen-clone bug in the CLI REPL and MCP server.

### Changes

**`cmd/wile/main.go:406`:**
```go
// Before:
docProv := repl.NewRegistryDocProvider(eng.Registry())

// After:
docProv := repl.NewRegistryDocProvider(
    eng.Environment().Namespace().Registry().(*registry.Registry))
```

**`cmd/wile/mcp.go:229`:** Same change.

### Verification

1. Build fresh binary
2. `(import (wile algebra group))` then `,topics` shows `algebra`
3. `(import (wile algebra group))` then `,apropos group-op` finds it
4. Existing tests pass (`make test && make lint`)

---

## Phase 4: Algebra Docstring Gaps (2 procedures)

**Effort:** Trivial (2 docstrings)
**Impact:** Low — internal helpers, but completeness matters

`tropical-min` and `tropical-add` in `stdlib/lib/wile/algebra/semiring.scm`
are the only algebra procedures without docstrings. They're internal helpers
for `tropical-semiring`, but since they're exported (visible to users who
import the library), they should be documented.

```scheme
(define (tropical-min a b)
  "Return the lesser of A and B under tropical arithmetic.\nIn the tropical semiring, addition is defined as min.\n\nParameters:\n  a : number\n  b : number\nReturns: number\nCategory: algebra\n\nSee also: `tropical-semiring'."
  (if (< a b) a b))

(define (tropical-add a b)
  "Return the sum of A and B under tropical arithmetic.\nIn the tropical semiring, multiplication is defined as +.\n\nParameters:\n  a : number\n  b : number\nReturns: number\nCategory: algebra\n\nSee also: `tropical-semiring'."
  (+ a b))
```

### Verification

`,doc tropical-min` shows structured output. All 63 algebra procedures documented.

---

## Phase 5: Third-Party Stdlib Docstrings (~242 procedures)

**Effort:** Large (content authoring across 13 files)
**Impact:** Medium — used less frequently than core, but SRFI-1 is common
**Depends on:** Phase 3 (structured metadata infrastructure for `,apropos`/`,topics` visibility)

### Sub-phases

| Sub-phase | Library | Files | ~Procs | Notes |
|-----------|---------|-------|--------|-------|
| 5a | `(srfi 1)` | 9 files | 99 | Most commonly used. Lean on SRFI-1 spec language |
| 5b | `(chibi diff)` | 1 file | 14 | Diff/patch. Document observed behavior |
| 5c | `(chibi optional)` | 1 file | 4 | Optional/keyword args |
| 5d | `(chibi test)` | 1 file | 61 | Test framework. Many are internal |
| 5e | `(chibi term ansi)` | 1 file | 64 | ANSI terminal. Mechanical color/style names |

Each sub-phase: write docstrings → `make test` → verify `,doc` output → commit.

### Approach for third-party code

These are adapted from external projects. Docstrings should:
- Describe observable behavior (what the function does)
- Reference the SRFI spec where applicable (e.g., "See SRFI-1 for full specification.")
- Use structured metadata sections (`Parameters:`, `Returns:`, `Category:`)
- Not claim authorship or originality

### Question: scope boundary

Should internal helpers (procedures not exported by the `.sld`) get docstrings?
They're not user-visible via `,doc` since they're not in the export list. Documenting
them is pure maintenance value — no REPL impact. Recommend: skip internal helpers,
document only exported procedures.

---

## Execution Order

```
Phase 3 (fix frozen clone)       ── do this FIRST, 2 lines, unblocks everything
           │
Phase 1 (ext lib descriptions) ─┐
Phase 2 (global + missing docs) ─┼── independent, can parallelize
Phase 4 (algebra 2 procs)       ─┘
           │
Phase 5 (third-party stdlib)    ── content authoring
```

Phase 3 is a 2-line fix. Do it first — it unblocks visibility for all
subsequent doc work. Phases 1, 2, and 4 are small, independent, single PRs.
Phase 5 is large enough to warrant one PR per sub-phase.

## Out of Scope

- `define-syntax` macro docstrings (no compiler mechanism exists)
- Documentation generation tooling (HTML, man pages)
- Auto-generating PRIMITIVES.md from registry
- Runtime type enforcement from docstring declarations
