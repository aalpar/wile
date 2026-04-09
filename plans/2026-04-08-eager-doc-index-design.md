# Eager Documentation Index

## Problem

Documentation is only discoverable through live bindings. An LLM calling
`(apropos "lattice")` gets nothing until `(import (wile algebra))` loads the
library, creates bindings, and attaches docstrings. The documentation exists
on disk in `.sld` files, but the doc system can't see it because docs are
properties of bindings, not independent entries.

This matters because LLMs are the primary consumers of the documentation
system. They use `apropos` and `doc` to discover what's available before
writing code. When discovery fails, they waste tool-call rounds on broken
attempts and hit round limits before computing answers. Benchmark data
(see `~/ClaudeProjects/LLMAccuracy/docs/mcp-documentation-tools.md`) shows
treatment accuracy drops below control specifically when the LLM can't
discover the right library to import.

## Insight

The doc system conflates two concerns: "what does this binding do?" (attached
to the implementation) and "what's available?" (an index over all known
libraries). The first concern is well-served by docs-on-bindings. The second
requires an index that exists before any library is imported.

A 4th phase (PhaseDoc) was considered and rejected. The existing phases
(Runtime, Expand, Compile) represent stages of the compilation pipeline.
Documentation isn't a pipeline stage — shoehorning it into the phase system
would be mechanically convenient but semantically misleading.

## Proposed Design

**Eager library metadata scan at engine initialization.**

At startup, the engine scans all `.sld` files reachable via the FileResolver
chain. For each library, it extracts:

- Library name (from `define-library`)
- Description (from `(description ...)` clause)
- Export list (from `(export ...)` clause)
- Per-export docstrings (from `(define ...)` forms in included files, if parseable)

This metadata is registered as `DocEntry` items in the registry (or a
parallel doc index). `SearchDoc` already searches doc entries — no new
search infrastructure needed.

**What this does NOT do:**
- Does not compile or execute library code
- Does not create runtime bindings
- Does not replace docs-on-bindings for loaded libraries (those remain
  authoritative once loaded)

**What this enables:**
- `(apropos "lattice")` finds `lattice-join` before `(wile algebra)` is imported
- `(doc "lattice-join")` shows the docstring from the `.sld` file
- LLMs discover the right library to import on the first tool call
- The MCP `apropos` tool returns useful results without pre-importing libraries

## Open Questions

- **Parsing depth:** Extracting export lists from `.sld` files is straightforward
  (S-expression parsing only). Extracting per-export docstrings requires parsing
  the included `.scm` files and finding `(define ...)` forms with docstrings.
  Is the simpler version (library name + description + export names only)
  sufficient for LLM discovery?

- **Staleness:** If a library's `.sld` changes after the engine starts, the
  index is stale. This is acceptable — the engine is not long-lived in
  typical use, and the loaded-library docs override the index.

- **Startup cost:** Scanning all `.sld` files adds initialization time.
  The scan is I/O-bound (read small text files) and should be negligible,
  but should be measured.

- **Index location:** Should this be a separate `DocIndex` type, or should
  `DocEntry` items be added to the existing registry? The registry approach
  is simpler but mixes "available" with "loaded."
