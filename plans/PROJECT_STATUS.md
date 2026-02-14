# Wile Project Status

**Current Version**: v1.2.0 (released 2026-02-11)

## Implementation Status

### Language Features

All R7RS-small features complete: lexical scoping, hygienic macros (syntax-rules, syntax-case, Flatt 2016), first-class + delimited continuations, full numeric tower, proper tail calls, records, exceptions, libraries, promises.

### Runtime

Bytecode compiler, stack-based VM, SRFI-18 threading, Go concurrency interop, pure Go (no CGo), source tracking.

### Standard Libraries

All 16 R7RS standard libraries complete. Additional: SRFI-1, SRFI-18, `(chibi test)`, `(chibi optional)`, `(chibi diff)`, `(chibi term ansi)`.

### Extensions (Internal)

9 extensions in `internal/extensions/`: io, files, math, system, threads, eval, exceptions, gointerop, all.

### Examples & Documentation

73 examples across 12 categories, plus 21 Gabriel benchmarks and Schelog. Comprehensive docs: README, PRIMITIVES.md, CHANGELOG, design docs, dev docs.

### Test Infrastructure

~302 Go test files (~85% median coverage). Scheme test infrastructure complete but only 1 smoke test exists.

### Tooling

REPL (readline, history, debugger), Makefile, CI/CD, GoReleaser, Docker, golangci-lint.

## Known Gaps

### Blocks Adoption

1. **Load-path resolution** — `(load "helper.scm")` resolves from CWD, not file directory
2. **External extensions** — Can't publish extensions to separate repos (see `EXTERNAL_EXTENSIONS_PLAN.md`)

### Growth

1. **Scheme test content** — Infrastructure exists, needs test files
2. **Performance benchmarks** — Need published results vs other Schemes
3. **External extension proof-of-concept** — No example yet

### Long-term

1. More SRFIs (currently SRFI-1 and SRFI-18 only)
2. Network libraries (TCP/UDP, HTTP)
3. POSIX API (SRFI-170)
4. LSP server
5. Package manager

## Dependencies

- `github.com/jessevdk/go-flags` — CLI flag parsing
- `golang.org/x/term` — REPL terminal support
- Go 1.23+, golangci-lint, GoReleaser (optional)
- Zero CGo dependencies
