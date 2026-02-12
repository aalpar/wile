# Wile Project Status

**Last Updated**: 2026-02-12

**Current Version**: v1.2.0 (released 2026-02-11)

This document provides a comprehensive overview of what exists in the Wile project vs what's planned.

---

## Core Implementation Status

### Language Features (R7RS-small)

| Feature | Status | Notes |
|---------|--------|-------|
| **Lexical scoping** | ✅ Complete | |
| **Hygienic macros** | ✅ Complete | syntax-rules, syntax-case, Flatt 2016 model |
| **First-class continuations** | ✅ Complete | call/cc, dynamic-wind |
| **Delimited continuations** | ✅ Complete | prompts, abort, composable |
| **Numeric tower** | ✅ Complete | Integer, Rational, Float, Complex, BigInteger, BigFloat |
| **Proper tail calls** | ✅ Complete | |
| **Records** | ✅ Complete | define-record-type (SRFI-9) |
| **Exceptions** | ✅ Complete | guard, raise, with-exception-handler |
| **Libraries** | ✅ Complete | define-library, import, export |
| **Promises** | ✅ Complete | delay, force, lazy evaluation |

### Runtime Features

| Feature | Status | Notes |
|---------|--------|-------|
| **Bytecode compiler** | ✅ Complete | |
| **Stack-based VM** | ✅ Complete | |
| **SRFI-18 threading** | ✅ Complete | Threads, mutexes, condition variables |
| **Go concurrency interop** | ✅ Complete | Channels, WaitGroups, RWMutex, Once, Atomic |
| **Pure Go** | ✅ Complete | No CGo dependencies |
| **Source tracking** | ✅ Complete | Line/column info in errors |

---

## Standard Libraries

### R7RS Standard Libraries

| Library | Status | Notes |
|---------|--------|-------|
| `(scheme base)` | ✅ Complete | Core language |
| `(scheme case-lambda)` | ✅ Complete | |
| `(scheme char)` | ✅ Complete | Character predicates |
| `(scheme complex)` | ✅ Complete | Complex operations |
| `(scheme cxr)` | ✅ Complete | car/cdr compositions |
| `(scheme eval)` | ✅ Complete | eval, environment |
| `(scheme file)` | ✅ Complete | File I/O |
| `(scheme inexact)` | ✅ Complete | Transcendental functions |
| `(scheme lazy)` | ✅ Complete | Promises |
| `(scheme load)` | ✅ Complete | load |
| `(scheme read)` | ✅ Complete | read |
| `(scheme write)` | ✅ Complete | write, display |
| `(scheme repl)` | ✅ Complete | interaction-environment |
| `(scheme process-context)` | ✅ Complete | command-line, exit |
| `(scheme time)` | ✅ Complete | current-second, jiffies |
| `(scheme r5rs)` | ✅ Complete | R5RS compatibility |

### Additional Libraries

| Library | Status | Files | Notes |
|---------|--------|-------|-------|
| `(srfi 1)` | ✅ Complete | 8 modules | List library (constructors, fold, predicates, etc.) |
| `(srfi 18)` | ✅ Complete | Built-in | Multithreading |
| `(chibi test)` | ✅ Complete | `lib/chibi/test.scm` | Test framework |
| `(chibi optional)` | ✅ Complete | `lib/chibi/optional.scm` | Optional arguments |
| `(chibi diff)` | ✅ Complete | `lib/chibi/diff.scm` | Diff utility |
| `(chibi term ansi)` | ✅ Complete | `lib/chibi/term/ansi.scm` | ANSI terminal |

---

## Extensions (Internal)

Located in `internal/extensions/`:

| Extension | Status | Purpose |
|-----------|--------|---------|
| **io** | ✅ Complete | Port operations, current-input/output-port parameters |
| **files** | ✅ Complete | File operations, directory operations |
| **math** | ✅ Complete | Extended math primitives |
| **system** | ✅ Complete | System interaction, environment variables |
| **threads** | ✅ Complete | SRFI-18 threading primitives |
| **eval** | ✅ Complete | eval, environment, load |
| **exceptions** | ✅ Complete | Exception handling |
| **gointerop** | ✅ Complete | Go FFI (RegisterFunc, channels, etc.) |
| **all** | ✅ Complete | Meta-extension loading all others |

**Note**: Extensions are currently internal. Making them public is a P1 item for v2.0.0.

---

## Examples & Documentation

### Examples Directory

| Category | Files | Status | Notes |
|----------|-------|--------|-------|
| **basics/** | 6 | ✅ Complete | hello, recursion, higher-order, closures, error-tracking, meta-eval |
| **numeric-tower/** | 6 | ✅ Complete | exactness, rationals, complex, big-numbers, mixed, symbolic-diff |
| **macros/** | 4 | ✅ Complete | simple, hygiene, anaphoric, state-machine |
| **control/** | 6 | ✅ Complete | continuations, dynamic-wind, exceptions, generators, coroutines, amb |
| **concurrency/** | 5 | ✅ Complete | threads, mutex, channels, producers-consumers, parallel-map |
| **data-structures/** | 5 | ✅ Complete | records, alists, vectors, lazy-streams, unification |
| **io/** | 3 | ✅ Complete | file-io, string-ports, binary-io |
| **applications/** | 2 | ✅ Complete | parser-combinators, rule-engine |
| **logic/schelog/** | ~15 | ✅ Complete | Full Prolog DSL implementation |
| **benchmarks/** | 19 | ✅ Complete | Gabriel suite (tak, fib, ackermann, sieve, etc.) |
| **embedding/** | 2 | ✅ Complete | basic.go, source-tracking/ |
| **lib/** | 1 | ✅ Complete | r6rs-compat.scm |
| **Total** | ~74 | ✅ Complete | Exceeds plan by 90% |

### Documentation

| Document | Status | Notes |
|----------|--------|-------|
| **README.md** | ✅ Current | Quick Start, Key Features, Installation |
| **PRIMITIVES.md** | ✅ Current | Complete primitives reference |
| **CHANGELOG.md** | ✅ Current | v1.2.0 released |
| **examples/README.md** | ✅ Current | Comprehensive index with learning paths |
| **docs/design/** | ✅ Current | Design documents for major features |
| **docs/dev/** | ✅ Current | Architecture and development guides |
| **CLAUDE.md files** | ✅ Current | Package-level context for AI assistance |

---

## Test Infrastructure

### Go Tests

| Package | Test Files | Coverage | Status |
|---------|-----------|----------|--------|
| Root (`wile`) | 6 | ~80% | ✅ Good |
| `values/` | ~30 | ~80% | ✅ Good |
| `environment/` | ~10 | ~80% | ✅ Good |
| `machine/` | ~15 | ~80% | ✅ Good |
| `registry/core/` | ~20 | ~80% | ✅ Good |
| `internal/` packages | ~30 | ~80% | ✅ Good |
| **Total** | ~111 | ~80% | ✅ Good |

### Scheme Tests

| Component | Status | Notes |
|-----------|--------|-------|
| **Infrastructure** | ✅ Complete | `test/` directory, runners, CI integration |
| **Test framework** | ✅ Complete | `(chibi test)` |
| **Test discovery** | ✅ Complete | Auto-finds `*-test.scm` in `test/` and `lib/` |
| **Cross-impl testing** | ✅ Complete | `compare-schemes.sh` |
| **Test content** | ⚠️ Minimal | Only 1 smoke test, needs comprehensive tests |

**Status**: Infrastructure complete, test content creation deferred.

---

## Tooling

| Tool | Status | Location | Notes |
|------|--------|----------|-------|
| **REPL** | ✅ Complete | `internal/repl/` | Readline, history, multi-line |
| **Debugger** | ✅ Complete | REPL integrated | Breakpoints, stepping, backtrace |
| **Makefile** | ✅ Complete | Root | build, test, lint, bench targets |
| **CI/CD** | ✅ Complete | `.github/workflows/` | Tests, lint, multi-platform builds |
| **GoReleaser** | ✅ Complete | `.goreleaser.yml` | Automated binary releases |
| **Docker** | ✅ Complete | `docker/` | Multi-platform container builds |
| **Linting** | ✅ Complete | `.golangci.yml` | golangci-lint configured |

---

## Priorities for Next Releases

### v1.3.0 (Next — ~1 week)

**Theme**: Documentation and polish

- [x] Examples complete (DONE)
- [x] Quick Start in README (DONE)
- [ ] Benchmarks documentation
- [ ] Verify all examples run
- [ ] Final CHANGELOG polish

**Estimated release**: 2026-02-15

### v1.4.0 (~3 weeks)

**Theme**: Fix file resolution

- [ ] Load-path stack implementation (9 phases)
- [ ] Relative path resolution for load/include/import
- [ ] Primitives: `current-load-path`, `current-load-directory`
- [ ] Engine API: `WithLoadPath`, `PushLoadPath`, `PopLoadPath`

**Estimated release**: 2026-03-05

### v2.0.0 (~12 weeks)

**Theme**: External extensions ecosystem

- [ ] EnvironmentAccess public interface
- [ ] Migrate internal extensions to new API
- [ ] Extract at least one extension to external repo
- [ ] Extension writing guide
- [ ] Beta period for API feedback

**Estimated release**: 2026-05-05

---

## Known Gaps

### Immediate (Block Adoption)

1. ✅ ~~Examples~~ — COMPLETE
2. **Load-path resolution** — `(load "helper.scm")` resolves from CWD, not file directory
3. **External extensions** — Can't publish extensions to separate repos

### Near-term (Growth)

1. **Scheme test content** — Infrastructure exists, needs test files
2. **Performance benchmarks** — Need published results vs Chez/Racket/Gambit
3. **External extension examples** — No proof-of-concept yet

### Long-term (Nice-to-Have)

1. **SRFI implementations** — Currently only SRFI-1 and SRFI-18
2. **Network libraries** — TCP/UDP, HTTP
3. **POSIX API (SRFI-170)** — Comprehensive OS access
4. **LSP server** — IDE integration
5. **Package manager** — R7RS library distribution

---

## Project Health Metrics

| Metric | Value | Status |
|--------|-------|--------|
| **Lines of Code** | ~50,000 | |
| **Go Packages** | 20 | |
| **Test Coverage** | ~80% | ✅ Good |
| **Go Test Files** | 111 | ✅ Good |
| **Scheme Test Files** | 1 | ⚠️ Needs work |
| **Examples** | 74 | ✅ Excellent |
| **Documentation Files** | 50+ | ✅ Excellent |
| **GitHub Stars** | New | 📊 TBD |
| **Production Users** | 0 | 📊 TBD |

---

## Dependencies

### Go Dependencies

- `github.com/jessevdk/go-flags` — Command-line flag parsing (GNU conventions)
- `golang.org/x/term` — Terminal support for REPL
- Standard library only otherwise

**Zero CGo dependencies** — Pure Go throughout.

### Build Tools

- Go 1.23 or later
- golangci-lint (for linting)
- GoReleaser (for releases, optional)

---

## Summary

**Strengths**:
- ✅ R7RS-small compliance complete
- ✅ Pure Go (no CGo pain)
- ✅ Comprehensive examples (70+)
- ✅ Strong Go test coverage (~80%)
- ✅ Good documentation
- ✅ Clean embedding API

**Work in Progress**:
- ⚠️ Scheme test content (infrastructure exists)
- ⚠️ Load-path resolution (planned for v1.4.0)
- ⚠️ External extensions (planned for v2.0.0)

**Long-term Goals**:
- 📝 SRFI ecosystem
- 📝 Network/POSIX libraries
- 📝 IDE tooling (LSP)
- 📝 Package manager

**Verdict**: **Production-ready for embedding** (v1.2.0). External extension ecosystem coming in v2.0.0 (Q2 2026).
