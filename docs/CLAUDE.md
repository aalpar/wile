# docs/ — Documentation Conventions

## Documentation Notation

| Notation | Meaning | Example |
|----------|---------|---------|
| `<value>` | Required placeholder (user supplies) | `git commit -m "<message>"` |
| `[value]` | Optional element | `go test [-v] ./...` |
| `<value>...` | One or more of this element | `cat <file>...` |
| `[value]...` | Zero or more of this element | `rm [file]...` |
| `{a\|b}` | Required choice between alternatives | `git {push\|pull}` |
| `[a\|b]` | Optional choice between alternatives | `make [build\|test]` |
| `ALLCAPS` | Environment variable or constant | `$GOPATH`, `EOF` |
| `` `literal` `` | Exact text (use as-is) | `` `--verbose` `` |
| `→` | Maps to / becomes / produces | `foo.go → foo_test.go` |

**Escaping**: When angle brackets appear literally in commands (rare), escape as `\<` or quote the whole command.

**Combining**: `[--timeout <ms>]` means the flag is optional, but if provided, requires a value.

## R7RS Specification Comments

Functions implementing R7RS-specified behavior must include comments citing the relevant specification section. This ensures traceability and helps maintain conformance.

**Format**: Use `R7RS §X.Y.Z` notation in doc comments.

**Example**:
```go
// Add returns the sum of this integer and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
func (p *Integer) Add(o Number) Number {
```

**When to include R7RS citations**:
- Type definitions for Scheme value types (Integer, Pair, etc.)
- Arithmetic and comparison operations
- Type predicates and conversions (exact, inexact, integer?, etc.)
- Primitive procedure implementations
- Exactness preservation/contagion behavior
- Any behavior specified by R7RS sections 4-6

**Key R7RS sections**:
| Section | Topic |
|---------|-------|
| §4.1-4.3 | Expressions, syntax |
| §5.1-5.5 | Program structure, definitions |
| §6.1 | Equivalence predicates |
| §6.2 | Numbers (tower, exactness, operations) |
| §6.3 | Booleans, pairs, lists, symbols, characters, strings, vectors |
| §6.4 | Bytevectors |
| §6.5 | Control features |
| §6.6 | Exceptions |
| §6.7-6.13 | Environments, I/O, system interface |

## Citing Design and Implementation Influences

When a design choice or implementation technique is drawn from an external source, cite it in code comments. This includes algorithms, data structure choices, and semantic decisions influenced by other work.

**What to cite**:
- Algorithms or techniques adopted from other Scheme implementations (Chez Scheme, Racket, Guile, Chibi-Scheme, Gambit, MIT/GNU Scheme, CHICKEN, etc.)
- Academic papers and their specific contributions (e.g., "optimistic bisimilarity — Chez Scheme, Racket")
- Books (SICP, TSPL, Lisp in Small Pieces, R. Kent Dybvig's writings, etc.)
- SRFIs that inform the design beyond their specification text
- Web resources with substantive technical content (blog posts, implementation notes)

**Format**: Cite inline in doc comments, close to the code the influence applies to. Name the source and what was adopted.

**Examples**:
```go
// Uses optimistic bisimilarity with a visited set to terminate on
// circular structures per R7RS §6.1. This is the same technique
// used by Chez Scheme and Racket.

// Scope sets follow Flatt 2016 ("Binding as Sets of Scopes").

// Floyd's cycle detection (tortoise-and-hare) as used in Chibi-Scheme's
// proper-list check.
```

**Why**: Citing sources makes the codebase self-documenting about *why* things are done a certain way, not just *what* they do. It helps future contributors understand the provenance of design decisions and find the original material for deeper understanding.
