# Wile Documentation

Documentation is organized by topic. Each directory collects design documents,
implementation notes, and educational articles for a single subsystem.

| Topic | Description |
|-------|-------------|
| [algebra/](algebra/) | Algebraic structures library -- composable records, equational rewriting, symbolic normalization. Entry points: [overview.md](algebra/overview.md), [reference.md](algebra/reference.md), runnable [tutorial.md](algebra/tutorial.md) |
| [compiler/](compiler/) | Compiler internals -- macro system, peephole optimizer, IR design (SSA, ANF/CPS, core-let, inlining) |
| [concurrency/](concurrency/) | SRFI-18 threads and Go-interop concurrency -- how blocking channel operations couple to VM cancellation (`with-timeout`, `thread-terminate!`) |
| [continuations/](continuations/) | Continuation system -- concepts, marks, VM implementation, delimited continuations, prompt/abort |
| [coverage/](coverage/) | Scheme-side line coverage -- `--cover` reports compatible with `go tool cover` |
| [dev/](dev/) | Developer guides -- debug methodology, iteration idioms, object pooling, foreign closures, project board |
| [embedding/](embedding/) | Embedding Wile in Go -- public API design, source loading (FileResolver), MCP server |
| [environment/](environment/) | Environment and namespace system -- binding scopes, phase hierarchy, environment diagram |
| [extensions/](extensions/) | Extension system -- architecture, authoring, R7RS library integration |
| [learn/](learn/) | Educational deep-dives -- how hygienic macros work, Scheme debugging primitives, Python vs Scheme for algebra |
| [numeric/](numeric/) | Numeric tower -- architecture, precision guarantees, NaN-boxing |
| [reference/](reference/) | Language reference -- Scheme language spec, CLI and REPL, R7RS differences, implementation notes |
| [security/](security/) | Sandboxing -- capability-based security, extension-level and fine-grained authorization |
| [types/](types/) | Type system -- records, abstract data types, existential types, Racket structs |

## See Also

- [`PRIMITIVES.md`](../PRIMITIVES.md) -- Complete reference of types and primitives
- [`BIBLIOGRAPHY.md`](../BIBLIOGRAPHY.md) -- Academic papers, specifications, canonical references
- [`CODING_STYLE.md`](../CODING_STYLE.md) -- Go code style guide
- [`TOC.md`](TOC.md) -- Flat listing of every document
