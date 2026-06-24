# Table of Contents

Every document in `docs/`, organized by topic.

## Algebra

- [overview.md](algebra/overview.md) -- Introduction, design philosophy, structure hierarchy, learning path
- [reference.md](algebra/reference.md) -- Complete API reference for all structures, projections, rewriting, symbolic ops
- [tutorial.md](algebra/tutorial.md) -- Runnable, self-verifying tutorial (11 deep chapters + 9 quick-tour files); integrated into `make tutorial-test`

## Compiler

- [macro-system.md](compiler/macro-system.md) -- Macro system design (three-layer architecture, sets of scopes)
- [peephole-optimizer.md](compiler/peephole-optimizer.md) -- Superinstruction formation, 3-pass pipeline, promoted opcodes
- [core-let.md](compiler/core-let.md) -- Core `let` as a compiler-recognized form
- [inlining.md](compiler/inlining.md) -- Procedure inlining after core `let`
- [ssa.md](compiler/ssa.md) -- Would SSA help the Wile compiler?
- [anf-and-cps.md](compiler/anf-and-cps.md) -- CPS and ANF as intermediate forms

## Continuations

- [concepts.md](continuations/concepts.md) -- What continuations are (general concept)
- [marks.md](continuations/marks.md) -- Continuation marks
- [implementation.md](continuations/implementation.md) -- How Wile implements continuations in a bytecode VM
- [delimited.md](continuations/delimited.md) -- Delimited continuations: prompts, abort, composable
- [escape-design.md](continuations/escape-design.md) -- First-class continuation escape mechanism
- [prompt-abort.md](continuations/prompt-abort.md) -- Prompt/abort system implementation details
- [optimizations.md](continuations/optimizations.md) -- Continuation-heavy workload optimizations
- [racket-primitives.md](continuations/racket-primitives.md) -- Racket's low-level control primitives

## Coverage

- [scheme-coverage.md](coverage/scheme-coverage.md) -- Scheme-side line coverage (`--cover`, `go tool cover`-compatible reports)

## Developer Guides

- [debug-methodology.md](dev/debug-methodology.md) -- Systematic debug logging methodology
- [foreign-closure-design.md](dev/foreign-closure-design.md) -- ForeignClosure design
- [iteration-idioms.md](dev/iteration-idioms.md) -- Four iteration shapes and when to use each
- [pooling.md](dev/pooling.md) -- Object pooling contract
- [project-board-setup.md](dev/project-board-setup.md) -- GitHub project board setup guide

## Embedding

- [api-design.md](embedding/api-design.md) -- Embedding API design (Engine, Value boundary, interop)
- [source-loading.md](embedding/source-loading.md) -- FileResolver chain, embedded stdlib, library import resolution
- [mcp.md](embedding/mcp.md) -- MCP server

## Environment

- [system.md](environment/system.md) -- Environment system architecture
- [diagram.md](environment/diagram.md) -- Environment relationship diagram
- [racket-namespaces.md](environment/racket-namespaces.md) -- Racket namespaces (comparative reference)

## Extensions

- [architecture.md](extensions/architecture.md) -- Extension system architecture and authoring guide
- [libraries.md](extensions/libraries.md) -- R7RS library integration for extensions

## Learn

- [macro-system.md](learn/macro-system.md) -- How hygienic macros work in Wile (pedagogical companion to [compiler/macro-system.md](compiler/macro-system.md))
- [scheme-debugging-primitives.md](learn/scheme-debugging-primitives.md) -- The load-bearing primitives of Scheme debugging
- [python-vs-scheme-for-algebra.md](learn/python-vs-scheme-for-algebra.md) -- Why Scheme fits symbolic algebra

## Numeric

- [tower.md](numeric/tower.md) -- Numeric tower architecture
- [precision-guarantees.md](numeric/precision-guarantees.md) -- Precision guarantees and tier model
- [nan-boxing.md](numeric/nan-boxing.md) -- NaN-boxing (educational)

## Reference

- [scheme.md](reference/scheme.md) -- Complete Wile Scheme language reference
- [cli-and-repl.md](reference/cli-and-repl.md) -- `wile` command-line flags, REPL meta commands, and debugger commands
- [r7rs-differences.md](reference/r7rs-differences.md) -- Documented differences from R7RS
- [implementation-notes.md](reference/implementation-notes.md) -- Implementation choices that differ from canonical approaches

## Security

- [sandboxing.md](security/sandboxing.md) -- Capability-based sandboxing model
- [blog-sandboxing.md](security/blog-sandboxing.md) -- Blog post: sandboxing in practice

## Types

- [records-as-formal-types.md](types/records-as-formal-types.md) -- Records as formal types (introduction/elimination rules)
- [abstract-data-types.md](types/abstract-data-types.md) -- Abstract data types and existential types
- [scheme-types-records-mop.md](types/scheme-types-records-mop.md) -- Scheme types, records, and the meta-object question
- [racket-structs.md](types/racket-structs.md) -- Racket structs (comparative reference)
