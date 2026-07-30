# Wile

[![CI](https://github.com/aalpar/wile/actions/workflows/ci.yml/badge.svg)](https://github.com/aalpar/wile/actions/workflows/ci.yml)
[![Go Reference](https://pkg.go.dev/badge/github.com/aalpar/wile.svg)](https://pkg.go.dev/github.com/aalpar/wile)

Wile is an R7RS-small Scheme interpreter written in pure Go. It is built to be
embedded: `go get` adds it to a Go project, and there is no CGo, no C
toolchain, and no cross-compilation friction. Scheme values are ordinary Go
heap objects collected by the Go garbage collector.

Wile targets a specific use case — adding a Lisp scripting layer to a Go
application where the workload benefits from Lisp semantics. That includes
configuration DSLs, policy evaluation, symbolic computation, and any
application where hygienic macros, exact arithmetic, or first-class
continuations are the right tool. It is not a replacement for Lua or
JavaScript on performance-bound scripting workloads.

The interpreter implements the R7RS-small language: hygienic macros via
Flatt's sets-of-scopes model, proper tail calls, first-class continuations,
the full numeric tower (exact integers, rationals, IEEE floats, arbitrary
precision, complex), and SRFI-18 threads. It can also be used as a standalone
interpreter via the `wile` command.

## Installation

Wile requires Go 1.24 or later. For a build free of known stdlib
vulnerabilities, use Go 1.26.4 or later (Go 1.26.3 and earlier carry reachable
stdlib CVEs); see [SECURITY.md](SECURITY.md#supported-versions).

### As a library

```bash
go get github.com/aalpar/wile@latest
```

Then import and use the public API; see
[`docs/embedding/api-design.md`](docs/embedding/api-design.md) for the full
embedding guide.

### As a standalone interpreter

Download a prebuilt binary from the [Releases](https://github.com/aalpar/wile/releases)
page, or build from source:

```bash
git clone https://github.com/aalpar/wile.git
cd wile
make build
```

The binary is written to `./dist/{os}/{arch}/wile`.

## Running

```bash
wile                                  # Start the REPL
wile program.scm                      # Run a file and exit
wile -f program.scm -i                # Run a file, then enter the REPL
wile -e '(+ 1 2)'                     # Evaluate an expression
wile --check program.scm              # Compile without running; report errors
wile -L /path/to/libs program.scm     # Add a library search path
wile --version                        # Print the version
```

`SCHEME_LIBRARY_PATH` (colon-separated) supplies additional library search
paths. The REPL supports readline-style editing, multi-line expressions, and
a built-in debugger:

```
> (define (fib n)
    (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
> (fib 10)
55
> ,doc map
> ,break program.scm:42
> ,continue
```

Meta commands begin with `,`. Use `,help` for the full list, or see
[`docs/reference/cli-and-repl.md`](docs/reference/cli-and-repl.md) for the
complete CLI flag set, meta commands, and debugger commands.

## Embedding

```go
import "github.com/aalpar/wile/pkg/wile"

engine, _ := wile.NewEngine(ctx)
engine.Define("width", wile.NewInteger(800))
result, _ := engine.Eval(ctx, engine.MustParse(ctx, "(* width 600)"))
fmt.Println(result.SchemeString())   // 480000
```

The full embedding API — value constructors, engine options, primitive
registration, profiles, sandboxing — is documented in
[`docs/embedding/api-design.md`](docs/embedding/api-design.md) and at
[pkg.go.dev/github.com/aalpar/wile/pkg/wile](https://pkg.go.dev/github.com/aalpar/wile/pkg/wile).
Worked examples live in [`examples/embedding/`](examples/embedding/).

### Asking for a cut

Wile is a full R7RS-small implementation, and its breadth can be more than an
embedder wants. The extension system is built for trimming: profiles
(`WithProfile`) and `WithExtension` opt into only the primitives you need, and
per-engine registries keep one engine's cut from affecting another. If your
application only wants, say, a configuration DSL or a policy evaluator, you can
run a much smaller surface than the default.

Reducing the linked *binary* size is a further step — it means splitting Wile
into separate modules so a build can exclude what it does not import. That work
is deferred until someone needs it, because the right split depends on the cut
being asked for. If binary size is blocking you from embedding Wile,
[open an issue](https://github.com/aalpar/wile/issues) describing the cut you
want and it can be scoped against your case.

## Documentation

| Topic | Document |
|-------|----------|
| Scheme language reference | [`docs/reference/scheme.md`](docs/reference/scheme.md) |
| CLI flags, REPL, debugger | [`docs/reference/cli-and-repl.md`](docs/reference/cli-and-repl.md) |
| Differences from R7RS | [`docs/reference/r7rs-differences.md`](docs/reference/r7rs-differences.md) |
| Embedding API | [`docs/embedding/api-design.md`](docs/embedding/api-design.md) |
| Embedded and virtual source loading | [`docs/embedding/source-loading.md`](docs/embedding/source-loading.md) |
| Extension system | [`docs/extensions/architecture.md`](docs/extensions/architecture.md) |
| R7RS library integration | [`docs/extensions/libraries.md`](docs/extensions/libraries.md) |
| Macro system and hygiene | [`docs/compiler/macro-system.md`](docs/compiler/macro-system.md) |
| Continuations | [`docs/continuations/concepts.md`](docs/continuations/concepts.md), [`delimited.md`](docs/continuations/delimited.md) |
| Numeric tower | [`docs/numeric/tower.md`](docs/numeric/tower.md) |
| Sandboxing and authorization | [`docs/security/sandboxing.md`](docs/security/sandboxing.md) |
| Algebra library | [`docs/algebra/overview.md`](docs/algebra/overview.md) |
| All documentation | [`docs/INDEX.md`](docs/INDEX.md), [`docs/TOC.md`](docs/TOC.md) |
| Primitives reference | [`PRIMITIVES.md`](PRIMITIVES.md) |
| Academic references | [`BIBLIOGRAPHY.md`](BIBLIOGRAPHY.md) |
| Release history | [`CHANGELOG.md`](CHANGELOG.md) |

Self-contained examples — basics, macros, numeric tower, concurrency,
control flow, logic programming, and embedding — live in
[`examples/`](examples/). Go static analysis extensions (AST, SSA, CFG,
call graph, lint) have been extracted to
[wile-goast](https://github.com/aalpar/wile-goast).

## References

- [Binding as Sets of Scopes](https://www.cs.utah.edu/plt/scope-sets/) — Flatt (2016)
- [R7RS-small](https://small.r7rs.org/) — Language specification
- [SRFI-18](https://srfi.schemers.org/srfi-18/) — Multithreading

## Contributing

Contributions are welcome. Useful areas:

- Documentation, examples, tutorials
- R7RS-small completeness and SRFI implementations
- Test coverage
- Targeted performance work and allocation reduction
- REPL, debugger, and tooling improvements

Browse [issues labeled `good-first-issue`](https://github.com/aalpar/wile/labels/good-first-issue)
or [help wanted](https://github.com/aalpar/wile/labels/help-wanted). See
[CONTRIBUTING.md](CONTRIBUTING.md) for the workflow.

## License

Apache License 2.0 — see [LICENSE](LICENSE).
