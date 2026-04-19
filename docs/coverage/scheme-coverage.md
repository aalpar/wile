# Scheme-Side Line Coverage

`wile --cover PATH` records which sub-expressions of your Scheme code
executed during a run, and writes a coverage report compatible with
`go tool cover -html`. For human-readable per-line output, use
`--cover-summary PATH`.

## Quick start

```bash
wile --cover cov.out --file myapp.scm
go tool cover -html=cov.out -o cov.html
open cov.html
```

## What is covered

Each `(file, start-line, start-col, end-line, end-col)` corresponding
to a compiled sub-expression (`SourceContext`) gets one entry.
Line-level coverage falls out as "any column on this line was covered."

Coverage is recorded at the bytecode level — after macro expansion.
You measure what actually ran, not what was textually written. A
macro that expands to no-op bytecode will show no entries; a macro
that expands to code appearing on a line you didn't write directly
will show entries attributed to the macro's source location.

## Per-line summary output

`wile --cover-summary cov.txt --file myapp.scm` produces:

```
myapp.scm:12  3/5 covered  max_col_reached=27
myapp.scm:15  0/2 covered  max_col_reached=0
TOTAL  3/7 sexprs covered
```

- `N/M`: distinct sub-expressions covered vs. total on the line.
- `max_col_reached`: rightmost start column of any covered
  sub-expression. Under straight sequential code (`(begin a b c)`),
  this scalar tells you "how deep into the line we got." Under
  branches (`if`/`cond`), a high `max_col_reached` with a low `N/M`
  means execution hit a later sub-expression but skipped one in the
  middle — typically a dead branch.

## Stdlib exclusion

By default, entries from the embedded stdlib (paths starting with
`scheme/`, `wile/`, or `srfi/`) are excluded. Pass `--cover-stdlib`
to include them in the output — useful when debugging stdlib
interactions, noisy otherwise.

## Limitations

- **Peephole fusion** may drop source attribution from some
  synthesized instructions; those PCs execute but produce no entry.
- **Constant folding** evaluates branches of `(if #t …)` /
  `(if #f …)` at compile time and emits bytecode only for the
  taken branch. The dead branch will not appear in coverage
  because no bytecode was emitted for it.
- **Coverage mode is `set`** — entries are 0 or 1, not a hit count.
  A `count` mode is a plausible future extension.
- **Coverage is opt-in.** With no `--cover` flag, the VM dispatch
  loop runs its regular path. The hook is a single predictable
  nil-branch in the hot loop when coverage is off; no measurable
  effect on throughput.

## Embedding API

For users of the `wile` package:

```go
import (
    "context"

    "github.com/aalpar/wile"
    "github.com/aalpar/wile/coverage"
)

col := coverage.NewCollector()
eng, _ := wile.NewEngine(context.Background(), wile.WithCoverage(col))

// ... compile and run Scheme ...

_ = coverage.WriteGoCover(os.Stdout, col)
// Or: coverage.WriteSummary(os.Stdout, col)
```

The collector is thread-safe for `Track` and `Entries` calls. Every
`*NativeTemplate` compiled by the engine — the top-level expression
plus every nested sub-template reachable via its literals pool
(lambda bodies, `define`'d procedures, etc.) — is automatically
registered.
