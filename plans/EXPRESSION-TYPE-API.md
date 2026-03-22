# Expression Type API Redesign

## Problem

The single-expression public APIs (`Eval`, `Compile`, `EvalIn`, `EvalWithSource`,
`CompileWithSource`) accept `string` but only process the first s-expression,
silently discarding any trailing input. This caused real bugs: test helpers passed
multi-expression code to `Eval`, which evaluated only the first expression (a
`define-syntax`) and never reached the use site. The tests "passed" because the
helper only checked for no-error.

A trailing-input error was added as a pragmatic fix (2026-03-21). This design
replaces it with a structural fix: make the type system enforce single-expression
input.

## Design

### New Type: Expression

An opaque wrapper in `wile/`, following the same pattern as `CompiledCode`
(which wraps `machine.NativeTemplate`):

```go
// Expression represents a parsed Scheme expression ready for compilation.
type Expression struct {
    stx    syntax.SyntaxValue
    source string
}
```

`Expression` is the output of parsing and the input to compilation/evaluation.
Since it holds exactly one parsed expression, there is no "trailing input" to
discard — the bug class is eliminated by construction.

### New Methods

```go
// Parse parses a single expression from code.
// Returns CompilationError if code contains zero or more than one expression.
func (p *Engine) Parse(ctx context.Context, code string) (*Expression, error)

// ParseWithSource is Parse with source attribution for error messages.
func (p *Engine) ParseWithSource(ctx context.Context, code, source string) (*Expression, error)

// MustParse is Parse that panics on error.
// Use for known-good string literals in tests and examples.
func (p *Engine) MustParse(ctx context.Context, code string) *Expression
```

`Parse` enforces "exactly one expression": read one `SyntaxValue`, then assert
EOF. This is where the trailing-input check lives — the only place that reads
from a raw string for single-expression use.

### Changed Signatures

| Before                                        | After                                  |
|-----------------------------------------------|----------------------------------------|
| `Eval(ctx, code string)`                      | `Eval(ctx, expr *Expression)`          |
| `EvalWithSource(ctx, code, source string)`    | removed (source is on `*Expression`)   |
| `EvalIn(ctx, code string, ns *Namespace)`     | `EvalIn(ctx, expr *Expression, ns)`    |
| `Compile(ctx, code string)`                   | `Compile(ctx, expr *Expression)`       |
| `CompileWithSource(ctx, code, source string)` | removed (source is on `*Expression`)   |

### Unchanged

| Method                                          | Reason                              |
|-------------------------------------------------|-------------------------------------|
| `EvalMultiple(ctx, code string)`                | Consumes all input by design        |
| `EvalMultipleWithSource(ctx, code, source str)` | Same                                |
| `Run(ctx, *CompiledCode)`                       | Already takes compiled, not string  |

### Error Handling

The trailing-input check added to `compile()` and `EvalIn()` on 2026-03-21 is
**removed** — it was the pragmatic fix; this is the structural replacement.

Error type boundaries remain clean:

| Method    | Error type        | When                              |
|-----------|-------------------|-----------------------------------|
| `Parse`   | `CompilationError` | Parse failure or trailing input   |
| `Eval`    | `CompilationError` | Expansion or compilation failure  |
| `Eval`    | `RuntimeError`     | Execution failure                 |
| `Compile` | `CompilationError` | Expansion or compilation failure  |
| `Run`     | `RuntimeError`     | Execution failure                 |

### Migration

Three caller groups:

**Internal engine code** (`engine.go`, `EvalIn`): Already parses then compiles
internally. Refactored to use `Parse` + `Eval`/`Compile` at the right boundary.

**Test/example string literals** (~80+ sites in wile, 10 in wile-goast, 4 in
wile-extension-example): Mechanical replacement — wrap in `MustParse` or switch
to `EvalMultiple`. `MustParse` keeps test code concise:

```go
// Before
result, err := engine.Eval(ctx, "(+ 1 2)")

// After
result, err := engine.Eval(ctx, engine.MustParse(ctx, "(+ 1 2)"))
```

**External consumers** (wile-goast, wile-extension-example): Same mechanical
change as tests. Main execution paths in wile-goast already use `EvalMultiple`.

### Files Changed

| File                        | Change                                             |
|-----------------------------|----------------------------------------------------|
| `wile/expression.go` (new) | `Expression`, `Parse`, `ParseWithSource`, `MustParse` |
| `wile/engine.go`           | Signature changes, remove internal `compile()`, remove trailing-input checks |
| `wile/doc.go`              | Update example                                     |
| `wile/*_test.go`           | Wrap in `MustParse` or switch to `EvalMultiple`    |
| `wile-goast` test helpers  | Same (5 files)                                     |
| `wile-extension-example`   | 4 call sites                                       |
| `README.md`, `examples/`   | Update code samples                                |

No changes to: `EvalMultiple`, `EvalMultipleWithSource`, `Run`, `CompiledCode`,
`values/`, `machine/`, `internal/`, `werr/`.
