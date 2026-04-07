# Public REPL API Design

**Date:** 2026-04-07
**Status:** Approved
**Goal:** Make REPL functionality available to external embedders as independently composable components.

## Summary

Move the REPL implementation from `internal/repl/` to a new public package `repl/`
(`github.com/aalpar/wile/repl`). Rewrite the compile/run path to use the public
Engine API instead of reaching into `internal/parser` and `machine/compilation`
directly. Delete `internal/repl/` after migration.

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| API model | Engine-centric (`*Engine`) | Embedders already have an Engine; avoids leaking internal types |
| Package location | `wile/repl/` | Idiomatic Go, clean import path, keeps Engine API focused |
| Code strategy | Move, not wrap | Engine-centric rewrite eliminates the internal imports that justified `internal/` |
| Doc interfaces | Stay in `repl/` | Only used by REPL features; extract later if needed (YAGNI) |
| Component granularity | All six independently constructible | MCP server already uses MetaCommandHandler standalone |
| Parse handoff | Engine.ReadExpression (reader-based) | Avoids redundant re-parse of already-accumulated input |

## Engine API Additions

Three additions to the `wile` package:

```go
// ReadExpression reads a single complete expression from r.
// Returns the parsed expression, or an error if parsing fails.
// Use IsIncompleteInput(err) to distinguish incomplete input from real errors.
func (p *Engine) ReadExpression(ctx context.Context, r io.Reader) (*Expression, error)

// IsIncompleteInput reports whether a parse error indicates the input
// is a valid prefix of an expression that needs more input to complete.
func IsIncompleteInput(err error) bool

// SetDebugger attaches a debugger to the engine. Subsequent Run calls
// will execute with the debugger active. Pass nil to detach.
func (p *Engine) SetDebugger(d *machine.Debugger)
```

**ReadExpression** wraps the internal parser, reading from an `io.Reader` and
returning the first complete expression. The REPL wraps accumulated input in
a `strings.Reader` and calls this.

**IsIncompleteInput** checks for EOF, "unexpected EOF", "unterminated", "unclosed"
in parse errors. Lives in the `wile` package (not `repl/`) because it is a
property of parse errors, not REPL-specific.

**SetDebugger** stores the debugger on the Engine. `Engine.Run` attaches it to
the MachineContext before execution. Session-scoped, not per-run.

## Package Structure

```
repl/
  repl.go                    REPL type, loop, options
  meta.go                    MetaCommandHandler (standalone)
  completer.go               Completer (standalone)
  debug.go                   DebugContext (standalone, wraps machine.Debugger)
  doc.go                     Package doc comment
  doc_provider.go            DocProvider, DocSearchProvider interfaces, DocInfo, DocSearchResult
  registry_doc_provider.go   RegistryDocProvider adapter
  pager.go                   writeWithPager helper (unexported)
```

Plus corresponding `_test.go` files.

## Public Types and Constructors

### REPL

```go
type REPL struct { /* unexported fields */ }

func New(eng *wile.Engine, opts ...Option) *REPL
func (p *REPL) Run(ctx context.Context) error
func (p *REPL) RunSimple(ctx context.Context) error
func (p *REPL) Debugger() *machine.Debugger
```

### Options

```go
type Option func(*REPL)

func WithHistoryFile(path string) Option
func WithPrompt(prompt string) Option
func WithContinuationPrompt(prompt string) Option
func WithOutput(w io.Writer) Option
func WithErrorOutput(w io.Writer) Option
func WithDocProvider(dp DocProvider) Option
func WithDebugContext(dc *DebugContext) Option
func WithCompleter(c *Completer) Option
```

`New` creates default instances for any component not provided via options.

### MetaCommandHandler

```go
type MetaCommandHandler struct { /* unexported fields */ }

type MetaOption func(*MetaCommandHandler)
func WithMetaDocProvider(dp DocProvider) MetaOption

func NewMetaCommandHandler(eng *wile.Engine, opts ...MetaOption) *MetaCommandHandler
func (p *MetaCommandHandler) Handle(line string, out io.Writer) bool
func (p *MetaCommandHandler) Commands() []string
func (p *MetaCommandHandler) SetPager(pager string)
func (p *MetaCommandHandler) DisassembleBinding(name string) (string, error)
```

Takes `*wile.Engine` instead of `*environment.EnvironmentFrame`. Calls
`eng.Environment()`, `eng.Namespace()` as needed.

### Completer

```go
type Completer struct { /* unexported fields */ }

func NewCompleter(eng *wile.Engine, metaCommands []string) *Completer
func (p *Completer) Do(line []rune, pos int) ([][]rune, int)
func (p *Completer) BindingNames() []string
```

Implements `readline.AutoCompleter`. Takes `*wile.Engine` instead of
`*environment.EnvironmentFrame`.

### DebugContext

```go
type DebugContext struct { /* unexported fields */ }

func NewDebugContext() *DebugContext
func (p *DebugContext) Debugger() *machine.Debugger
func (p *DebugContext) SetCurrentMC(mc *machine.MachineContext)
func (p *DebugContext) HandleDebugCommand(line string, out io.Writer) bool
func (p *DebugContext) DebugCommands() []DebugCommandInfo
```

No Engine dependency. Wraps `machine.Debugger`.

### Doc System

```go
type DocInfo struct {
    Doc        string
    Syntax     string
    TypeLabel  string
    ParamNames []string
    Category   string
    ParamCount int
    IsVariadic bool
    ParamTypes []values.ValueType
    ReturnType values.ValueType
}

type DocProvider interface {
    LookupDoc(name string) (info DocInfo, found bool)
}

type DocSearchResult struct {
    Name     string
    Doc      string
    Category string
}

type DocSearchProvider interface {
    DocProvider
    Search(pattern string) []DocSearchResult
    Categories() []string
    ByCategory(category string) []DocSearchResult
}

type RegistryDocProvider struct { /* unexported fields */ }

func NewRegistryDocProvider(reg *registry.Registry) *RegistryDocProvider
```

`RegistryDocProvider` implements both `DocProvider` and `DocSearchProvider`.

## Component Independence

No component requires any other component to function. The REPL composes them;
embedders pick what they need.

### Standalone usage examples

**Completer alone** (custom IDE integration):
```go
eng, _ := wile.NewEngine(ctx)
completer := repl.NewCompleter(eng, nil)
names := completer.BindingNames()
```

**MetaCommandHandler alone** (MCP server):
```go
eng, _ := wile.NewEngine(ctx)
docProv := repl.NewRegistryDocProvider(eng.Registry())
meta := repl.NewMetaCommandHandler(eng, repl.WithMetaDocProvider(docProv))
meta.SetPager("")
meta.Handle(",doc map", os.Stdout)
```

**DebugContext alone** (programmatic debugging):
```go
dbg := repl.NewDebugContext()
dbg.Debugger().SetBreakpoint("file.scm", 10, 0)
eng.SetDebugger(dbg.Debugger())
```

**DocProvider alone** (documentation tooling):
```go
docProv := repl.NewRegistryDocProvider(eng.Registry())
info, found := docProv.LookupDoc("map")
results := docProv.Search("list")
```

**Full REPL** (batteries included):
```go
eng, _ := wile.NewEngine(ctx, wile.WithAllExtensions())
docProv := repl.NewRegistryDocProvider(eng.Registry())
r := repl.New(eng, repl.WithDocProvider(docProv))
r.Run(ctx)
```

**Full REPL** (customized):
```go
eng, _ := wile.NewEngine(ctx, wile.WithAllExtensions())
docProv := repl.NewRegistryDocProvider(eng.Registry())
dbg := repl.NewDebugContext()
meta := repl.NewMetaCommandHandler(eng, repl.WithMetaDocProvider(docProv))
completer := repl.NewCompleter(eng, meta.Commands())
r := repl.New(eng,
    repl.WithDocProvider(docProv),
    repl.WithDebugContext(dbg),
    repl.WithCompleter(completer),
    repl.WithPrompt("wile> "),
    repl.WithOutput(myWriter),
)
r.Run(ctx)
```

## Migration

### cmd/wile/main.go

```go
// Before:
import "github.com/aalpar/wile/internal/repl"

// After:
import "github.com/aalpar/wile/repl"
```

`runREPL` switches from `repl.New(eng.Environment(), ...)` to
`repl.New(eng, ...)`. Otherwise identical.

### cmd/wile/mcp.go

Same import switch. `MetaCommandHandler` construction switches from
`repl.NewMetaCommandHandler(eng.Environment(), nil, docProv)` to
`repl.NewMetaCommandHandler(eng, repl.WithMetaDocProvider(docProv))`.

### internal/repl/

Deleted after migration. All code moves to `repl/` with the Engine-centric
rewrite. The `compile` and `run` helper functions are replaced by
`Engine.Compile` and `Engine.Run` calls.

## REPL Loop Rewrite

The core read-eval-print loop changes from:

```go
// Old (internal): direct parser + compilation APIs
parser := parser.NewParser(p.env, true, rdr)
stx, parseErr := parser.ReadSyntax(ctx)
tpl, compileErr := compile(ctx, p.env, stx)
mv, runErr := run(ctx, tpl, p.env)
```

To:

```go
// New (public): Engine API
expr, parseErr := p.eng.ReadExpression(ctx, rdr)
cc, compileErr := p.eng.Compile(ctx, expr)
mv, runErr := p.eng.Run(ctx, cc)
```

The `isIncompleteInput` check becomes `wile.IsIncompleteInput(parseErr)`.

Debugger attachment moves from per-run `mc.SetDebugger()` to session-scoped
`eng.SetDebugger()` — the Engine attaches it to every MachineContext it creates.

## Non-Goals

- Exposing `internal/parser` or `internal/syntax` publicly
- Changing the REPL's user-facing behavior (commands, prompts, keybindings)
- Adding new REPL features (this is a pure API extraction)
- Moving `DocProvider` to a separate package (YAGNI; extract later if needed)
