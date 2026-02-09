# Plan: Introduce TopLevelEnvironment

## Summary

Create a new `TopLevelEnvironment` type that owns symbol interning on a per-instance basis, enabling multiple independent Wile VMs in a single Go process.

## Goals

1. **Per-instance symbol interning**: Each Wile VM has its own symbol table
2. **Centralized phase management**: TopLevelEnvironment owns the PhaseRegistry
3. **Clear interning gates**: Parser and `string->symbol` are the defined entry points
4. **VM isolation**: Multiple TopLevelEnvironments operate independently

## Architecture

```
TopLevelEnvironment (per Wile VM instance)
├── symbolInterns: map[Symbol]*Symbol + mutex
├── syntaxInterns: map[Value]SyntaxValue + mutex
├── phases: *PhaseRegistry
├── libraryRegistry: any
└── runtime: *EnvironmentFrame (phase 0)

EnvironmentFrame (lexical scoping)
├── parent: *EnvironmentFrame (lexical chain)
├── local: *LocalEnvironmentFrame
├── global: *GlobalEnvironmentFrame
├── topLevel: *TopLevelEnvironment  ← NEW
└── phaseLevel: int

GlobalEnvironmentFrame (per-phase bindings)
├── keys: map[Symbol]int
├── bindings: []*Binding
└── topLevel: *TopLevelEnvironment  ← NEW (back-reference)
```

## Symbol Interning Gates

Only two entry points for symbols:

1. **Parser** (`parser.go:170`): `wrapSyntaxSymbol()` → `env.InternSymbol()`
2. **string->symbol** (`prim_strings.go:256`): `PrimStringToSymbol()` → `env.InternSymbol()`

All `InternSymbol()` calls delegate to `TopLevelEnvironment.InternSymbol()`.

## New File

**`go/environment/top_level_environment.go`**:
```go
type TopLevelEnvironment struct {
    symbolInterns   map[values.Symbol]*values.Symbol
    symbolInternsMu sync.RWMutex
    syntaxInterns   map[values.Value]syntax.SyntaxValue
    syntaxInternsMu sync.RWMutex
    phases          *PhaseRegistry
    libraryRegistry any
    runtime         *EnvironmentFrame
}

func NewTopLevelEnvironment() *TopLevelEnvironment
func (p *TopLevelEnvironment) InternSymbol(s *values.Symbol) *values.Symbol
func (p *TopLevelEnvironment) InternSyntax(k values.Value, v syntax.SyntaxValue) syntax.SyntaxValue
func (p *TopLevelEnvironment) Runtime() *EnvironmentFrame
func (p *TopLevelEnvironment) AtPhase(phase int) *EnvironmentFrame
func (p *TopLevelEnvironment) Expand() *EnvironmentFrame
func (p *TopLevelEnvironment) Compile() *EnvironmentFrame
```

## Files to Modify

| File | Changes |
|------|---------|
| `environment/top_level_environment.go` | **NEW** - TopLevelEnvironment type |
| `environment/environment_frame.go` | Add `topLevel *TopLevelEnvironment` field |
| `environment/global_environment_frame.go` | Add `topLevel` field, remove syntaxInterns |
| `environment/phase_registry.go` | Reference TopLevelEnvironment instead of EnvironmentFrame |
| `values/symbol_intern.go` | Deprecate global functions |
| `wile/engine.go` | Use `NewTopLevelEnvironment()` |
| `runtime/environment_tiny.go` | Update to create TopLevelEnvironment |

## Migration Strategy

**Phase 1**: Add TopLevelEnvironment alongside existing global interning
- Create TopLevelEnvironment type
- Add `topLevel` field to EnvironmentFrame and GlobalEnvironmentFrame
- Delegate `InternSymbol()` through topLevel
- Keep `values.InternSymbol()` working for backward compatibility

**Phase 2**: Update Engine and runtime
- `wile.Engine` creates TopLevelEnvironment
- Update registry application

**Phase 3**: Deprecate global interning
- Add deprecation comments to `values.InternSymbol()`
- Remove in future release

## Verification

1. `cd go && go test ./...` - all existing tests pass
2. Create test with two TopLevelEnvironments verifying symbol isolation
3. `go test -race ./...` - no race conditions
4. Run r7rs-tests.scm to verify R7RS compliance maintained
