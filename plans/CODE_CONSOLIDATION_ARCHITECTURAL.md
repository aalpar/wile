# Architectural Consolidation Plan

**Status:** Optional operation code generation remains. All other items complete or rejected.

---

## Operation Code Generation

**Risk: HIGH**
**Estimated savings: ~2,000-3,000 bytes**
**Files affected: 20+ operation files**
**Status: Not started**

### Problem

36 operation files follow repetitive patterns. Example categories:

**Zero-field operations** (6 files, ~270 lines total):
- `operation_pop.go`, `operation_pop_all.go`, `operation_push.go`
- `operation_brk.go`, `operation_pull.go`, `operation_drop.go`

Each has identical boilerplate:
```go
type OperationXxx struct{}
func NewOperationXxx() *OperationXxx { return &OperationXxx{} }
func (p *OperationXxx) SchemeString() string { return "#<machine-operation-xxx>" }
func (p *OperationXxx) IsVoid() bool { return p == nil }
func (p *OperationXxx) EqualTo(o values.Value) bool { /* boilerplate */ }
func (p *OperationXxx) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) { /* unique */ }
```

**Single-field operations** (8+ files, ~400 lines total):
- `operation_branch_offset_immediate.go`
- `operation_branch_on_false_offset_immediate.go`
- `operation_load_literal_integer.go`
- etc.

### Proposed Solution: Code Generation

Create `machine/gen_operations_main.go` with `//go:generate` directive. Generator produces `operation_generated.go` containing all simple operations. Complex operations (`Apply`, `MakeClosure`, `SyntaxCase`) remain hand-written.

### Alternative: Embedding with Generics (Lower Risk)

Instead of code generation, use embedding:

```go
type zeroFieldOperation[T any] struct{}

func (p *zeroFieldOperation[T]) IsVoid() bool { return p == nil }
func (p *zeroFieldOperation[T]) EqualTo(o values.Value) bool {
    return sameType[*T](o)
}
```

This saves less code (~100 lines) but carries lower risk.

### Impact Analysis

| Category | Files | Lines Removed | Generated Lines |
|----------|-------|---------------|-----------------|
| Zero-field | 6 | 273 | 180 (in one file) |
| Single-field (simple) | 5 | 255 | 150 (in one file) |
| Branch variants | 3 | 161 | 90 (parameterized) |
| **Total** | **14** | **689** | **420** |

**Net savings**: ~270 lines (~2,700 bytes)

### Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Generated code harder to debug | Keep Apply bodies readable; good comments |
| Build dependency on generator | Simple Go code, easy to maintain |
| Complex operations don't fit template | Keep complex ops as manual files |

---

## Existing Infrastructure

| File | What's There |
|------|-------------|
| `machine/operation_helpers.go` | `sameType[T]`, `fieldMatches[T, Op]` |
| `registry/helpers/args.go` | `RequireArg[T]`, `RequireIndex`, `ParseOptionalStartEnd`, `ParseSubrange` |
