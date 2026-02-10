# Architectural Consolidation Plan

**Status:** Optional operation code generation remains. All other items complete or rejected.

---

## Operation Code Generation

**Risk: HIGH**
**Estimated savings: ~1,500-2,000 bytes**
**Files affected: 29 operation files**
**Status: Not started**

### Problem

29 operation files share boilerplate for type definition, constructor, `SchemeString`, `IsVoid`, and `EqualTo`. Only `Apply` is unique per operation. The `EqualTo` migration (Phase 6) already reduced each `EqualTo` to a one-liner, but the remaining per-file boilerplate is still ~20 lines.

**Current zero-field operation** (e.g., `operation_pop.go`):
```go
type OperationPop struct{}

func NewOperationPop() *OperationPop {
	return &OperationPop{}
}

func (*OperationPop) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	// ... unique logic ...
}

func (p *OperationPop) SchemeString() string {
	return "#<machine-operation-pop>"
}

func (p *OperationPop) IsVoid() bool {
	return p == nil
}

func (p *OperationPop) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPop)
	return sameType(p, v, ok)
}
```

**Current single-field operation** (e.g., `operation_branch_offset_immediate.go`):
```go
type OperationBranchOffsetImmediate struct {
	Offset int
}

func NewOperationBranchOffsetImmediate(offset int) *OperationBranchOffsetImmediate {
	return &OperationBranchOffsetImmediate{Offset: offset}
}

func (p *OperationBranchOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-branch-offset-immediate %d>", p.Offset)
}

func (p *OperationBranchOffsetImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationBranchOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBranchOffsetImmediate)
	return fieldMatches(p, v, ok, func(op *OperationBranchOffsetImmediate) int { return op.Offset })
}
```

### Proposed Solution: Code Generation

Create `machine/gen_operations_main.go` with `//go:generate` directive. Generator produces `operation_generated.go` containing all simple operations. Complex operations (`Apply`, `MakeClosure`, `SyntaxCase`) remain hand-written.

### Alternative: Embedding with Generics (Lower Risk)

Instead of code generation, use embedding:

```go
type zeroFieldOperation[T any] struct{}

func (p *zeroFieldOperation[T]) IsVoid() bool { return p == nil }
func (p *zeroFieldOperation[T]) EqualTo(o values.Value) bool {
    v, ok := o.(*T)
    return sameType(p, v, ok)
}
```

This saves less code (~60 lines) but carries lower risk.

### Impact Analysis

Boilerplate per operation (excluding Apply, copyright, imports):

| Category | Count | Boilerplate/file | Total boilerplate |
|----------|-------|------------------|-------------------|
| Zero-field | ~6 | ~14 lines | ~84 lines |
| Single-field | ~8 | ~17 lines | ~136 lines |
| Complex (multi-field, closures) | ~15 | varies | not candidates |
| **Candidates total** | **~14** | | **~220 lines** |

**Net savings with codegen**: ~220 lines boilerplate removed, ~120 lines generator added = ~100 lines net (~1,000 bytes)

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
| `machine/operation_helpers.go` | `sameType`, `fieldMatches`, `fieldMethodMatches`, `sliceMatches` |
| `registry/helpers/args.go` | `RequireArg[T]`, `RequireIndex`, `ParseOptionalStartEnd`, `ParseSubrange` |
