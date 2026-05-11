# Phase 4 / Finding 6 — `Operation` interface gets `OpKind() OpCode`

Implementation plan for Phase 4 of `plans/2026-05-06-machine-structural-reduction.md`.
Parent finding: **Finding 6 — `Operation` interface is the empty contract**.

## Decision: Option (b)

Add a discriminator method `OpKind() OpCode` to the `Operation` interface
rather than deleting `Operation` and widening the variadic constructors to
`...values.Value` (Option (a)).

**Rationale**:
- Preserves the Go-level narrowing — anything passed to `AppendOperations`
  must declare its opcode at compile time. Removes the runtime
  type-assertion failure path in `AppendOperationsWithSource:251` for any
  compiler-built sequence.
- Materializes the implicit `Op* → OpCode` mapping currently buried in
  `operationToInstruction`'s type-switch. This is **dispatch-axis-as-data**
  per `plans/2026-05-08-dispatch-axis-as-data.md` (defunctionalization).
- Gives the `Operation` interface a real, enforced contract — Finding 6's
  exact remediation wording.

## Scope

| Item                | Count |
|---------------------|-------|
| Op* types touched   | ~30   |
| Files in `machine/` | ~10   |
| Files in `machine/compilation/` | 3 |
| LOC added           | ~120  |
| LOC removed         | ~0 (additive)  |

This is a pure refactor: no behavior change, no opcode-table change, no bytecode emission change.

## Phases

### Phase 1 — Extend the `Operation` interface

`machine/operation.go`:
```go
type Operation interface {
    values.Value
    OpKind() OpCode
}
```

`InlinedOperation` keeps its `Apply` method. Side-table-only ops will
report `OpKind() == OpComplex`.

### Phase 2 — Implement `OpKind()` on every Op* type

**Direct-dispatch ops** (return their dedicated opcode):
- `machine/operations_stack.go`: Push, Pop, Pull, Drop, PeekK
- `machine/operations_load_store.go`: LoadVoid, LoadLiteralByLiteralIndexImmediate, LoadGlobalByGlobalIndexLiteralIndexImmediate, LoadLocalByLocalIndexImmediate, LoadCachedBinding, StoreLocalByLocalIndexImmediate, StoreGlobalByGlobalIndexLiteralIndexImmediate
- `machine/operations_control.go`: BranchOffsetImmediate, BranchOnFalseValueOffsetImmediate, SaveContinuationOffsetImmediate, RestoreContinuation
- `machine/operations_call.go`: Apply, UnpackListToStack, ForeignFunctionCall (→ OpComplex)
- `machine/operations_closure.go`: MakeClosure (→ OpMakeClosure), MakeCaseLambdaClosure (→ OpComplex)
- `machine/operations_winding.go`: PushWind (→ OpComplex), PopWind (→ OpComplex), PopEnv (→ OpPopEnv), PushEnv (→ OpPushEnv)
- `machine/operation_cont_mark.go`: SetContMark, SaveContMark, RestoreContMark (all → OpComplex)
- `machine/compilation/operation_build_syntax.go`: BuildSyntaxList (→ OpComplex)
- `machine/compilation/operation_syntax_case.go`: SyntaxCaseMatch, BindPatternVars, SyntaxCaseNoMatch, SyntaxTemplateExpand, StoreSyntaxCaseInput, ClearSyntaxCaseInput (all → OpComplex)
- `machine/compilation/operation_syntax_rules_transform.go`: SyntaxRulesTransform (→ OpComplex)

Method body is one line: `return OpFoo`.

Add `var _ Operation = (*OperationFoo)(nil)` to each Op* type's file for compile-time enforcement (where not already present).

### Phase 3 — Wire `OpKind()` into `operationToInstruction`

Replace the per-op opcode encoding in the type switch with one default
dispatch on `op.OpKind()`. The operand-extraction logic still lives in
the type switch (each operand kind needs its own extractor). The switch
becomes:

```go
func operationToInstruction(op Operation) (Instruction, bool) {
    kind := op.OpKind()
    if kind == OpComplex {
        return Instruction{}, false
    }
    switch v := op.(type) {
    case *OperationLoadLiteralByLiteralIndexImmediate:
        return Instruction{Op: kind, Arg: int32(v.LiteralIndex)}, true
    // ... operand-extraction cases ...
    default:
        // zero-operand ops: just the opcode
        return Instruction{Op: kind}, true
    }
}
```

This collapses the ~24 zero-operand cases into one default branch and
preserves the operand-extraction cases for the ~12 operand-bearing ops.

### Phase 4 — Tests, lint, ci

- `go test ./machine/...`
- `make lint`
- `make covercheck`
- `make ci`

No new tests required — `OpKind()` is exercised by every existing
compile path. The compile-time `var _ Operation` assertions catch missing
implementations.

## Out of scope

- Deleting `InlinedOperation` (Apply contract still needed for OpComplex dispatch).
- Renaming `OpKind` → something else. The name matches Finding 6's wording.
- Adding a full `ToInstruction()` method that returns the encoded `Instruction`. That's a deeper refactor (replaces the entire type switch); record as a follow-up if interesting after this lands.
