# machine/ — VM, Compiler, Expander

## VM Operations

`Operation.Apply(context.Context, *MachineContext) (*MachineContext, error)` — stored in `NativeTemplate.Operations`, PC-indexed.

Key ops: Push/Pop (stack), Apply (dispatch), ForeignFunctionCall (Go primitives), MakeClosure, LoadLocal/StoreLocal, LoadGlobal/StoreGlobal, BranchOnFalse/BranchOnNotFalse, SaveContinuation/RestoreContinuation, PushWind/PopWind.

## Extensions

Primitives: `registry/core/prim_*.go`. Signature: `func(context.Context, *MachineContext) (*MachineContext, error)`.

Register: `r.AddPrimitive(PrimitiveSpec{Name, ParamCount (-1=variadic), IsVariadic, Impl}, Phase)`. Phases: `PhaseRuntime | PhaseExpand | PhaseCompile`.

Extension interface: `Name() string` + `AddToRegistry(*Registry) error`.

## Compile-Time Code

Compile-time/macro code uses `*Pair` only (no `ArrayList` at those phases). The `Tuple` interface is for runtime read-only operations.
