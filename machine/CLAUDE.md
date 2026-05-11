# machine/ — VM, Compiler, Expander

## VM Operations

Two-tier dispatch: Most opcodes are inlined directly in the `Run()` switch (65 cases including promoted ops from opcode promotion Phases 1-3). `Operation` carries `OpKind() OpCode` declaring each operation's dispatch identity (its own opcode for direct-dispatch ops, `OpComplex` for side-table ops). `InlinedOperation` extends `Operation` with `Apply(*MachineContext) (*MachineContext, error)` for the side-table ops dispatched via `OpComplex` (16 ops: build-syntax, syntax-rules-transform, syntax-case, cont-mark, helpers, etc.).

Key ops: Push/Pop (stack), Apply (dispatch), CallForeignCached/CallForeignCachedTail (Go primitives), MakeClosure, LoadLocal/StoreLocal, PushLocal/PushCachedBinding (fused ops), BranchOnFalse, SaveContinuation/RestoreContinuation, PushWind/PopWind. Promoted ops: NullQ, PairQ, Car, Cdr, Add, Sub, Mul, Div, Cons, numeric comparisons, EqQ, VectorQ, VectorRef (each with tail variants).

## Extensions

Primitives: `registry/core/prim_*.go`. Signature: `func(CallContext) error` (type: `machine.ForeignFunction`).

`CallContext` interface (`call_context.go`) exposes 7 methods: `Arg`, `SetValue`, `SetValues`, `Authorizer`, `Context`, `EnvironmentFrame`, `Thread`. Primitives needing full VM access (sub-contexts, continuations, exception handling) type-assert to `*MachineContext`.

Register: `r.AddPrimitive(PrimitiveSpec{Name, ParamCount, IsVariadic, Impl}, Phase)`. Phases: `PhaseRuntime | PhaseExpand | PhaseCompile`.

Extension interface: `Name() string` + `AddToRegistry(*Registry) error`.

## Compile-Time Code

Compile-time/macro code uses `*Pair` only. The `Tuple` interface is for runtime read-only operations.
