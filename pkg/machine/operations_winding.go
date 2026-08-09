// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package machine

import (
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// --- PushWind ---

// OperationPushWind creates a dynamic-wind frame and pushes it onto the winding stack.
// It expects the stack to contain [before, thunk, after] where:
//   - before is at PeekK(2) - the before thunk
//   - thunk is at PeekK(1) - the main thunk (not used by this operation)
//   - after is at PeekK(0) - the after thunk
//
// The frame is created from the before and after closures and pushed onto the
// winding stack. The stack is not modified.
//
// R7RS §6.10: dynamic-wind establishes a dynamic extent during which the before
// and after thunks are called whenever control enters or exits.
type OperationPushWind struct {
	OperationBase
}

func NewOperationPushWind() *OperationPushWind {
	return &OperationPushWind{
		OperationBase: NewOperationBase("machine-operation-push-wind"),
	}
}

func (*OperationPushWind) Apply(mc *MachineContext) (*MachineContext, error) {
	// `before` is not re-checked here. compileDynamicWind emits phase 2 (apply
	// `before` via OpApply) before phase 3 (this op), so a value that reaches
	// depth 2 has already survived ApplyCallable's dispatch — and all six types
	// it dispatches implement values.Callable. The old `.(Closure)` assertion
	// was reachable only because Closure is narrower than what ApplyCallable
	// accepts; widening it to values.Callable makes the branch unreachable, so
	// it is gone rather than left as dead code. A non-procedure in `before`
	// still fails, one phase earlier, with ApplyCallable's own diagnostic.
	before, ok := mc.evals.PeekK(2).(values.Callable)
	if !ok {
		err := mc.WrapError(werr.ErrInternal,
			"push-wind: before survived OpApply but is not callable")
		return mc, err
	}

	// `after`, by contrast, is checked here and not applied until phase 6, so
	// this is the only boundary it crosses — and the only operand position whose
	// diagnostic does not already come from ApplyCallable. Bridge it: this is an
	// argument-domain fault (R7RS §6.10 requires a procedure), so it belongs to
	// the Scheme program and must be reachable by guard / with-exception-handler.
	// The bridge goes HERE and not at Run()'s OpComplex arm, which dispatches all
	// nine side-table operations: blanket-bridging there would also make
	// OperationPopWind's werr.ErrInternal (below) guard-able, which
	// CODING_STYLE.md forbids for impossible states. Pinned by
	// TestPopWindInternalErrorStaysUncatchable.
	after, ok := mc.evals.PeekK(0).(values.Callable)
	if !ok {
		err := mc.WrapError(werr.ErrNotAProcedure, "dynamic-wind: after must be a procedure")
		return bridgeForeignError(mc, err)
	}

	// Create and push the winding frame, snapshotting the reachable marks at the
	// dynamic-wind call site so the before/after thunks run in this entry environment
	// (R7RS §6.10), not the body's — a parameterize established inside the body must
	// not leak into the after thunk on an escape/reconcile.
	frame := NewDynamicWindFrame(before, after)
	frame.entryMarks = mc.collectReachableMarks()
	mc.PushWindingFrame(frame)

	mc.pc++
	return mc, nil
}

func (p *OperationPushWind) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPushWind)
	return SameType(p, v, ok)
}

// --- PopWind ---

// OperationPopWind pops the innermost dynamic-wind frame from the winding stack.
// This operation does NOT call the after thunk - that is done explicitly in the
// bytecode stream to ensure proper continuation semantics.
//
// The frame is simply removed from the winding stack. If a continuation captured
// inside the dynamic extent is later restored, RestoreWithWindingFrom will handle
// running the appropriate before/after thunks.
//
// R7RS §6.10: dynamic-wind establishes a dynamic extent during which the before
// and after thunks are called whenever control enters or exits.
type OperationPopWind struct {
	OperationBase
}

func NewOperationPopWind() *OperationPopWind {
	return &OperationPopWind{
		OperationBase: NewOperationBase("machine-operation-pop-wind"),
	}
}

func (*OperationPopWind) Apply(mc *MachineContext) (*MachineContext, error) {
	// An empty pop means the PushWind/PopWind pairing the compiler emits has
	// been broken. That is worth an error rather than a shrug: the winding stack
	// silently mis-depths, FindCommonWindingPrefix then computes the wrong
	// common depth, and that number decides which after-thunks run on a
	// continuation escape. Wrong cleanup, no diagnostic.
	_, ok := mc.PopWindingFrame()
	if !ok {
		err := mc.WrapError(werr.ErrInternal,
			"pop-wind: winding stack is empty; PushWind/PopWind pairing is broken")
		return mc, err
	}
	mc.pc++
	return mc, nil
}

func (p *OperationPopWind) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPopWind)
	return SameType(p, v, ok)
}

// --- PopEnv ---

// OperationPopEnv pops one level from the environment chain, restoring the parent
// environment. A nil parent is an error (wrapped werr.ErrNilParentEnvironment): the
// top-level environment cannot be popped. It also clears envPooled, since the parent
// frame was never acquired from the pool and RestoreAndRelease must not release it.
//
// Emitted to close a frame opened by OpPushEnv: at the end of a non-tail
// let/let*/letrec/letrec* body (compile_let.go), and in syntax-case fender evaluation
// to restore the environment when the fender returns false, before branching to the
// next clause.
type OperationPopEnv struct {
	OperationBase
}

func NewOperationPopEnv() *OperationPopEnv {
	return &OperationPopEnv{
		OperationBase: NewOperationBaseWithGoName("operation:pop-env", "PopEnv"),
	}
}

func (p *OperationPopEnv) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationPopEnv)
	return SameType(p, v, ok)
}

// --- PushEnv ---

// OperationPushEnv allocates a new environment frame with the specified
// number of local binding slots and chains it to the current environment.
// Paired with OpPopEnv which restores the parent.
type OperationPushEnv struct {
	OperationBase
	SlotCount int
}

func NewOperationPushEnv(slotCount int) *OperationPushEnv {
	return &OperationPushEnv{
		OperationBase: NewOperationBaseWithGoName("operation:push-env", "PushEnv"),
		SlotCount:     slotCount,
	}
}

func (p *OperationPushEnv) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationPushEnv)
	return FieldMatches(p, v, ok, func(op *OperationPushEnv) int {
		return op.SlotCount
	})
}
