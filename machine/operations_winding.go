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
	"github.com/aalpar/wile/values"
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
	// Get before closure from stack (at depth 2)
	beforeVal := mc.evals.PeekK(2)
	before, ok := beforeVal.(Closure)
	if !ok {
		err := mc.Error("dynamic-wind: before must be a procedure")
		return mc, err
	}

	// Get after closure from stack (at depth 0 = top)
	afterVal := mc.evals.PeekK(0)
	after, ok := afterVal.(Closure)
	if !ok {
		err := mc.Error("dynamic-wind: after must be a procedure")
		return mc, err
	}

	// Create and push the winding frame
	frame := NewDynamicWindFrame(before, after)
	mc.PushWindingFrame(frame)

	mc.pc++
	return mc, nil
}

func (p *OperationPushWind) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPushWind)
	return sameType(p, v, ok)
}

// --- PopWind ---

// OperationPopWind pops the innermost dynamic-wind frame from the winding stack.
// This operation does NOT call the after thunk - that is done explicitly in the
// bytecode stream to ensure proper continuation semantics.
//
// The frame is simply removed from the winding stack. If a continuation captured
// inside the dynamic extent is later restored, the RestoreWithWinding mechanism
// will handle running the appropriate before/after thunks.
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
	mc.PopWindingFrame()
	mc.pc++
	return mc, nil
}

func (p *OperationPopWind) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPopWind)
	return sameType(p, v, ok)
}

// --- PopEnv ---

// OperationPopEnv unconditionally restores the parent environment.
// It pops one level from the environment chain (restoring the parent environment).
//
// This is used in syntax-case fender evaluation to restore the environment
// when the fender returns false, before branching to the next clause.
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
	return sameType(p, v, ok)
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
	return fieldMatches(p, v, ok, func(op *OperationPushEnv) int {
		return op.SlotCount
	})
}
