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
	"context"

	"wile/values"
)

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
type OperationPushWind struct{}

func NewOperationPushWind() *OperationPushWind {
	return &OperationPushWind{}
}

func (*OperationPushWind) Apply(_ context.Context, mc *MachineContext) (*MachineContext, error) {
	// Get before closure from stack (at depth 2)
	beforeVal := mc.evals.PeekK(2)
	before, ok := beforeVal.(*MachineClosure)
	if !ok {
		return mc, mc.Error("dynamic-wind: before must be a procedure")
	}

	// Get after closure from stack (at depth 0 = top)
	afterVal := mc.evals.PeekK(0)
	after, ok := afterVal.(*MachineClosure)
	if !ok {
		return mc, mc.Error("dynamic-wind: after must be a procedure")
	}

	// Create and push the winding frame
	frame := NewDynamicWindFrame(before, after)
	mc.PushWindingFrame(frame)

	mc.pc++
	return mc, nil
}

func (*OperationPushWind) SchemeString() string {
	return "#<machine-operation-push-wind>"
}

func (p *OperationPushWind) IsVoid() bool {
	return p == nil
}

func (p *OperationPushWind) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPushWind)
	return sameType(p, v, ok)
}
