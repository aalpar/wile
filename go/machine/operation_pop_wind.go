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
type OperationPopWind struct{}

func NewOperationPopWind() *OperationPopWind {
	return &OperationPopWind{}
}

func (*OperationPopWind) Apply(_ context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.PopWindingFrame()
	mc.pc++
	return mc, nil
}

func (*OperationPopWind) SchemeString() string {
	return "#<machine-operation-pop-wind>"
}

func (p *OperationPopWind) IsVoid() bool {
	return p == nil
}

func (p *OperationPopWind) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPopWind)
	return sameType(p, v, ok)
}
