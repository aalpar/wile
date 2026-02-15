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

	"github.com/aalpar/wile/values"
)

type OperationPush struct {
	OperationBase
}

func NewOperationPush() *OperationPush {
	return &OperationPush{
		OperationBase: NewOperationBase("machine-operation-push"),
	}
}

func (p *OperationPush) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPush)
	return sameType(p, v, ok)
}

// Apply pushes the value register onto the evaluation stack. It branches on
// the split value representation: the common single-value case uses Push
// (no slice overhead), while the rare multi-value case falls through to
// PushAll. A nil value register (void) pushes nothing.
func (*OperationPush) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	if mc.multiValues != nil {
		mc.evals.PushAll(mc.multiValues)
	} else if mc.singleValue != nil {
		mc.evals.Push(mc.singleValue)
	}
	mc.pc++
	return mc, nil
}
