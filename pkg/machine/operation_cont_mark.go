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
)

// OperationSetContMark sets a continuation mark on the current frame. Both arms
// of with-continuation-mark use it: in tail position on the activation's own
// frame, out of tail position on the frame the form has just pushed, so neither
// needs a restore.
//
// Pre:  eval stack = [..., key], value register = val
// Post: eval stack = [...], marks[key] = val, pc++
type OperationSetContMark struct {
	OperationBase
}

func NewOperationSetContMark() *OperationSetContMark {
	return &OperationSetContMark{
		OperationBase: NewOperationBase("machine-operation-set-cont-mark"),
	}
}

func (*OperationSetContMark) Apply(mc *MachineContext) (*MachineContext, error) {
	key := mc.evals.Pop()
	mc.SetMark(key, mc.GetValue())
	mc.pc++
	return mc, nil
}

// evalStackDelta is the key the op pops; see evalStackDeltaOperation.
func (*OperationSetContMark) evalStackDelta() int {
	return -1
}

func (p *OperationSetContMark) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationSetContMark)
	return SameType(p, v, ok)
}
