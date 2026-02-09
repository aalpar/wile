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

type OperationPull struct{}

func NewOperationPull() *OperationPull {
	return &OperationPull{}
}

func (p *OperationPull) SchemeString() string {
	return "#<machine-operation-pull>"
}

func (p *OperationPull) IsVoid() bool {
	return p == nil
}

func (p *OperationPull) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPull)
	return sameType(p, v, ok)
}

func (*OperationPull) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.value = MultipleValues{mc.evals.Pull()}
	mc.pc++
	return mc, nil
}
