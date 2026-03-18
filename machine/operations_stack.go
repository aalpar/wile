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
	"fmt"

	"github.com/aalpar/wile/values"
)

// --- Push ---

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

// --- Pop ---

type OperationPop struct {
	OperationBase
}

func NewOperationPop() *OperationPop {
	return &OperationPop{
		OperationBase: NewOperationBase("machine-operation-pop"),
	}
}

func (p *OperationPop) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPop)
	return sameType(p, v, ok)
}

// --- Pull ---

type OperationPull struct {
	OperationBase
}

func NewOperationPull() *OperationPull {
	return &OperationPull{
		OperationBase: NewOperationBase("machine-operation-pull"),
	}
}

func (p *OperationPull) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPull)
	return sameType(p, v, ok)
}

// --- Drop ---

// OperationDrop removes the top value from the eval stack without
// affecting the value register. This is used when we need to clean
// up the stack but preserve the current result.
type OperationDrop struct {
	OperationBase
}

func NewOperationDrop() *OperationDrop {
	return &OperationDrop{
		OperationBase: NewOperationBase("machine-operation-drop"),
	}
}

func (p *OperationDrop) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationDrop)
	return sameType(p, v, ok)
}

// --- PeekK ---

type OperationPeekK struct {
	OperationBase
	Depth int
}

func NewOperationPeekK(depth int) *OperationPeekK {
	return &OperationPeekK{
		OperationBase: NewOperationBase("machine-operation-peek-k"),
		Depth:         depth,
	}
}

// SchemeString overrides OperationBase to include depth value.
func (p *OperationPeekK) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-peek-k %d>", p.Depth)
}

func (p *OperationPeekK) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPeekK)
	return fieldMatches(p, v, ok, func(op *OperationPeekK) int { return op.Depth })
}
