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

// noMarkSentinel is a package-level sentinel used by OperationSaveContMark
// and OperationRestoreContMark to distinguish "no previous mark" from a mark
// whose value is nil. Compared by pointer identity only.
var noMarkSentinel = values.NewSymbol("\x00no-mark-sentinel")

// OperationSetContMark sets a continuation mark on the current frame.
// Used in tail position where no restore is needed.
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

func (p *OperationSetContMark) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationSetContMark)
	return sameType(p, v, ok)
}

// OperationSaveContMark saves the previous mark value and sets a new one.
// Used in non-tail position, paired with OperationRestoreContMark.
//
// Pre:  eval stack = [..., key], value register = val
// Post: eval stack = [..., key, old_val_or_sentinel], marks[key] = val, pc++
type OperationSaveContMark struct {
	OperationBase
}

func NewOperationSaveContMark() *OperationSaveContMark {
	return &OperationSaveContMark{
		OperationBase: NewOperationBase("machine-operation-save-cont-mark"),
	}
}

func (*OperationSaveContMark) Apply(mc *MachineContext) (*MachineContext, error) {
	key := mc.evals.Pop()
	old := mc.GetMark(key)
	mc.evals.Push(key)
	if old == nil {
		mc.evals.Push(noMarkSentinel)
	} else {
		mc.evals.Push(old)
	}
	mc.SetMark(key, mc.GetValue())
	mc.pc++
	return mc, nil
}

func (p *OperationSaveContMark) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationSaveContMark)
	return sameType(p, v, ok)
}

// OperationRestoreContMark restores the previous mark value after body evaluation.
// Paired with OperationSaveContMark.
//
// Pre:  eval stack = [..., key, old_val_or_sentinel]
// Post: eval stack = [...], marks[key] restored or deleted, pc++
type OperationRestoreContMark struct {
	OperationBase
}

func NewOperationRestoreContMark() *OperationRestoreContMark {
	return &OperationRestoreContMark{
		OperationBase: NewOperationBase("machine-operation-restore-cont-mark"),
	}
}

func (*OperationRestoreContMark) Apply(mc *MachineContext) (*MachineContext, error) {
	old := mc.evals.Pop()
	key := mc.evals.Pop()
	if old == noMarkSentinel {
		mc.DeleteMark(key)
	} else {
		mc.SetMark(key, old)
	}
	mc.pc++
	return mc, nil
}

func (p *OperationRestoreContMark) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationRestoreContMark)
	return sameType(p, v, ok)
}
