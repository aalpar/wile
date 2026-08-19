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

// GoFrameFunc is the post-body work RunBodyUnderGoFrame reifies as a
// continuation-chain frame. It receives the body's result — mc.GetValue(), so the
// first of several values, Void if none; a callback that cares about the count
// reads mc.GetValues() instead.
//
// Two legal endings, which OperationGoReturn tells apart the same way applyForeign
// does for a foreign function:
//
//   - Deliver a value: leave it in the register (mc.SetValue); the frame returns
//     it to its parent.
//   - Continue the VM: call ApplyCallable or another RunBodyUnder* helper; the
//     frame's return is skipped and whatever was applied produces its result.
type GoFrameFunc func(mc *MachineContext, v values.Value) error

// OperationGoReturn calls a Go function with the value register's contents. It is
// the body of the frame RunBodyUnderGoFrame pushes — the seam that lets post-body
// work written in GO sit on the continuation chain, where every other
// RunBodyUnder* member can only hand a body's result to a SCHEME procedure (an
// apply-frame's consumer).
//
// Pre:  value register = the body's result, mc.cont = the frame's parent
// Post: fn has run; pc++ (so the frame returns) unless fn reconfigured the VM
type OperationGoReturn struct {
	OperationBase
	fn GoFrameFunc
}

// goReturnOpBase is the shared name every OperationGoReturn carries. A value, not
// a pointer, so RunBodyUnderGoFrame can stamp it into a co-allocated
// OperationGoReturn without a second allocation.
var goReturnOpBase = NewOperationBase("machine-operation-go-return")

// NewOperationGoReturn creates an OperationGoReturn that calls fn. The per-call
// path uses RunBodyUnderGoFrame instead, which co-allocates it with its template.
func NewOperationGoReturn(fn GoFrameFunc) *OperationGoReturn {
	return &OperationGoReturn{
		OperationBase: goReturnOpBase,
		fn:            fn,
	}
}

// Apply runs the Go callback and decides whether the frame returns.
//
// The reconfiguration test mirrors applyForeign's: the flag catches an in-place
// Apply (which can leave the template unchanged, e.g. self-application), the
// template comparison catches continuation-restore paths that repoint the template
// without going through Apply. Advancing pc in either case would run the frame's
// OpRestoreContinuation on top of whatever the callback started.
func (p *OperationGoReturn) Apply(mc *MachineContext) (*MachineContext, error) {
	savedTemplate := mc.template
	mc.reconfigured = false
	err := p.fn(mc, mc.GetValue())
	if err != nil {
		return nil, err
	}
	if mc.reconfigured || mc.template != savedTemplate {
		return mc, nil
	}
	mc.pc++
	return mc, nil
}

// EqualTo compares by identity: interchangeable means the same callback, and Go
// funcs are not comparable.
func (p *OperationGoReturn) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationGoReturn)
	if !ok {
		return false
	}
	return p == v
}
