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

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// --- MakeClosure ---

type OperationMakeClosure struct {
	OperationBase
	// freeCount is how many free values sit under the template on the eval
	// stack, in slot order.
	freeCount int
	// selfSlot is the free-vector index the finished closure writes itself into
	// (the letrec T2 back-patch), or -1 when the closure has no self reference.
	selfSlot int
}

// NewOperationMakeClosure returns a make-closure op that pops freeCount free
// values (pushed in slot order) and then the template. selfSlot is the free-vector
// index the closure must write ITSELF into once built — the letrec T2
// back-patch — or -1 when there is none.
func NewOperationMakeClosure(freeCount, selfSlot int) *OperationMakeClosure {
	return &OperationMakeClosure{
		OperationBase: NewOperationBase("machine-operation-make-closure"),
		freeCount:     freeCount,
		selfSlot:      selfSlot,
	}
}

// popMakeClosureArgs pops MakeClosure's operands off the eval stack: the
// template (pushed LAST, so popped first), then freeCount free values, which
// were pushed in slot order and therefore come back bottom-to-top.
//
// The compile-time frame used to ride alongside the template and now rides ON it
// (NativeTemplate.shape), so codegen pushes the template and the free values and
// nothing else. Both the (production-vestigial) OperationMakeClosure.Apply
// method and the inline OpMakeClosure case in Run() use this so the validation
// lives in one place.
func popMakeClosureArgs(mc *MachineContext, freeCount int) (*NativeTemplate, []values.Value, error) {
	tpl, ok := mc.evals.Pop().(*NativeTemplate)
	if !ok {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAMachineTemplate,
			"MakeClosure: expected native template on stack")
	}
	if freeCount == 0 {
		return tpl, nil, nil
	}
	free := make([]values.Value, freeCount)
	// PopN hands back the drained values bottom-to-top, which IS slot order:
	// the emitter pushed slot 0 first. Copying rather than retaining the drain
	// buffer matters — it is reused by the next drain.
	drained := mc.evals.PopN(freeCount)
	if len(drained) != freeCount {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"MakeClosure: expected %d free values on stack, got %d", freeCount, len(drained))
	}
	copy(free, drained)
	return tpl, free, nil
}

func (p *OperationMakeClosure) Apply(mc *MachineContext) (*MachineContext, error) {
	tpl, free, err := popMakeClosureArgs(mc, p.freeCount)
	if err != nil {
		return mc, err
	}
	// FLAT closure. The free variables were copied onto the eval stack by the
	// creation protocol and travel in the vector; the static link is chosen by
	// closureLink, which answers the lexical ROOT (parent == nil) for an ordinary
	// body and only keeps mc.env for a template that reads through the frame
	// chain in a way the free-variable pass cannot see.
	//
	//   The shape is a property of the compiled body, so it rides on the
	//   template; only the link varies per closure. See MachineClosure for why
	//   neither is combined into a frame.
	//   Constrains: Apply (always builds a fresh frame from the pair to
	//     prevent aliasing across recursive calls and SRFI-18 thread races).
	//   Constrained by: CESK model (E component).
	//
	// See BIBLIOGRAPHY.md "Linked Closure Representation" for the model this
	// replaced.
	//
	// envPooled is narrowed to the retaining case. The argument lives with the
	// live path — the inline OpMakeClosure arm in MachineContext.Run — and this
	// method must not diverge from it.
	if tpl.RetainsLexicalEnv() {
		mc.envPooled = false
	}
	cls := NewClosureCapturing(tpl, closureLink(mc.env, tpl), free)
	backPatchSelfSlot(cls, free, p.selfSlot)
	mc.SetValue(cls)
	mc.pc++
	return mc, nil
}

// backPatchSelfSlot writes the just-built closure into its own free vector — the
// letrec T2 carve-out. The emitter pushed a placeholder for that slot, and no
// bytecode runs between the push and here, so the placeholder window is not
// observable: the closure cannot be applied before it exists.
//
// slot < 0 means the closure has no self-reference, which is the common case.
func backPatchSelfSlot(cls *MachineClosure, free []values.Value, slot int) {
	if slot < 0 || slot >= len(free) {
		return
	}
	free[slot] = cls
}

// EqualTo compares BOTH packed fields.
//
// THIS IS LOAD-BEARING, NOT STYLE. The literal pool dedups templates through
// NativeTemplate.EqualTo, which compares their code, and two MakeClosure
// instructions that differ only in free count or self slot would otherwise
// compare equal — collapsing two templates whose closures capture different
// things. Mirrors OperationMakeCaseLambdaClosure.EqualTo.
func (p *OperationMakeClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationMakeClosure)
	return FieldMatches(p, v, ok, func(op *OperationMakeClosure) [2]int {
		return [2]int{op.freeCount, op.selfSlot}
	})
}

// --- MakeCaseLambdaClosure ---

// OperationMakeCaseLambdaClosure creates a case-lambda closure from multiple closures.
// Stack layout (top to bottom): closure_n, closure_n-1, ..., closure_1
// The closureCount immediate specifies how many closures to pop.
type OperationMakeCaseLambdaClosure struct {
	OperationBase
	closureCount int
}

func NewOperationMakeCaseLambdaClosure(closureCount int) *OperationMakeCaseLambdaClosure {
	return &OperationMakeCaseLambdaClosure{
		OperationBase: NewOperationBase("machine-operation-make-case-lambda-closure"),
		closureCount:  closureCount,
	}
}

func (p *OperationMakeCaseLambdaClosure) Apply(mc *MachineContext) (*MachineContext, error) {
	// Batch pop all closures from the stack
	// PopN returns elements in stack order (bottom to top)
	elements := mc.evals.PopN(p.closureCount)

	// Convert elements to closures, preserving order
	closures := make([]*MachineClosure, p.closureCount)
	for i := 0; i < p.closureCount; i++ {
		cls, ok := elements[i].(*MachineClosure)
		if !ok {
			err := mc.WrapError(werr.ErrNotAClosure, fmt.Sprintf("expected closure in case-lambda, got %T", elements[i]))
			return mc, err
		}
		closures[i] = cls
	}

	caseLambda := NewCaseLambdaClosure(closures)
	mc.SetValue(caseLambda)
	mc.pc++
	return mc, nil
}

func (p *OperationMakeCaseLambdaClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationMakeCaseLambdaClosure)
	return FieldMatches(p, v, ok, func(op *OperationMakeCaseLambdaClosure) int { return op.closureCount })
}
