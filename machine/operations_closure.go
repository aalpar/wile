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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// --- MakeClosure ---

type OperationMakeClosure struct {
	OperationBase
}

func NewOperationMakeClosure() *OperationMakeClosure {
	return &OperationMakeClosure{
		OperationBase: NewOperationBase("machine-operation-make-closure"),
	}
}

func (p *OperationMakeClosure) Apply(mc *MachineContext) (*MachineContext, error) {
	envVal, tplVal := mc.evals.Pop2()
	compiletimeEnv, ok := envVal.(*environment.EnvironmentFrame)
	if !ok {
		return mc, werr.WrapForeignErrorf(werr.ErrNotALocalEnvironmentFrame, "MakeClosure: expected environment frame on stack")
	}
	tpl, ok := tplVal.(*NativeTemplate)
	if !ok {
		return mc, werr.WrapForeignErrorf(werr.ErrNotAMachineTemplate, "MakeClosure: expected native template on stack")
	}
	// Linked closure (Cardelli 1983). Captures E by pointer, not by copy.
	//
	//   closure.env = NewFrame(compiletimeEnv.Local(), mc.env)
	//
	//   where compiletimeEnv.Local() provides the parameter structure
	//   and mc.env becomes the parent for free variable access.
	//
	//   Invariant: the parent pointer must be the RUNTIME mc.env, not
	//     the compile-time env. Compile-time frames hold placeholders;
	//     runtime frames hold actual values.
	//   Constrains: Apply (always copies this env to prevent aliasing
	//     across recursive calls and SRFI-18 thread races), envPooled flag
	//     (this env is not poolable — closure holds a live reference).
	//   Constrained by: de Bruijn (depth in free-var access counts
	//     parent hops through this chain), CESK model (E component).
	//
	// See BIBLIOGRAPHY.md "Linked Closure Representation".
	//
	// Create runtime environment with compile-time local structure
	// but RUNTIME parent chain. This is critical for:
	// 1. Nested lambdas: inner lambdas need to access outer params via parent chain
	// 2. Closures: captured variables must have runtime values, not compile-time placeholders
	runtimeEnv := environment.NewEnvironmentFrameWithParent(
		compiletimeEnv.LocalEnvironment(), // Keep local structure for parameters
		mc.env,                            // Capture runtime parent for free variables
	)
	// The closure now references mc.env through runtimeEnv's parent chain.
	// Mark it non-poolable so RestoreAndRelease won't recycle it while the
	// closure still holds a live reference.
	mc.envPooled = false
	cls := NewClosureWithTemplate(tpl, runtimeEnv)
	mc.SetValue(cls)
	mc.pc++
	return mc, nil
}

func (p *OperationMakeClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationMakeClosure)
	return SameType(p, v, ok)
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
			err := mc.Error(fmt.Sprintf("expected closure in case-lambda, got %T", elements[i]))
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
