// Copyright 2025 Aaron Alpar
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
	"wile/environment"
	"wile/values"
)

type OperationMakeClosure struct {
}

func NewOperationMakeClosure() *OperationMakeClosure {
	return &OperationMakeClosure{}
}

func (p *OperationMakeClosure) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	compiletimeEnv, ok := mc.evals.Pop().(*environment.EnvironmentFrame)
	if !ok {
		return mc, values.ErrNotALocalEnvironmentFrame
	}
	tpl, ok := mc.evals.Pop().(*NativeTemplate)
	if !ok {
		return mc, values.ErrNotAMachineTemplate
	}
	// Create runtime environment with compile-time local structure
	// but RUNTIME parent chain. This is critical for:
	// 1. Nested lambdas: inner lambdas need to access outer params via parent chain
	// 2. Closures: captured variables must have runtime values, not compile-time placeholders
	runtimeEnv := environment.NewEnvironmentFrameWithParent(
		compiletimeEnv.LocalEnvironment(), // Keep local structure for parameters
		mc.env,                            // Capture runtime parent for free variables
	)
	cls := NewClosureWithTemplate(tpl, runtimeEnv)
	mc.value = MultipleValues{cls}
	mc.pc++
	return mc, nil
}

func (p *OperationMakeClosure) SchemeString() string {
	return "#<machine-operation-make-closure>"
}

func (p *OperationMakeClosure) IsVoid() bool {
	return p == nil
}

func (p *OperationMakeClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationMakeClosure)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}
