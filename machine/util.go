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

import "github.com/aalpar/wile/environment"

func NewForeignClosure(env *environment.EnvironmentFrame, pcnt int, variadic bool, fn ForeignFunction) *ForeignClosure {
	lenv := environment.NewLocalEnvironment(pcnt)
	env = environment.NewEnvironmentFrameWithParent(lenv, env)
	return &ForeignClosure{
		fn:         fn,
		env:        env,
		paramCount: pcnt,
		isVariadic: variadic,
	}
}

// NewVMForeignClosure creates a *MachineClosure backed by a ForeignFunction
// via a bytecode template (OpForeignFunctionCall + OpRestoreContinuation).
// Use this instead of NewForeignClosure for foreign functions that do nested
// VM execution (sub-context + Run), where the iterative VM loop prevents
// Go stack growth that applyForeign would cause.
func NewVMForeignClosure(env *environment.EnvironmentFrame, pcnt int, variadic bool, fn ForeignFunction) *MachineClosure {
	tpl := NewNativeTemplate(pcnt, 0, variadic)
	tpl.AppendOperations(
		NewOperationForeignFunctionCall(fn),
		NewOperationRestoreContinuation(),
	)
	lenv := environment.NewLocalEnvironment(pcnt)
	env = environment.NewEnvironmentFrameWithParent(lenv, env)
	return NewClosureWithTemplate(tpl, env)
}
