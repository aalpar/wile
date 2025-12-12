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

package primitives

import (
	"context"

	"wile/machine"
	"wile/syntax"
	"wile/values"
)

// PrimExpandOnce implements the expand-once primitive.
// Performs a single step of macro expansion and returns both the
// expanded syntax and a boolean indicating whether expansion occurred.
// (expand-once stx) -> (values expanded-stx did-expand?)
func PrimExpandOnce(ctx context.Context, mc *machine.MachineContext) error {
	stx := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	syntaxVal, ok := stx.(syntax.SyntaxValue)
	if !ok {
		return values.NewForeignError("expand-once: expected syntax object")
	}

	// Check if we're in an expansion context
	expanderCtx := mc.ExpanderContext()
	if expanderCtx != nil {
		// In expansion phase - use current context
		expanded, didExpand, err := expanderCtx.ExpandOnce(syntaxVal)
		if err != nil {
			return values.WrapForeignErrorf(err, "expand-once: expansion failed")
		}
		mc.SetValues(expanded, values.NewBoolean(didExpand))
		return nil
	}

	// Not in expansion phase - create temporary expander
	env := mc.EnvironmentFrame()
	expander := machine.NewExpanderTimeContinuation(env)
	ectx := machine.NewExpandTimeCallContext()
	expanded, didExpand, err := expander.ExpandOnce(ectx, syntaxVal)
	if err != nil {
		return values.WrapForeignErrorf(err, "expand-once: expansion failed")
	}
	mc.SetValues(expanded, values.NewBoolean(didExpand))
	return nil
}
