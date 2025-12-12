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

// PrimExpand implements the expand primitive.
// Fully expands a syntax object and returns the expanded syntax.
// (expand stx) -> expanded-stx
func PrimExpand(ctx context.Context, mc *machine.MachineContext) error {
	stx := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	syntaxVal, ok := stx.(syntax.SyntaxValue)
	if !ok {
		return values.NewForeignError("expand: expected syntax object")
	}

	// Check if we're in an expansion context
	expanderCtx := mc.ExpanderContext()
	if expanderCtx != nil {
		// In expansion phase - use current context
		expanded, err := expanderCtx.Expand(syntaxVal)
		if err != nil {
			return values.WrapForeignErrorf(err, "expand: expansion failed")
		}
		mc.SetValue(expanded)
		return nil
	}

	// Not in expansion phase - create temporary expander
	env := mc.EnvironmentFrame()
	expander := machine.NewExpanderTimeContinuation(env)
	ectx := machine.NewExpandTimeCallContext()
	expanded, err := expander.ExpandExpression(ectx, syntaxVal)
	if err != nil {
		return values.WrapForeignErrorf(err, "expand: expansion failed")
	}
	mc.SetValue(expanded)
	return nil
}
