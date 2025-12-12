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

// PrimSyntaxLocalIntroduce implements the syntax-local-introduce primitive.
// Flips the introduction scope on a syntax object.
// (syntax-local-introduce stx) -> stx
//
// This primitive toggles the "introduction scope" on a syntax object.
// The introduction scope is added to identifiers introduced by a macro.
// By flipping it, you can make an introduced identifier behave as if it
// came from the macro use site (or vice versa).
//
// Use cases:
//   - Breaking hygiene intentionally (anaphoric macros)
//   - Making macro-introduced bindings visible at the use site
//   - Implementing advanced macro patterns like syntax-parameterize
//
// This primitive can only be called during macro expansion (when an
// ExpanderContext is set on the MachineContext with an introduction scope).
func PrimSyntaxLocalIntroduce(ctx context.Context, mc *machine.MachineContext) error {
	stx := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	syntaxVal, ok := stx.(syntax.SyntaxValue)
	if !ok {
		return values.NewForeignError("syntax-local-introduce: expected syntax object")
	}

	expanderCtx := mc.ExpanderContext()
	if expanderCtx == nil {
		return values.NewForeignError("syntax-local-introduce: not in expansion context")
	}

	introScope := expanderCtx.IntroductionScope()
	if introScope == nil {
		// No introduction scope set - return syntax unchanged
		// This can happen if called outside a macro transformer
		mc.SetValue(syntaxVal)
		return nil
	}

	// Flip the scope on the syntax object
	result := syntax.FlipScope(syntaxVal, introScope)
	mc.SetValue(result)
	return nil
}
