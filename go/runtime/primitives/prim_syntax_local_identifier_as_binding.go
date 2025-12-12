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

// PrimSyntaxLocalIdentifierAsBinding implements the syntax-local-identifier-as-binding primitive.
// Marks an identifier as a binding site by adding the use-site scope.
// (syntax-local-identifier-as-binding id) -> id
//
// This primitive adds the "use-site scope" to an identifier, marking it as
// a binding site. This is used in binding forms (like let, lambda) to ensure
// proper hygiene when the binding form is implemented as a macro.
//
// When a macro introduces a binding form, the bound identifiers need the
// use-site scope to be properly distinguished from identifiers at the
// macro's definition site.
//
// Use cases:
//   - Implementing custom binding forms as macros
//   - Ensuring proper hygiene for macro-generated bindings
//   - Creating hygienic versions of anaphoric macros
//
// This primitive can only be called during macro expansion (when an
// ExpanderContext is set on the MachineContext with a use-site scope).
func PrimSyntaxLocalIdentifierAsBinding(ctx context.Context, mc *machine.MachineContext) error {
	id := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	syntaxSym, ok := id.(*syntax.SyntaxSymbol)
	if !ok {
		return values.NewForeignError("syntax-local-identifier-as-binding: expected identifier")
	}

	expanderCtx := mc.ExpanderContext()
	if expanderCtx == nil {
		return values.NewForeignError("syntax-local-identifier-as-binding: not in expansion context")
	}

	useSiteScope := expanderCtx.UseSiteScope()
	if useSiteScope == nil {
		// No use-site scope set - return identifier unchanged
		mc.SetValue(syntaxSym)
		return nil
	}

	// Add the use-site scope to mark as binding
	result := syntax.AddScopeToSyntax(syntaxSym, useSiteScope)
	mc.SetValue(result)
	return nil
}
