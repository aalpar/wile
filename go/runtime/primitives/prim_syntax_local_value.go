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

// PrimSyntaxLocalValue implements the syntax-local-value primitive.
// Retrieves the compile-time value bound to an identifier in the expand phase.
// (syntax-local-value id) -> value
//
// This primitive can only be called during macro expansion (when an ExpanderContext
// is set on the MachineContext). It looks up the identifier in the expand phase
// environment, respecting hygiene scopes.
//
// If the binding is a CompileTimeValue, it returns the unwrapped value.
// This allows define-for-syntax bindings to be accessed from macro transformers.
func PrimSyntaxLocalValue(ctx context.Context, mc *machine.MachineContext) error {
	id := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	syntaxSym, ok := id.(*syntax.SyntaxSymbol)
	if !ok {
		return values.NewForeignError("syntax-local-value: expected identifier")
	}

	expanderCtx := mc.ExpanderContext()
	if expanderCtx == nil {
		return values.NewForeignError("syntax-local-value: not in expansion context")
	}

	// Look up in expand phase
	expandEnv := expanderCtx.Env().Expand()
	sym := syntaxSym.Datum()
	sym = expandEnv.InternSymbol(sym)

	binding := expandEnv.GetBindingWithScopes(sym, syntaxSym.Scopes())
	if binding == nil {
		return values.NewForeignErrorf("syntax-local-value: no binding for %s", sym.Key)
	}

	val := binding.Value()

	// If it's a CompileTimeValue, unwrap it
	if ctv, ok := val.(*values.CompileTimeValue); ok {
		val = ctv.Unwrap()
	}

	mc.SetValue(val)
	return nil
}
