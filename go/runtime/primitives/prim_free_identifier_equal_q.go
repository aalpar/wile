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
	"wile/utils"
	"wile/values"
)

// PrimFreeIdentifierEqualQ implements the free-identifier=? predicate (R7RS).
// Returns #t if two identifiers would resolve to the same binding in the current environment.
// For unbound identifiers, returns #t if they have the same name.
func PrimFreeIdentifierEqualQ(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	id0, ok0 := o0.(*syntax.SyntaxSymbol)
	if !ok0 {
		return values.NewForeignError("free-identifier=?: argument 1 is not an identifier")
	}
	id1, ok1 := o1.(*syntax.SyntaxSymbol)
	if !ok1 {
		return values.NewForeignError("free-identifier=?: argument 2 is not an identifier")
	}

	env := mc.EnvironmentFrame()
	sym0 := env.InternSymbol(values.NewSymbol(id0.Key))
	sym1 := env.InternSymbol(values.NewSymbol(id1.Key))

	// Look up bindings for both identifiers
	binding0 := env.GetBindingWithScopes(sym0, id0.Scopes())
	binding1 := env.GetBindingWithScopes(sym1, id1.Scopes())

	// Both unbound → compare names (free references to same global)
	if binding0 == nil && binding1 == nil {
		mc.SetValue(utils.BoolToBoolean(id0.Key == id1.Key))
		return nil
	}

	// One bound, one unbound → not equal
	if binding0 == nil || binding1 == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}

	// Both bound → same binding object?
	mc.SetValue(utils.BoolToBoolean(binding0 == binding1))
	return nil
}
