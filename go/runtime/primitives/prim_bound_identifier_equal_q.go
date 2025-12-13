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

// PrimBoundIdentifierEqualQ implements the bound-identifier=? predicate (R7RS).
// Returns #t if two identifiers have the same name AND the same scope sets,
// meaning they would create the same binding if used as binding occurrences.
func PrimBoundIdentifierEqualQ(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	id0, ok0 := o0.(*syntax.SyntaxSymbol)
	if !ok0 {
		return values.NewForeignError("bound-identifier=?: argument 1 is not an identifier")
	}
	id1, ok1 := o1.(*syntax.SyntaxSymbol)
	if !ok1 {
		return values.NewForeignError("bound-identifier=?: argument 2 is not an identifier")
	}

	// Same name?
	if id0.Key != id1.Key {
		mc.SetValue(values.FalseValue)
		return nil
	}

	// Same scopes? (set equality = mutual subset check)
	scopes0 := id0.Scopes()
	scopes1 := id1.Scopes()
	result := syntax.ScopesMatch(scopes0, scopes1) && syntax.ScopesMatch(scopes1, scopes0)
	mc.SetValue(utils.BoolToBoolean(result))
	return nil
}
