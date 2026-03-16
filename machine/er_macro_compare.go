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
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// NewERCompareClosure creates the `compare` closure for an ER macro invocation.
// useEnv is the use-site environment for resolving identifiers.
// The closure accepts two identifier arguments and returns #t if both resolve
// to the same binding (pointer equality) or both are unbound with the same name.
func NewERCompareClosure(useEnv *environment.EnvironmentFrame) *ForeignClosure {
	fn := func(mc *MachineContext) error {
		id1 := mc.Arg(0)
		id2 := mc.Arg(1)

		bnd1, name1, err := resolveERIdentifier(useEnv, id1)
		if err != nil {
			return err
		}
		bnd2, name2, err := resolveERIdentifier(useEnv, id2)
		if err != nil {
			return err
		}

		same := erBindingsEqual(bnd1, name1, bnd2, name2)
		mc.SetValue(values.BoolToBoolean(same))
		return nil
	}

	cls := NewForeignClosure(useEnv, 2, false, fn)
	cls.SetName("er-compare")
	return cls
}

// erBindingsEqual checks if two resolved identifiers refer to the same binding.
// Handles same-env (pointer equality) and cross-library (value equality) cases.
func erBindingsEqual(bnd1 *environment.Binding, name1 string, bnd2 *environment.Binding, name2 string) bool {
	if bnd1 != nil && bnd2 != nil {
		if bnd1 == bnd2 {
			return true
		}
		// Cross-library fallback: both imported from the same library share
		// the same value pointer even though the Binding objects differ.
		return bnd1.BindingType() == bnd2.BindingType() && bnd1.Value() == bnd2.Value()
	}
	if bnd1 == nil && bnd2 == nil {
		return name1 == name2
	}
	return false
}

// resolveERIdentifier resolves an identifier to its binding in the given environment.
// Returns the binding (may be nil if unbound) and the symbol's string key.
//
// For SyntaxSymbols with a ResolvedBinding (from the rename closure), the binding
// is looked up in the definition-site global environment carried by the GlobalIndex.
// This handles cross-library compare correctly: renamed symbols resolve in the
// library where the macro was defined, not the use site.
func resolveERIdentifier(env *environment.EnvironmentFrame, id values.Value) (*environment.Binding, string, error) {
	switch v := id.(type) {
	case *values.Symbol:
		bnd := env.GetBinding(v)
		return bnd, v.Key, nil
	case *syntax.SyntaxSymbol:
		sym := v.Sym

		// Renamed symbols carry ResolvedBinding — resolve in definition-site env.
		if v.ResolvedBinding != nil {
			gi, ok := v.ResolvedBinding.(*environment.GlobalIndex)
			if ok && gi.Env != nil {
				bnd := gi.Env.GetOwnGlobalBinding(gi)
				return bnd, sym.Key, nil
			}
		}

		scopes := v.Scopes()
		if len(scopes) > 0 {
			bnd := env.GetBindingWithScopes(sym, scopes)
			return bnd, sym.Key, nil
		}
		bnd := env.GetBinding(sym)
		return bnd, sym.Key, nil
	default:
		return nil, "", werr.WrapForeignErrorf(
			werr.ErrNotASymbol,
			"er-compare: expected a symbol, got %T", id,
		)
	}
}
