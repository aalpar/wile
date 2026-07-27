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

package compilation

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// NewERRenameClosure creates the `rename` closure for an ER macro invocation.
// defExpandEnv is the definition-site expand environment.
// introScope is a fresh scope unique to this macro invocation, used to ensure
// that renamed symbols not found in the definition-site env (e.g., temporary
// names like 'tmp') get a unique identity that prevents variable capture.
// The returned closure accepts a single symbol argument and returns a
// SyntaxSymbol that resolves to the definition-site binding.
// Results are cached per symbol name so that (eq? (rename 'x) (rename 'x)) is #t.
func NewERRenameClosure(
	defExpandEnv *environment.EnvironmentFrame,
	introScope *syntax.Scope,
) *machine.ForeignClosure {
	cache := make(map[string]*syntax.SyntaxSymbol)

	fn := func(mc machine.CallContext) error {
		arg := mc.Arg(0)
		key, err := extractSymbolKey(arg)
		if err != nil {
			return err
		}

		// Check cache — eq? contract requires same pointer for same symbol name.
		cached, ok := cache[key]
		if ok {
			mc.SetValue(cached)
			return nil
		}

		sym := values.NewSymbol(key)
		result := resolveRenamedSymbol(defExpandEnv, sym, introScope)

		cache[key] = result
		mc.SetValue(result)
		return nil
	}

	cls := machine.NewForeignClosure(defExpandEnv, 1, false, fn)
	cls.SetName("er-rename")
	return cls
}

// resolveRenamedSymbol creates a SyntaxSymbol that resolves to the
// definition-site binding for the given symbol.
// For symbols not found in any definition-site environment, the introScope
// is added to ensure the renamed identifier is distinct from any use-site
// binding with the same name, preventing variable capture.
func resolveRenamedSymbol(defExpandEnv *environment.EnvironmentFrame, sym *values.Symbol, introScope *syntax.Scope) *syntax.SyntaxSymbol {
	// Definition-site lookup. The expand frame parents to the namespace's sealed
	// EXPAND base, which in turn parents to the frozen sealed base, so this reaches
	// base bindings; phase-0 user/import bindings are
	// intentionally invisible (hermeticity — see the reparent in
	// environment/phase_registry.go createPhaseEnv).
	bnd := defExpandEnv.GetBinding(sym, syntax.AllScopes())
	if bnd != nil {
		return symbolWithBindingScopes(sym.Key, bnd, defExpandEnv)
	}

	// Not found — return symbol with the intro scope. This ensures that
	// renamed temporaries like (rename 'tmp) are distinct from any use-site
	// binding of 'tmp', providing ER macro hygiene for introduced identifiers.
	sctx := syntax.NewSourceContext(
		"", "",
		syntax.NewSourceIndexes(0, 0, 0),
		syntax.NewSourceIndexes(0, 0, 0),
	)
	if introScope != nil {
		sctx = sctx.WithScope(introScope)
	}
	return syntax.NewSyntaxSymbol(sym.Key, sctx)
}

// symbolWithBindingScopes creates a SyntaxSymbol with the binding's scopes
// and optionally a ResolvedBinding for cross-library hygiene.
func symbolWithBindingScopes(key string, bnd *environment.Binding, env *environment.EnvironmentFrame) *syntax.SyntaxSymbol {
	bindingScopes := bnd.Scopes()
	sctx := syntax.NewSourceContext(
		"", "",
		syntax.NewSourceIndexes(0, 0, 0),
		syntax.NewSourceIndexes(0, 0, 0),
	)
	for _, scope := range bindingScopes {
		sctx = sctx.WithScope(scope)
	}
	result := syntax.NewSyntaxSymbol(key, sctx)

	gi := env.GetGlobalIndex(values.NewSymbol(key))
	if gi != nil {
		result = result.WithResolvedBinding(gi)
	}

	return result
}

// extractSymbolKey extracts the string key from a symbol or syntax symbol argument.
func extractSymbolKey(arg values.Value) (string, error) {
	switch v := arg.(type) {
	case *values.Symbol:
		return v.Key, nil
	case *syntax.SyntaxSymbol:
		return v.Key(), nil
	default:
		return "", werr.WrapForeignErrorf(
			werr.ErrNotASymbol,
			"er-rename: expected a symbol, got %T", arg,
		)
	}
}
