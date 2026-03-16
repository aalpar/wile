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

// NewERRenameClosure creates the `rename` closure for an ER macro invocation.
// defExpandEnv is the definition-site expand environment.
// The returned closure accepts a single symbol argument and returns a
// SyntaxSymbol that resolves to the definition-site binding.
// Results are cached per symbol name so that (eq? (rename 'x) (rename 'x)) is #t.
func NewERRenameClosure(
	defExpandEnv *environment.EnvironmentFrame,
) *ForeignClosure {
	cache := make(map[string]*syntax.SyntaxSymbol)

	fn := func(mc *MachineContext) error {
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
		result := resolveRenamedSymbol(defExpandEnv, sym)

		cache[key] = result
		mc.SetValue(result)
		return nil
	}

	cls := NewForeignClosure(defExpandEnv, 1, false, fn)
	cls.SetName("er-rename")
	return cls
}

// resolveRenamedSymbol creates a SyntaxSymbol that resolves to the
// definition-site binding for the given symbol.
func resolveRenamedSymbol(defExpandEnv *environment.EnvironmentFrame, sym *values.Symbol) *syntax.SyntaxSymbol {
	// Try expand environment first.
	bnd := defExpandEnv.GetBinding(sym)
	if bnd != nil {
		return symbolWithBindingScopes(sym.Key, bnd, defExpandEnv)
	}

	// Try runtime environment (phase 0).
	runtimeEnv := findRuntimeEnv(defExpandEnv)
	if runtimeEnv != nil {
		bnd = runtimeEnv.GetBinding(sym)
		if bnd != nil {
			return symbolWithBindingScopes(sym.Key, bnd, runtimeEnv)
		}
	}

	// Not found — return symbol with empty scopes (top-level resolution).
	sctx := syntax.NewSourceContext(
		"", "",
		syntax.NewSourceIndexes(0, 0, 0),
		syntax.NewSourceIndexes(0, 0, 0),
	)
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

// findRuntimeEnv walks up from a phase environment to find the phase-0
// (runtime) environment.
func findRuntimeEnv(env *environment.EnvironmentFrame) *environment.EnvironmentFrame {
	for env != nil && env.PhaseLevel() != 0 {
		env = env.Parent()
	}
	return env
}

// extractSymbolKey extracts the string key from a symbol or syntax symbol argument.
func extractSymbolKey(arg values.Value) (string, error) {
	switch v := arg.(type) {
	case *values.Symbol:
		return v.Key, nil
	case *syntax.SyntaxSymbol:
		return v.Sym.Key, nil
	default:
		return "", werr.WrapForeignErrorf(
			werr.ErrNotASymbol,
			"er-rename: expected a symbol, got %T", arg,
		)
	}
}
