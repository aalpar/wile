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
//
// defEnv is the definition-site environment AT THE PHASE THE OUTPUT LANDS IN,
// which invokeERTransformer derives from the expansion under way. It is not the
// frame the transformer body compiled in: that frame is one rung up the tower,
// and reading a rename there asks the transformer's implementation language what
// a name means. See invokeERTransformer for the contract and what the phase-up
// read was measured to do.
//
// introScope is a fresh scope unique to this macro invocation, used to ensure
// that renamed symbols not found in defEnv (e.g., temporary names like 'tmp)
// get a unique identity that prevents variable capture.
// The returned closure accepts a single symbol argument and returns a
// SyntaxSymbol that resolves to the definition-site binding.
// Results are cached per symbol name so that (eq? (rename 'x) (rename 'x)) is #t.
func NewERRenameClosure(
	defEnv *environment.EnvironmentFrame,
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
		result := resolveRenamedSymbol(defEnv, sym, introScope)

		cache[key] = result
		mc.SetValue(result)
		return nil
	}

	cls := machine.NewForeignClosure(defEnv, 1, false, fn)
	cls.SetName("er-rename")
	return cls
}

// resolveRenamedSymbol creates a SyntaxSymbol that resolves to the
// definition-site binding for the given symbol.
// For symbols not found in any definition-site environment, the introScope
// is added to ensure the renamed identifier is distinct from any use-site
// binding with the same name, preventing variable capture.
func resolveRenamedSymbol(defEnv *environment.EnvironmentFrame, sym *values.Symbol, introScope *syntax.Scope) *syntax.SyntaxSymbol {
	// Definition-site lookup, at defEnv's own phase — the phase the output lands
	// in. The ranked probe is a candidate only against slots at exactly that
	// phase; hermeticity is that key disjointness, not a parent link —
	// createPhaseEnv (environment/phase_registry.go) mints a view with no lexical
	// parent at all.
	//
	// The comment this replaces was wrong on both halves, and both were measured.
	// It said the probe reaches base bindings "via the ambient T3 tier": Stage A
	// deleted that tier (probeTiersLocked's tierOf has no ambient arm), and the
	// base is reached through a BULK ROW instead, which resolves to the phase-0
	// slot's own *Binding rather than to a copy. And it said level-0 user and
	// import bindings are "intentionally invisible": true of the frame the caller
	// used to pass (a phase-0 (define (helper) 'def) probed nil from phase 1, and
	// the ER test that looked like a counterexample was reaching the global through
	// the intro-scope branch below, not through this one), but the invisibility was
	// the defect rather than the design — a rename of a phase-0 name is meant to
	// find it.
	bnd := defEnv.GetBinding(sym, syntax.AllScopes())
	if bnd != nil {
		return symbolWithBindingScopes(sym.Key, bnd, defEnv)
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
