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

package validate

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// bindLocalSymbol binds sym in env as a local variable using the canonical
// four-argument shape (symbol, BindingTypeVariable, scopes, source context).
// This is the per-iteration primitive used by let* / letrec / named-let when
// the environment evolves with each binding and a fresh child frame is built
// separately.
func bindLocalSymbol(env *environment.EnvironmentFrame, sym *syntax.SyntaxSymbol) {
	env.MaybeCreateLocalBinding(
		sym.Sym,
		environment.BindingTypeVariable,
		sym.Scopes(),
		sym.SourceContext(),
	)
}

// createChildEnvWithSymbols ALWAYS creates a fresh child frame and binds the
// given symbols as variables, even when syms is empty. This is the primitive
// used by lambda parameter binding, which requires a per-lambda frame
// boundary independent of how many parameters were declared.
func createChildEnvWithSymbols(env *environment.EnvironmentFrame, syms []*syntax.SyntaxSymbol) *environment.EnvironmentFrame {
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)
	for _, sym := range syms {
		bindLocalSymbol(childEnv, sym)
	}
	return childEnv
}

// extendEnvWithSymbols is the let-family-flavored wrapper around
// createChildEnvWithSymbols: it short-circuits and returns env unchanged when
// syms is empty (since let/let*/letrec with zero bindings have no need to
// open a fresh scope). Use createChildEnvWithSymbols directly when an
// always-fresh frame is required (lambda).
func extendEnvWithSymbols(env *environment.EnvironmentFrame, syms []*syntax.SyntaxSymbol) *environment.EnvironmentFrame {
	if len(syms) == 0 {
		return env
	}
	return createChildEnvWithSymbols(env, syms)
}

// findDuplicateSymbols returns the duplicates in syms in order of second
// (and later) appearance. Equality is by (key, scope-fingerprint) tuple so
// hygienic bindings with the same name but different scope sets (introduced
// by macro expansion) are not falsely treated as duplicates. Empty result
// means no duplicates; callers decide how to report.
func findDuplicateSymbols(syms []*syntax.SyntaxSymbol) []*syntax.SyntaxSymbol {
	if len(syms) < 2 {
		return nil
	}
	seen := make(map[bindingIdentity]bool, len(syms))
	var dups []*syntax.SyntaxSymbol
	for _, sym := range syms {
		id := bindingIdentity{
			key:      sym.Sym.Key,
			scopeKey: scopeFingerprint(sym.Scopes()),
		}
		if seen[id] {
			dups = append(dups, sym)
			continue
		}
		seen[id] = true
	}
	return dups
}
