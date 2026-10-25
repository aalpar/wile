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
// defScopes is the er-macro-transformer keyword's scope set
// (ERMacroTransformer.DefScopes) and introScope is a fresh scope unique to this
// invocation. Every rename carries both: (datum->syntax #'k sym) followed by the
// introduction flip, as the Scheme layer spells it.
//
// The returned closure takes one symbol or identifier. A bare symbol becomes a
// SyntaxSymbol cached per name so that (eq? (rename 'x) (rename 'x)) is #t. An
// identifier is returned unchanged, as the Scheme layer's (datum->syntax #'k id)
// returns it, and never enters the cache: its scopes are its meaning, and
// re-resolving its spelling at the definition site would turn a use-site local
// into the definition-site global of the same name.
func NewERRenameClosure(
	defEnv *environment.EnvironmentFrame,
	defScopes syntax.Scopes,
	introScope *syntax.Scope,
) *machine.ForeignClosure {
	cache := make(map[string]*syntax.SyntaxSymbol)
	scopes := defScopes
	if introScope != nil {
		scopes = scopes.Add(introScope)
	}

	fn := func(mc machine.CallContext) error {
		switch v := mc.Arg(0).(type) {
		case *syntax.SyntaxSymbol:
			mc.SetValue(v)
			return nil
		case *values.Symbol:
			// Check cache — eq? contract requires same pointer for same symbol name.
			cached, ok := cache[v.Key]
			if ok {
				mc.SetValue(cached)
				return nil
			}
			result := renameSymbol(defEnv, v, scopes)
			cache[v.Key] = result
			mc.SetValue(result)
			return nil
		default:
			return werr.WrapForeignErrorf(
				werr.ErrNotASymbol,
				"er-rename: expected a symbol or identifier, got %T", v,
			)
		}
	}

	cls := machine.NewForeignClosure(defEnv, 1, false, fn)
	cls.SetName("er-rename")
	return cls
}

// renameSymbol builds the renamed identifier for sym: its scopes are the
// definition site's plus the invocation's intro scope, so it resolves wherever
// the definition site's name does (a binding's scopes are a subset of the
// site's, and a fresh scope removes no candidate) and, used as a binder, binds
// only references that carry the intro scope, which a pass-through identifier
// does not. A rename used to drop the intro scope when the name was bound at
// the definition site, and (let ((,(r 'tmp) ,a)) …) then captured the user's tmp.
//
// The pin is for the store the use site cannot see: a library macro's rename of
// one of the library's own globals. It is resolved under the identifier's own
// scopes, at defEnv's phase, the phase the output lands in; a name the
// definition site's globals do not bind (a temporary, or a macro, whose slot is
// a phase up) gets none and resolves at the use site by its scopes alone.
func renameSymbol(defEnv *environment.EnvironmentFrame, sym *values.Symbol, scopes syntax.Scopes) *syntax.SyntaxSymbol {
	sctx := syntax.NewSourceContext(
		"", "",
		syntax.NewSourceIndexes(0, 0, 0),
		syntax.NewSourceIndexes(0, 0, 0),
	)
	sctx.Scopes = scopes
	q := syntax.NewSyntaxSymbol(sym.Key, sctx)
	gi := defEnv.GetGlobalIndexWithScopes(sym, syntax.ScopesOf(scopes))
	if gi != nil {
		q = q.WithResolvedBinding(gi)
	}
	return q
}
