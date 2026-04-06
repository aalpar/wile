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

import "github.com/aalpar/wile/environment"

// markEscapedBindings walks the validated body (and optionally init
// expressions) to determine which let bindings are referenced in non-call
// positions (argument, return value, init expression). A reference is in
// call position only when it is the Proc of a ValidatedCall or ValidatedApply.
//
// set! targets are NOT marked as escaped — mutation is tracked by Mutable.
// The three fields (Mutable, Captured, Escapes) form an implicational base:
// each carries information not derivable from the others.
//
// walkInits should be true for let*, letrec, and letrec* (where inits see the
// bindings) and false for plain let (where inits are in the outer scope).
//
// Best-effort: if binding resolution fails (scope mismatch), the binding
// stays non-escaped. Must not gate correctness-critical optimizations
// without re-validation.
func markEscapedBindings(
	childEnv *environment.EnvironmentFrame,
	bindings []ValidatedLetBinding,
	body []ValidatedExpr,
	walkInits bool,
) {
	if childEnv == nil || len(bindings) == 0 {
		return
	}

	// Build BindingID → index map for the let bindings.
	idToIdx := make(map[environment.BindingID]int, len(bindings))
	for i, b := range bindings {
		bid, ok := childEnv.ResolveBindingID(b.Name.Sym, b.Name.Scopes())
		if ok {
			idToIdx[bid] = i
		}
	}
	if len(idToIdx) == 0 {
		return
	}

	w := escapeWalker{
		env:      childEnv,
		bindings: bindings,
		idToIdx:  idToIdx,
	}

	if walkInits {
		for _, b := range bindings {
			w.walkExpr(b.Init)
		}
	}
	for _, expr := range body {
		w.walkExpr(expr)
	}
}

// escapeWalker walks a ValidatedExpr tree detecting non-call-position
// references to tracked let bindings. Uses WalkSubExprs for structural
// recursion — only the symbol check and call-position logic are here.
type escapeWalker struct {
	env      *environment.EnvironmentFrame
	bindings []ValidatedLetBinding
	idToIdx  map[environment.BindingID]int
}

func (p *escapeWalker) walkExpr(expr ValidatedExpr) {
	if expr == nil {
		return
	}
	// Symbols are leaf nodes — check for non-call-position reference.
	sym, ok := expr.(*ValidatedSymbol)
	if ok {
		bid, resolved := p.env.ResolveBindingID(sym.Symbol.Sym, sym.Symbol.Scopes())
		if resolved {
			idx, found := p.idToIdx[bid]
			if found {
				p.bindings[idx].Escapes = true
			}
		}
		return
	}
	// Structural recursion via WalkSubExprs.
	WalkSubExprs(expr, func(child ValidatedExpr, role ChildRole) {
		if role == RoleCallProc {
			sym, ok := child.(*ValidatedSymbol)
			if ok {
				bid, resolved := p.env.ResolveBindingID(sym.Symbol.Sym, sym.Symbol.Scopes())
				if resolved {
					_, tracked := p.idToIdx[bid]
					if tracked {
						// Call position — do NOT mark Escapes.
						return
					}
				}
			}
		}
		p.walkExpr(child)
	})
}
