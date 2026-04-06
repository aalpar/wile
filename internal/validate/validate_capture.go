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

// markCapturedBindings walks the validated body (and optionally init
// expressions) to determine which let bindings are referenced from inside
// escaping closures. A lambda is non-escaping only when it appears as the
// operator of a ValidatedCall (immediately-applied lambda).
//
// walkInits should be true for let*, letrec, and letrec* (where inits see the
// bindings) and false for plain let (where inits are in the outer scope).
//
// Best-effort: if binding resolution fails (scope mismatch), the binding
// stays non-captured. Must not gate correctness-critical optimizations
// without re-validation.
func markCapturedBindings(
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

	w := captureWalker{
		env:      childEnv,
		bindings: bindings,
		idToIdx:  idToIdx,
	}

	if walkInits {
		for _, b := range bindings {
			w.walkExpr(b.Init, 0)
		}
	}
	for _, expr := range body {
		w.walkExpr(expr, 0)
	}
}

// captureWalker walks a ValidatedExpr tree tracking closure depth.
type captureWalker struct {
	env      *environment.EnvironmentFrame
	bindings []ValidatedLetBinding
	idToIdx  map[environment.BindingID]int
}

// checkSymbol marks a binding as captured if the given symbol resolves to
// one of the tracked bindings and we are inside a closure (depth > 0).
func (p *captureWalker) checkSymbol(sym *syntax.SyntaxSymbol, depth int) {
	if depth <= 0 {
		return
	}
	bid, ok := p.env.ResolveBindingID(sym.Sym, sym.Scopes())
	if ok {
		idx, found := p.idToIdx[bid]
		if found {
			p.bindings[idx].Captured = true
		}
	}
}

func (p *captureWalker) walkExpr(expr ValidatedExpr, depth int) {
	if expr == nil {
		return
	}
	// Leaf: check symbol reference.
	sym, ok := expr.(*ValidatedSymbol)
	if ok {
		p.checkSymbol(sym.Symbol, depth)
		return
	}
	// set! target: mutation from inside a closure also captures.
	// WalkSubExprs intentionally omits the set! target (it's mutation, not a reference),
	// but capture analysis needs it because mutating a variable from inside a closure
	// requires the variable to be captured just like reading it does.
	setBang, ok := expr.(*ValidatedSetBang)
	if ok {
		p.checkSymbol(setBang.Name, depth)
	}

	WalkSubExprs(expr, func(child ValidatedExpr, role ChildRole) {
		switch role {
		case RoleClosureBody:
			p.walkExpr(child, depth+1)
		case RoleCallProc:
			// Immediately-applied lambda: walk body at current depth,
			// not depth+1, because the closure does not escape.
			switch proc := child.(type) {
			case *ValidatedLambda:
				for _, b := range proc.Body() {
					p.walkExpr(b, depth)
				}
			case *ValidatedCaseLambda:
				for _, clause := range proc.Clauses() {
					for _, b := range clause.Body() {
						p.walkExpr(b, depth)
					}
				}
			default:
				p.walkExpr(child, depth)
			}
		default:
			p.walkExpr(child, depth)
		}
	})
}
