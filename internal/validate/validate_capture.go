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
	switch e := expr.(type) {
	case *ValidatedSymbol:
		p.checkSymbol(e.Symbol, depth)

	case *ValidatedLambda:
		p.walkBody(e.Body(), depth+1)

	case *ValidatedCaseLambda:
		for _, clause := range e.Clauses() {
			p.walkBody(clause.Body(), depth+1)
		}

	case *ValidatedCall:
		switch proc := e.Proc().(type) {
		case *ValidatedLambda:
			// Immediately applied — walk body at current depth
			p.walkBody(proc.Body(), depth)
		case *ValidatedCaseLambda:
			// Immediately applied — walk each clause at current depth
			for _, clause := range proc.Clauses() {
				p.walkBody(clause.Body(), depth)
			}
		default:
			p.walkExpr(e.Proc(), depth)
		}
		// Walk args at current depth in all cases
		for _, arg := range e.Body() {
			p.walkExpr(arg, depth)
		}

	case *ValidatedIf:
		p.walkExpr(e.Test, depth)
		p.walkExpr(e.Conseq, depth)
		p.walkExpr(e.Alt, depth)

	case *ValidatedBegin:
		p.walkBody(e.Body(), depth)

	case *ValidatedSetBang:
		p.checkSymbol(e.Name, depth)
		p.walkExpr(e.SubExp(), depth)

	case *ValidatedLet:
		// Nested let: walk inits and body at current depth to find
		// references to the OUTER let's bindings through lambdas in
		// the inner scope. The inner let handles its own bindings via
		// its own markCapturedBindings call at validation time.
		for _, b := range e.Bindings {
			p.walkExpr(b.Init, depth)
		}
		p.walkBody(e.Body(), depth)

	case *ValidatedDynamicWind:
		p.walkExpr(e.Before, depth)
		p.walkExpr(e.Thunk, depth)
		p.walkExpr(e.After, depth)

	case *ValidatedWithContinuationMark:
		p.walkExpr(e.Key, depth)
		p.walkExpr(e.Val, depth)
		p.walkExpr(e.Body, depth)

	case *ValidatedApply:
		p.walkExpr(e.Proc, depth)
		for _, arg := range e.PrefixArgs {
			p.walkExpr(arg, depth)
		}
		p.walkExpr(e.FinalList, depth)

	case *ValidatedDefine:
		if e.IsFunction {
			// (define (f x) body) — the body is inside a closure
			p.walkBody(e.Body(), depth+1)
		} else {
			p.walkExpr(e.SubExp(), depth)
		}

	case *ValidatedQuote, *ValidatedLiteral, *ValidatedQuasiquote:
		// No sub-expressions to walk

	default:
		// Best-effort: unknown validated forms are conservatively ignored
		// so capture analysis cannot take down validation.
	}
}

func (p *captureWalker) walkBody(body []ValidatedExpr, depth int) {
	for _, expr := range body {
		p.walkExpr(expr, depth)
	}
}
