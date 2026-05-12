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

// RefRole describes the structural slot a symbol reference occupies.
// It is the "what role does this use play" axis, orthogonal to depth.
type RefRole int

const (
	// RefInBody is a normal-position reference: argument, return value,
	// init expression, branch arm, sequence body, set! value, etc.
	// This is the default for any non-call-proc, non-set!-target reference.
	RefInBody RefRole = iota

	// RefInCallProc is the operator position of a ValidatedCall or
	// ValidatedApply (the "callee" slot).
	RefInCallProc

	// RefSetBangTarget is the target name of a ValidatedSetBang (the
	// symbol being mutated). The value expression of the set! is walked
	// separately as RefInBody.
	RefSetBangTarget
)

// WalkBindingRefs walks expr recursively, calling visit for every
// *syntax.SyntaxSymbol reference encountered, with its role and
// closure-nesting depth.
//
// depth is the number of escaping closure boundaries crossed (0 = same
// closure as expr). Immediately-applied lambdas (ValidatedLambda or
// ValidatedCaseLambda as the Proc of a ValidatedCall or ValidatedApply)
// do NOT increment depth, since the closure does not escape.
//
// ValidatedSetBang emits a synthetic RefSetBangTarget visit for its
// target name, then recurses into its value expression as a normal walk
// (the value's symbol references appear as RefInBody at the same depth).
//
// Symbols are NEVER yielded with RefInClosureBody; "is this inside a
// closure?" is the depth > 0 predicate.
func WalkBindingRefs(
	expr ValidatedExpr,
	visit func(sym *syntax.SyntaxSymbol, role RefRole, depth int),
) {
	walkBindingRefsAt(expr, RefInBody, 0, visit)
}

// walkBindingRefsAt is the recursive driver. role describes the slot
// expr occupies in its parent; depth is the closure-nesting count
// already crossed.
func walkBindingRefsAt(
	expr ValidatedExpr,
	role RefRole,
	depth int,
	visit func(sym *syntax.SyntaxSymbol, role RefRole, depth int),
) {
	if expr == nil {
		return
	}

	// Symbol leaf: yield at the current role/depth.
	sym, ok := expr.(*ValidatedSymbol)
	if ok {
		visit(sym.Symbol, role, depth)
		return
	}

	// set! target: emit a synthetic RefSetBangTarget event for the
	// target name, then fall through to WalkSubExprs (which yields the
	// value expression as a child).
	setBang, ok := expr.(*ValidatedSetBang)
	if ok {
		visit(setBang.Name, RefSetBangTarget, depth)
	}

	WalkSubExprs(expr, func(child ValidatedExpr, childRole ChildRole) {
		switch childRole {
		case RoleClosureBody:
			// Descend into a closure body — depth increases by one.
			walkBindingRefsAt(child, RefInBody, depth+1, visit)
		case RoleCallProc:
			// Immediately-applied lambda optimization: when the callee
			// IS a lambda, the closure does not escape — walk its body
			// at the CURRENT depth (not depth+1).
			switch proc := child.(type) {
			case *ValidatedLambda:
				for _, b := range proc.Body() {
					walkBindingRefsAt(b, RefInBody, depth, visit)
				}
			case *ValidatedCaseLambda:
				for _, clause := range proc.Clauses() {
					for _, b := range clause.Body() {
						walkBindingRefsAt(b, RefInBody, depth, visit)
					}
				}
			default:
				// Normal call-proc: tag with RefInCallProc so the
				// visitor can distinguish "called" from "used".
				walkBindingRefsAt(child, RefInCallProc, depth, visit)
			}
		default:
			walkBindingRefsAt(child, RefInBody, depth, visit)
		}
	})
}

// buildBindingIdxMap resolves each binding's name (under childEnv) to
// its BindingID, returning a BindingID → bindings-slice-index map.
// Bindings whose names fail to resolve under childEnv are silently
// dropped — best-effort, matching the prior walker behavior.
func buildBindingIdxMap(
	childEnv *environment.EnvironmentFrame,
	bindings []ValidatedLetBinding,
) map[environment.BindingID]int {
	idToIdx := make(map[environment.BindingID]int, len(bindings))
	for i, b := range bindings {
		bid, ok := childEnv.ResolveBindingID(b.Name.Sym, b.Name.Scopes())
		if ok {
			idToIdx[bid] = i
		}
	}
	return idToIdx
}
