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

// WalkSubExprs calls fn for every direct sub-expression of expr.
//
// callPosition is true only for the operator of ValidatedCall and
// ValidatedApply. All other sub-expressions (arguments, bodies, inits,
// branch arms, etc.) pass callPosition=false.
//
// ValidatedSetBang: walks only the value expression (callPosition=false).
// The set! target is mutation (tracked by Mutable), not a reference.
//
// ValidatedSymbol has no children — fn is not called. The caller handles
// symbols directly before calling WalkSubExprs.
func WalkSubExprs(expr ValidatedExpr, fn func(child ValidatedExpr, callPosition bool)) {
	if expr == nil {
		return
	}
	switch e := expr.(type) {
	case *ValidatedCall:
		fn(e.Proc(), true)
		for _, arg := range e.Body() {
			fn(arg, false)
		}

	case *ValidatedApply:
		fn(e.Proc, true)
		for _, arg := range e.PrefixArgs {
			fn(arg, false)
		}
		fn(e.FinalList, false)

	case *ValidatedLambda:
		for _, b := range e.Body() {
			fn(b, false)
		}

	case *ValidatedCaseLambda:
		for _, clause := range e.Clauses() {
			for _, b := range clause.Body() {
				fn(b, false)
			}
		}

	case *ValidatedIf:
		fn(e.Test, false)
		fn(e.Conseq, false)
		fn(e.Alt, false)

	case *ValidatedBegin:
		for _, b := range e.Body() {
			fn(b, false)
		}

	case *ValidatedSetBang:
		fn(e.SubExp(), false)

	case *ValidatedLet:
		for _, b := range e.Bindings {
			fn(b.Init, false)
		}
		for _, b := range e.Body() {
			fn(b, false)
		}

	case *ValidatedDynamicWind:
		fn(e.Before, false)
		fn(e.Thunk, false)
		fn(e.After, false)

	case *ValidatedWithContinuationMark:
		fn(e.Key, false)
		fn(e.Val, false)
		fn(e.Body, false)

	case *ValidatedDefine:
		if e.IsFunction {
			for _, b := range e.Body() {
				fn(b, false)
			}
		} else {
			fn(e.SubExp(), false)
		}

	case *ValidatedQuote, *ValidatedLiteral, *ValidatedQuasiquote, *ValidatedSymbol:
		// No sub-expressions

	default:
		// Unknown form: no sub-expressions walked. If a new ValidatedExpr
		// type has children, add a case here. Analysis passes that use
		// WalkSubExprs will be incomplete until then.
	}
}
