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

// ChildRole describes the structural position of a sub-expression within its
// parent validated form.
type ChildRole int

const (
	// RoleNormal is the default: arguments, init expressions, branch arms,
	// body of begin/let/dynamic-wind/with-continuation-mark, define-variable value.
	RoleNormal ChildRole = iota

	// RoleCallProc is the operator position of ValidatedCall and ValidatedApply.
	RoleCallProc

	// RoleClosureBody is a body expression inside a closure boundary:
	// ValidatedLambda, ValidatedCaseLambda clause, or ValidatedDefine (function form).
	RoleClosureBody
)

// WalkSubExprs calls fn for every direct sub-expression of expr, reporting
// each child's structural role:
//
//   - RoleCallProc: the operator of ValidatedCall and ValidatedApply
//   - RoleClosureBody: body of ValidatedLambda, ValidatedCaseLambda clause,
//     or ValidatedDefine (function form)
//   - RoleNormal: everything else (arguments, inits, branches, sequence bodies)
//
// ValidatedSetBang: walks only the value expression (RoleNormal).
// The set! target is mutation (tracked by Mutable), not a reference.
//
// ValidatedSymbol has no children — fn is not called. The caller handles
// symbols directly before calling WalkSubExprs.
func WalkSubExprs(expr ValidatedExpr, fn func(child ValidatedExpr, role ChildRole)) {
	if expr == nil {
		return
	}
	switch e := expr.(type) {
	case *ValidatedCall:
		fn(e.Proc(), RoleCallProc)
		for _, arg := range e.Body() {
			fn(arg, RoleNormal)
		}

	case *ValidatedApply:
		fn(e.Proc, RoleCallProc)
		for _, arg := range e.PrefixArgs {
			fn(arg, RoleNormal)
		}
		fn(e.FinalList, RoleNormal)

	case *ValidatedLambda:
		for _, b := range e.Body() {
			fn(b, RoleClosureBody)
		}

	case *ValidatedCaseLambda:
		for _, clause := range e.Clauses() {
			for _, b := range clause.Body() {
				fn(b, RoleClosureBody)
			}
		}

	case *ValidatedIf:
		fn(e.Test, RoleNormal)
		fn(e.Conseq, RoleNormal)
		fn(e.Alt, RoleNormal)

	case *ValidatedBegin:
		for _, b := range e.Body() {
			fn(b, RoleNormal)
		}

	case *ValidatedSetBang:
		fn(e.SubExp(), RoleNormal)

	case *ValidatedLet:
		for _, b := range e.Bindings {
			fn(b.Init, RoleNormal)
		}
		for _, b := range e.Body() {
			fn(b, RoleNormal)
		}

	case *ValidatedDynamicWind:
		fn(e.Before, RoleNormal)
		fn(e.Thunk, RoleNormal)
		fn(e.After, RoleNormal)

	case *ValidatedWithContinuationMark:
		fn(e.Key, RoleNormal)
		fn(e.Val, RoleNormal)
		fn(e.Body, RoleNormal)

	case *ValidatedDefine:
		if e.IsFunction {
			for _, b := range e.Body() {
				fn(b, RoleClosureBody)
			}
		} else {
			fn(e.SubExp(), RoleNormal)
		}

	case *ValidatedQuote, *ValidatedLiteral, *ValidatedQuasiquote, *ValidatedSymbol:
		// No sub-expressions

	default:
		// ADDING A NEW ValidatedExpr TYPE
		//
		// This switch is the SINGLE registration point for tree-walking
		// analysis in the validate package. Both markCapturedBindings
		// and markEscapedBindings traverse through WalkBindingRefs,
		// which in turn dispatches through this switch via WalkSubExprs.
		// A new ValidatedExpr type without a case here will silently
		// skip both analyses — bindings referenced inside the new form
		// will not be marked Captured or Escapes, and the compiler will
		// receive valid-but-under-marked bytecode (closures missing
		// capture slots, stack allocation where heap is needed).
		//
		// When adding a new ValidatedExpr type with child expressions:
		//   1. Add a case above with the correct ChildRole for each
		//      sub-expression (RoleNormal / RoleCallProc / RoleClosureBody).
		//   2. Update WalkBindingRefs's switch (walk_binding_refs.go)
		//      if the new form needs role-tagging beyond the default.
		//   3. Add the form to walk_sub_exprs_test.go's shape table.
	}
}
