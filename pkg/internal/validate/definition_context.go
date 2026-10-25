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

// checkDefinitionContexts refuses a definition outside a definition context
// (R7RS §5.3, §5.4, §5.5): the root of a unit, a body, or a begin that is itself
// in one. Anywhere else (a call argument, an if arm, a define's right-hand
// side) the form is an expression, and racket -I r5rs and Chez refuse it
// there, so (list (define x 1) x) is an error rather than (#<void> 1). The
// root's context is the caller's (ValidateExpressionIn).
//
// It also records, on each passthrough literal and quasiquote, whether it sits
// in an expression context: their code is compiled later as a tree of its own,
// and that tree's root is validated in the context recorded here.
//
// A pass over the finished tree and not a flag threaded through validateExpr:
// each node names its children's context, so no context can leak from a body
// into the arguments of a call inside it. Only a *ValidatedDefine and a
// define-syntax literal are checked, and the validator builds those only for a
// head that resolved to the form, so a call to a user procedure named define is
// never refused.
//
// The first refusal ends the walk, as it does in Racket: define-values and
// define-record-type expand to several defines, and one located error reads
// better than an unlocated list of them. It reports whether the walk may go on.
func checkDefinitionContexts(expr ValidatedExpr, definitionContext bool, result *ValidationResult) bool {
	switch e := expr.(type) {
	case *ValidatedDefine:
		if !definitionContext {
			result.addError(e.Source(), "define", "definition not allowed in an expression context")
			return false
		}
	case *ValidatedLiteral:
		if e.definesSyntax && !definitionContext {
			result.addError(e.Source(), "define-syntax", "definition not allowed in an expression context")
			return false
		}
		e.inExpression = !definitionContext
		return true
	case *ValidatedQuasiquote:
		// `,(define x 1) is the define itself, in the quasiquote's place.
		e.inExpression = !definitionContext
		return true
	case *ValidatedBegin:
		return checkEachDefinitionContext(e.Body(), definitionContext, result)
	case *ValidatedLet:
		for _, b := range e.Bindings {
			ok := checkDefinitionContexts(b.Init, false, result)
			if !ok {
				return false
			}
		}
		return checkEachDefinitionContext(e.Body(), true, result)
	}
	ok := true
	WalkSubExprs(expr, func(child ValidatedExpr, role ChildRole) {
		if ok {
			ok = checkDefinitionContexts(child, role == RoleClosureBody, result)
		}
	})
	return ok
}

func checkEachDefinitionContext(exprs []ValidatedExpr, definitionContext bool, result *ValidationResult) bool {
	for _, e := range exprs {
		ok := checkDefinitionContexts(e, definitionContext, result)
		if !ok {
			return false
		}
	}
	return true
}

// BodyBindsNothing reports whether body, a <body>, makes no definition of its
// own: no define and no define-syntax at its top or in a begin spliced there.
// Such a body binds nothing, so (let () body ...) needs no frame of its own.
func BodyBindsNothing(body []ValidatedExpr) bool {
	for _, e := range body {
		switch v := e.(type) {
		case *ValidatedDefine:
			return false
		case *ValidatedLiteral:
			if v.definesSyntax {
				return false
			}
		case *ValidatedBegin:
			if !BodyBindsNothing(v.Body()) {
				return false
			}
		}
	}
	return true
}
