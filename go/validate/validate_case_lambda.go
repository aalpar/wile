// Copyright 2025 Aaron Alpar
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
	"wile/syntax"
)

// validateCaseLambda validates (case-lambda [clause] ...)
// Each clause is (params body...) like a lambda without the 'lambda' keyword
func validateCaseLambda(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "case-lambda", "case-lambda form must be a proper list")
		return nil
	}

	// elements[0] is 'case-lambda', need at least one clause
	if len(elements) < 2 {
		result.addError(source, "case-lambda", "case-lambda requires at least one clause")
		return nil
	}

	var clauses []*ValidatedCaseLambdaClause
	for i := 1; i < len(elements); i++ {
		clause := validateCaseLambdaClause(elements[i], result)
		if clause != nil {
			clauses = append(clauses, clause)
		}
	}

	// If any clause validation failed, return nil
	if len(clauses) != len(elements)-1 {
		return nil
	}

	return &ValidatedCaseLambda{
		source:  source,
		Clauses: clauses,
	}
}

// validateCaseLambdaClause validates a single case-lambda clause: (params body...)
func validateCaseLambdaClause(expr syntax.SyntaxValue, result *ValidationResult) *ValidatedCaseLambdaClause {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		result.addErrorf(getSourceContext(expr), "case-lambda", "clause must be a list, got %T", expr)
		return nil
	}

	source := pair.SourceContext()

	// Collect all elements
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "case-lambda", "clause must be a proper list")
		return nil
	}

	// Need params and at least one body expression
	if len(elements) < 2 {
		result.addErrorf(source, "case-lambda", "clause requires parameters and at least one body expression, got %d parts", len(elements))
		return nil
	}

	// Validate parameters
	params := validateParams(elements[0], "case-lambda", result)

	// Validate body - must have at least one expression
	var body []ValidatedExpr
	for i := 1; i < len(elements); i++ {
		bodyExpr := validateExpr(elements[i], result)
		if bodyExpr != nil {
			body = append(body, bodyExpr)
		}
	}

	// If body validation failed, return nil
	if len(body) != len(elements)-1 {
		return nil
	}

	return &ValidatedCaseLambdaClause{
		Params: params,
		Body:   body,
	}
}
