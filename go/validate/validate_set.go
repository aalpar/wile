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

// validateSetBang validates (set! name expr)
func validateSetBang(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "set!", "set! form must be a proper list")
		return nil
	}

	// elements[0] is 'set!', need exactly name and value
	if len(elements) != 3 {
		result.addErrorf(source, "set!", "set! requires exactly 2 arguments, got %d", len(elements)-1)
		return nil
	}

	// Second element must be a symbol
	name, ok := asSyntaxSymbol(elements[1])
	if !ok {
		result.addErrorf(source, "set!", "expected symbol as first argument to set!, got %T", elements[1])
		return nil
	}

	// Validate the value expression
	value := validateExpr(elements[2], result)
	if value == nil {
		return nil
	}

	return &ValidatedSetBang{
		source: source,
		Name:   name,
		Value:  value,
	}
}
