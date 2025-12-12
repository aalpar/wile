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

// validateCall validates (proc arg...)
func validateCall(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "call", "function call must be a proper list")
		return nil
	}

	// Need at least the procedure
	if len(elements) < 1 {
		result.addError(source, "call", "empty application")
		return nil
	}

	// Validate the procedure expression
	proc := validateExpr(elements[0], result)

	// Validate all arguments
	var args []ValidatedExpr
	for i := 1; i < len(elements); i++ {
		arg := validateExpr(elements[i], result)
		if arg != nil {
			args = append(args, arg)
		}
	}

	// If proc or any arg validation failed, return nil
	expectedArgCount := len(elements) - 1
	if proc == nil || (len(args) != expectedArgCount && expectedArgCount > 0) {
		return nil
	}

	return &ValidatedCall{
		source: source,
		Proc:   proc,
		Args:   args,
	}
}
