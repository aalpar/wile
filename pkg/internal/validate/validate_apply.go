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
	"context"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
)

// validateApply validates (apply proc arg1 ... args)
//
// R7RS 6.10: apply calls proc with the arguments arg1 ... concatenated
// with the elements of args. At least two arguments are required (proc
// and the final list). Under-arity is a static error.
func validateApply(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "apply", 2, -1, result)
	if !ok {
		return nil
	}

	// elements[0] = "apply" keyword
	// elements[1] = proc
	// elements[2..n-1] = prefix args (optional)
	// elements[n] = final list (last argument)

	proc := validateExpr(ctx, env, elements[1], result)

	var prefixArgs []ValidatedExpr
	for i := 2; i < len(elements)-1; i++ {
		arg := validateExpr(ctx, env, elements[i], result)
		prefixArgs = append(prefixArgs, arg)
	}

	finalList := validateExpr(ctx, env, elements[len(elements)-1], result)

	if proc == nil || finalList == nil {
		return nil
	}
	for _, arg := range prefixArgs {
		if arg == nil {
			return nil
		}
	}

	return &ValidatedApply{
		validatedBase: validatedBase{formName: "apply", source: source},
		Proc:          proc,
		PrefixArgs:    prefixArgs,
		FinalList:     finalList,
	}
}
