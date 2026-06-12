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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// validateSetBang validates (set! name expr)
func validateSetBang(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "set!", 2, 2, result)
	if !ok {
		return nil
	}

	// Second element must be a symbol
	name, ok := elements[1].(*syntax.SyntaxSymbol)
	if !ok {
		result.addErrorf(source, "set!", "expected symbol as first argument to set!, got %T", elements[1])
		return nil
	}

	// Validate the value expression
	value := validateExpr(ctx, env, elements[2], result)
	if value == nil {
		return nil
	}

	// Resolve the target binding for mutability tracking.
	// Uses BindingID (frame + slot) for stable identity — *Binding pointers
	// into []Binding become stale when append reallocates the backing array.
	// Opportunistic: if resolution fails, the compiler catches the error.
	//
	// Local vs global: ResolveBindingID resolves locals only. A target that
	// resolves is a local mutation (tracked by BindingID). A target that does
	// NOT resolve is a global/top-level mutation — recorded by symbol key for
	// the define-stability pass, since top-level define bindings are created in
	// the compiler and are invisible to the validator's local resolver. With no
	// env to distinguish, fall back to marking the symbol (conservative).
	if env != nil {
		bid, ok := env.ResolveBindingID(name.Sym, name.Scopes())
		if ok {
			result.markMutated(bid)
		} else {
			result.markMutatedSymbol(name.Sym.Key)
		}
	} else {
		result.markMutatedSymbol(name.Sym.Key)
	}

	return &ValidatedSetBang{
		validatedBase: validatedBase{formName: "set!", source: source},
		Name:          name,
		subExp:        value,
	}
}
