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

	// Precise: when this set! targets a resolved local slot, record it by its
	// LocalRef (read by markMutableBindings to flag mutable let bindings). A
	// non-local target is left to the conservative by-name mark below, so the
	// global arm is not double-recorded here.
	if env != nil {
		ref := env.ResolveBindingRef(name.Sym, syntax.ScopesOf(name.Scopes()))
		if ref.IsLocal() {
			result.markMutated(ref)
		}
	}

	// Conservative: also mark the bare name unconditionally (even with no env,
	// and even when the target above resolved to a local shadow), so a
	// top-level set! inside a (begin ...) unit — whose global binding does not
	// yet exist at validation time — still marks that name non-stable. This
	// over-approximation is what keeps the frame-reclaim Stable stamp sound.
	// Powers StableInUnit (see finalizeStability).
	result.markMutated(environment.GlobalRef(name.Key()))

	return &ValidatedSetBang{
		formName: "set!", source: source,
		Name:   name,
		subExp: value,
	}
}
