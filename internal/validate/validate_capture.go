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

// markCapturedBindings walks the validated body (and optionally init
// expressions) to determine which let bindings are referenced from inside
// escaping closures. A lambda is non-escaping only when it appears as the
// operator of a ValidatedCall (immediately-applied lambda) — that case is
// handled by WalkBindingRefs which does not increment depth across an
// immediately-applied lambda.
//
// Capture predicate: any reference (including set!-target) at depth > 0.
// The role of the reference is irrelevant — what matters is whether the
// reference appears inside an escaping closure.
//
// walkInits should be true for let*, letrec, and letrec* (where inits see the
// bindings) and false for plain let (where inits are in the outer scope).
//
// Best-effort: if binding resolution fails (scope mismatch), the binding
// stays non-captured. Must not gate correctness-critical optimizations
// without re-validation.
func markCapturedBindings(
	childEnv *environment.EnvironmentFrame,
	bindings []ValidatedLetBinding,
	body []ValidatedExpr,
	walkInits bool,
) {
	if childEnv == nil || len(bindings) == 0 {
		return
	}
	idToIdx := buildBindingIdxMap(childEnv, bindings)
	if len(idToIdx) == 0 {
		return
	}

	visit := func(sym *syntax.SyntaxSymbol, _ RefRole, depth int) {
		if depth <= 0 {
			return
		}
		bid, ok := childEnv.ResolveBindingID(sym.Sym, sym.Scopes())
		if !ok {
			return
		}
		idx, found := idToIdx[bid]
		if !found {
			return
		}
		bindings[idx].Captured = true
	}

	if walkInits {
		for _, b := range bindings {
			WalkBindingRefs(b.Init, visit)
		}
	}
	for _, expr := range body {
		WalkBindingRefs(expr, visit)
	}
}
