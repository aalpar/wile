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
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
)

// CallbackIsCaptureSafe reports whether a callback ARGUMENT at a call site is
// provably capture-safe — the D2 proof the inline-HOF dispatch (callback
// specialization Strategy A) requires before inlining a curated tail HOF's
// reclaiming loop with this callback substituted. Inlining a capturing callback
// into a frame-reclaiming loop would be a use-after-release, so this answers true
// only with positive proof; everything uncertain is false (the soundness
// asymmetry — a false negative merely forgoes the optimization).
//
// Two positive forms:
//
//   - a symbol resolving to a binding that is IsCaptureSafe() AND IsStable() — the
//     same gate bodyCalleesAllCaptureSafe applies to a loop's callees: capture-safe
//     (cannot invoke a Scheme procedure, so cannot capture) and stable (not
//     rebindable to something that can), or
//   - a lambda literal whose body ProcedureBodyIsCaptureSafe proves cannot capture
//     the caller's continuation. The self symbol is nil — an argument callback is
//     anonymous, so no callee is exempted as a self-recursive tail call.
//
// Anything else — a procedure-invoking, unbound, or rebindable symbol; a lambda
// that runs a capture operator or a procedure-invoking callee; or a computed
// operator expression (neither symbol nor lambda) — returns false.
func CallbackIsCaptureSafe(arg ValidatedExpr, env *environment.EnvironmentFrame) bool {
	if env == nil {
		return false
	}
	switch a := arg.(type) {
	case *ValidatedSymbol:
		b := env.GetBinding(a.Symbol.Sym, syntax.ScopesOf(a.Symbol.Scopes()))
		return b != nil && b.IsCaptureSafe() && b.IsStable()
	case *ValidatedLambda:
		return ProcedureBodyIsCaptureSafe(a, nil, env)
	default:
		return false
	}
}
