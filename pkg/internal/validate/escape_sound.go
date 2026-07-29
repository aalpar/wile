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
	"github.com/aalpar/wile/pkg/syntax"
)

// letBoundClosureEscapes reports whether the lambda bound by the i-th binding of
// v can outlive the enclosing call. It is the sound sibling of
// markEscapedBindings: that one is best-effort and leaves an unresolved
// reference NON-escaping (validate_escape.go — the ResolveBindingID miss just
// returns), which is the wrong direction for anything gating frame reuse. This
// one answers "does not escape" only from positive evidence.
//
// The rule is one line of semantics: a let-bound lambda does not escape iff
// EVERY occurrence of its name within the let form is in call-operator position.
// Called is not escaping; anything else — stored, returned, passed as an
// argument, set! — is.
//
// WHICH OCCURRENCES COUNT, and why the test is sound without resolving anything.
// An occurrence is treated as a reference to this binding when the names match
// AND the binder's scopes are compatible with the occurrence's — Flatt's rule,
// bindingScopes ⊆ useScopes, via the same ScopesCompatible the environment's
// resolveLocal uses, so this cannot drift from real resolution. Both halves are
// pure functions of two syntax objects, so no *EnvironmentFrame is needed and
// bodyCreatesEscapingClosure keeps its body-only signature.
//
// This is an over-approximation in the SAFE direction, in both halves. A genuine
// reference to this binding necessarily carries this name and necessarily
// satisfies the subset relation — that is what makes it resolve here — so no
// reference can be missed. What is over-counted is an occurrence that passes both
// tests yet resolves to a NEARER shadowing binder; deciding which of several
// candidate binders wins is the only part that would need the environment, and
// skipping it can only add occurrences. Since an added occurrence can only set
// the verdict to "escapes", the imprecision costs a forgone optimization, never a
// missed escape.
//
// That also makes the plan's "resolution failure implies escaped" clause vacuous
// rather than implemented: there is no resolution step to fail, so there is no
// fail-open default to invert.
//
// SELF-REFERENCE IS NOT AN ESCAPE, and the co-induction is the reason. A named
// let is a letrec whose single binding is the loop lambda, so the lambda's own
// body calls its own name from inside itself (at closure depth 1). That
// reference is in call position, so it is admitted under the assumption that
// the binding does not escape — which is exactly what is being proven. The
// assumption is discharged by every OTHER occurrence also being a call.
//
// Depth needs no separate treatment. A reference from inside some other closure
// only outlives the frame if that closure escapes, and a closure this predicate
// has not cleared is still rejected outright by exprCreatesEscapingClosure —
// so the relaxation composes by induction over nesting rather than needing to
// reason about depth here.
func letBoundClosureEscapes(v *ValidatedLet, i int) bool {
	switch v.Bindings[i].Init.(type) {
	case *ValidatedLambda, *ValidatedCaseLambda:
	default:
		// Not a closure binding: nothing for this predicate to clear, and the
		// caller must not treat the init as an applied lambda.
		return true
	}

	binder := v.Bindings[i].Name
	escapes := false
	visit := func(sym *syntax.SyntaxSymbol, role RefRole, _ int) {
		if escapes {
			return
		}
		if sym.Sym.Key != binder.Sym.Key {
			return
		}
		if !syntax.ScopesCompatible(binder.Scopes(), sym.Scopes()) {
			return
		}
		// RefInBody covers stored / returned / passed-as-argument;
		// RefSetBangTarget means the name may later denote a different
		// procedure entirely, so the init stops describing what the name is.
		if role != RefInCallProc {
			escapes = true
		}
	}

	// Every binding init, plus the body. For letrec-family inits the bindings
	// are in scope, so a sibling init can reference this one; for a plain let
	// they are not, and an occurrence there belongs to an outer binding — walking
	// it anyway can only refuse, per the soundness note above.
	for _, b := range v.Bindings {
		WalkBindingRefs(b.Init, visit)
	}
	for _, e := range v.Body() {
		WalkBindingRefs(e, visit)
	}
	return escapes
}
