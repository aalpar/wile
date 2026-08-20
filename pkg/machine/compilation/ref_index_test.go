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

package compilation

import (
	"testing"

	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/syntax"

	qt "github.com/frankban/quicktest"
)

// binderNamed returns the SyntaxSymbol that binds name among a body's internal
// defines. Queries take a BINDER, not a reference, so a fixture cannot be
// written by picking any occurrence of the spelling.
func binderNamed(t *testing.T, body []validate.ValidatedExpr, name string) *syntax.SyntaxSymbol {
	t.Helper()
	for _, b := range bodyBindersOfRegion(body) {
		if b.name != nil && b.name.Sym.Key == name {
			return b.name
		}
	}
	t.Fatalf("no internal define binds %q in the fixture", name)
	return nil
}

// TestRefIndexPositions pins the three position questions the boxing pass asks,
// on the distinctions that decide them.
//
// These used to be three walks of the region per binder (referencesBinder /
// assignsBinder); they are now one walk plus a probe. The answers must not have
// moved, so each row states a case where a coarser index would answer
// differently.
func TestRefIndexPositions(t *testing.T) {
	tcs := []struct {
		name string
		code string
		// bind is the internal define whose binder is queried.
		bind             string
		captured         bool
		assigned         bool
		outsideAnyClosur bool
	}{
		{
			// The default: read where it is written, never captured, never
			// assigned. Nothing boxes.
			name:             "plain read outside any closure",
			code:             `(lambda () (define a 1) (define b a) b)`,
			bind:             "a",
			captured:         false,
			assigned:         false,
			outsideAnyClosur: true,
		},
		{
			// RoleClosureBody is the boundary, and an IMMEDIATELY APPLIED
			// lambda is still a closure body — it is not evaluated where it is
			// written, even though it never escapes.
			name:             "read inside an immediately-applied lambda counts as captured",
			code:             `(lambda () (define a 1) ((lambda () a)))`,
			bind:             "a",
			captured:         true,
			assigned:         false,
			outsideAnyClosur: false,
		},
		{
			// A set! target IS a reference: a write reaches the same location a
			// read does. Both answers must be true, not just assigned.
			name:             "a set! inside a closure is both a capture and an assignment",
			code:             `(lambda () (define a 1) (define f (lambda () (set! a 2))) (f))`,
			bind:             "a",
			captured:         true,
			assigned:         true,
			outsideAnyClosur: false,
		},
		{
			// The set! sits in no lambda at all, so it is an assignment and NOT
			// a capture. This is the direction the depth-qualified formulation
			// binderIsBoxed replaced used to miss.
			name:             "a set! outside every closure assigns without capturing",
			code:             `(lambda () (define a 1) (set! a 2) a)`,
			bind:             "a",
			captured:         false,
			assigned:         true,
			outsideAnyClosur: true,
		},
		{
			// Captured and assigned, but by different forms. binderIsBoxed is
			// the conjunction over the whole region, not over one expression.
			name:             "captured by one form and assigned by another is both",
			code:             `(lambda () (define a 1) (define f (lambda () a)) (set! a 2) (f))`,
			bind:             "a",
			captured:         true,
			assigned:         true,
			outsideAnyClosur: true,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			body := bodyOfLambda(t, tc.code)
			refs := newRefIndex(body)
			binder := binderNamed(t, body, tc.bind)

			c.Assert(refs.capturedBy(binder), qt.Equals, tc.captured)
			c.Assert(refs.assignedBy(binder), qt.Equals, tc.assigned)
			c.Assert(refs.referencedOutsideClosure(binder), qt.Equals, tc.outsideAnyClosur)
			c.Assert(refs.binderIsBoxed(binder), qt.Equals, tc.captured && tc.assigned)
		})
	}
}

// TestRefIndexOriginIsolation pins that a per-origin query answers about ONE
// expression of the region and not about the region.
//
// This is what letrecRegionTier needs, and it is the property a spelling-keyed
// index could most easily lose: every occurrence of `a` lands in one bucket, so
// origin has to survive into the entry rather than being implied by it.
func TestRefIndexOriginIsolation(t *testing.T) {
	c := qt.New(t)
	body := bodyOfLambda(t, `(lambda ()
	                           (define a 1)
	                           (define b (lambda () a))
	                           (define d 3)
	                           (b))`)
	refs := newRefIndex(body)
	a := binderNamed(t, body, "a")

	// Body element 1 is `(define b (lambda () a))` — the only one referencing a,
	// and only from inside a closure.
	c.Assert(refs.referencedFrom(a, 1, refAnywhere), qt.IsTrue)
	c.Assert(refs.referencedFrom(a, 1, refInsideClosure), qt.IsTrue)
	c.Assert(refs.referencedFrom(a, 1, refOutsideClosure), qt.IsFalse)

	// Element 0 is a's own define, element 2 is an unrelated one.
	c.Assert(refs.referencedFrom(a, 0, refAnywhere), qt.IsFalse)
	c.Assert(refs.referencedFrom(a, 2, refAnywhere), qt.IsFalse)

	// And an origin outside the region matches nothing rather than everything.
	c.Assert(refs.referencedFrom(a, 99, refAnywhere), qt.IsFalse)
}

// TestRefIndexDiscriminatesByScopeNotSpelling is the binding-identity ratchet.
//
// Two identifiers spelled the same land in ONE bucket of a spelling-keyed index,
// so the subset test has to run per occurrence — the bucket is the narrowing,
// never the decision. This fixture makes the two disagree: `def-hidden`
// introduces its `hidden` binder from the template, so that binder carries the
// expansion's intro scope, while the reference in `shown`'s body carries only
// the scopes the user wrote. binderScopes ⊄ refScopes, so it must NOT match.
//
// Reduce this to a name comparison and the query answers TRUE, and the boxing
// pass installs a cell for a slot no closure can reach — the failure the root
// CLAUDE.local.md's "Binding Identity" section forbids. No value assertion
// catches it, because over-boxing evaluates to the same values.
//
// The OPPOSITE direction is deliberately not asserted here: a reference shadowed
// between the query and the binder still satisfies the subset test and counts.
// That over-approximation is stated at binderIsBoxed and is the safe direction.
func TestRefIndexDiscriminatesByScopeNotSpelling(t *testing.T) {
	c := qt.New(t)
	body := bodyOfLambda(t, `(lambda ()
	                           (define-syntax def-hidden
	                             (syntax-rules ()
	                               ((_ v) (define hidden v))))
	                           (def-hidden 1)
	                           (define shown (lambda () (set! hidden 3) hidden))
	                           (shown))`)
	refs := newRefIndex(body)
	macroHidden := binderNamed(t, body, "hidden")

	// The bucket is populated: the user DID write `hidden`, twice, inside a
	// closure, once as a set! target. A spelling-keyed answer would be true for
	// every one of the three questions below.
	c.Assert(len(refs["hidden"]), qt.Equals, 2)

	c.Assert(refs.capturedBy(macroHidden), qt.IsFalse)
	c.Assert(refs.assignedBy(macroHidden), qt.IsFalse)
	c.Assert(refs.binderIsBoxed(macroHidden), qt.IsFalse)

	// And the binder really is the macro's, not a same-named user binding that
	// happens to be unreferenced — it carries a scope the reference does not.
	c.Assert(len(macroHidden.Scopes()) > 0, qt.IsTrue)
	c.Assert(syntax.ScopesCompatible(macroHidden.Scopes(), refs["hidden"][0].sym.Scopes()),
		qt.IsFalse)
}

// TestRefIndexIgnoresNilTargets pins that an empty region is empty rather than
// universally matching, which is the failure mode a nil-tolerant probe invites.
func TestRefIndexIgnoresNilTargets(t *testing.T) {
	c := qt.New(t)
	body := bodyOfLambda(t, `(lambda () (define a 1) a)`)
	refs := newRefIndex(nil)

	c.Assert(len(refs), qt.Equals, 0)
	c.Assert(refs.capturedBy(binderNamed(t, body, "a")), qt.IsFalse)
	c.Assert(refs.assignedBy(nil), qt.IsFalse)
	c.Assert(newRefIndex(body).capturedBy(nil), qt.IsFalse)
}
