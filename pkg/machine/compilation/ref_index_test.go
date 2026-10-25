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
	"reflect"
	"sort"
	"strings"
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

	// The bucket is populated three times over: the user wrote `hidden` twice
	// inside a closure, once as a set! target, and the define-syntax form is an
	// opaque subtree whose template mentions it a third time. A spelling-keyed
	// answer would be true for every one of the three questions below.
	c.Assert(len(refs["hidden"]), qt.Equals, 3)

	c.Assert(refs.capturedBy(macroHidden), qt.IsFalse)
	c.Assert(refs.assignedBy(macroHidden), qt.IsFalse)
	c.Assert(refs.binderIsBoxed(macroHidden), qt.IsFalse)

	// And the binder really is the macro's, not a same-named user binding that
	// happens to be unreferenced — it carries a scope NO occurrence carries.
	// Asserted over the whole bucket rather than one index: the occurrences come
	// from two walks now (the validated tree and the opaque scan), so their order
	// is not a fact this test should depend on.
	c.Assert(macroHidden.Scopes().Len() > 0, qt.IsTrue)
	for i, r := range refs["hidden"] {
		c.Assert(syntax.ScopesCompatible(macroHidden.Scopes(), r.sym.Scopes()), qt.IsFalse,
			qt.Commentf("occurrence %d", i))
	}
}

// TestRefIndexEmptyRegionAndNilLookups pins that an empty region is empty rather than
// universally matching, which is the failure mode a nil-tolerant probe invites.
func TestRefIndexEmptyRegionAndNilLookups(t *testing.T) {
	c := qt.New(t)
	body := bodyOfLambda(t, `(lambda () (define a 1) a)`)
	refs := newRefIndex(nil)

	c.Assert(len(refs), qt.Equals, 0)
	c.Assert(refs.capturedBy(binderNamed(t, body, "a")), qt.IsFalse)
	c.Assert(refs.assignedBy(nil), qt.IsFalse)
	c.Assert(newRefIndex(body).capturedBy(nil), qt.IsFalse)
}

// refIndexAllocRegion builds a lambda body of n calls, each applying k
// parameters to one another, so the region holds exactly k distinct spellings
// and k*n occurrences of them. Parameters are binders, not references.
func refIndexAllocRegion(t *testing.T, k, n int) []validate.ValidatedExpr {
	t.Helper()
	names := make([]string, k)
	for i := range names {
		names[i] = string(rune('a' + i))
	}
	params := strings.Join(names, " ")
	var b strings.Builder
	b.WriteString("(lambda (" + params + ")")
	for range n {
		b.WriteString(" (" + params + ")")
	}
	b.WriteString(")")
	return bodyOfLambda(t, b.String())
}

// assertOneBlock asserts that the index's per-spelling slices are exactly full
// and tile one contiguous backing array: sorted by address, each begins where
// the previous one ends. This is the structural witness of "one block"; the
// allocation count only catches a make per spelling when K is large enough for
// the extra allocations to show (at K=1 both cost 3).
//
// Its limit: where 24*n lands on a Go size class, separate allocations can sit
// adjacent and pass as one block. False passes were seen at n <= 8 and none at
// n=50 or n=200, which are the sizes the callers that rely on contiguity use.
func assertOneBlock(c *qt.C, refs refIndex) {
	c.Helper()
	type span struct {
		lo, n, capacity uintptr
	}
	spans := make([]span, 0, len(refs))
	for _, occ := range refs {
		spans = append(spans, span{
			lo:       reflect.ValueOf(occ).Pointer(),
			n:        uintptr(len(occ)),
			capacity: uintptr(cap(occ)),
		})
	}
	sort.Slice(spans, func(i, j int) bool {
		return spans[i].lo < spans[j].lo
	})
	size := reflect.TypeFor[binderRef]().Size()
	for i, s := range spans {
		c.Assert(s.capacity, qt.Equals, s.n, qt.Commentf("span %d", i))
		if i == 0 {
			continue
		}
		prev := spans[i-1]
		c.Assert(s.lo, qt.Equals, prev.lo+prev.n*size, qt.Commentf("span %d does not start where span %d ends", i, i-1))
	}
}

// TestNewRefIndexAllocatesPerKeyNotPerOccurrence pins that building the index
// costs a constant number of allocations, not one per occurrence or per growth
// step.
//
// The per-key append grew each spelling's slice by doubling: 1600 occurrences
// over 8 spellings cost 74 allocations on master and cost 3 here (the index
// map's header plus its one Swiss-table group, and the block; the count map
// stays on the stack for K <= 8). The bound of 4 holds for K <= 8, where a map
// fits one group (K=9 measures 8); it is an upper bound, not an equality,
// because map allocation differs by runtime version. One make per spelling
// measures 11 at K=8, so the bound catches it here, but not in general (at K=1
// both cost 3); assertOneBlock is the structural witness.
func TestNewRefIndexAllocatesPerKeyNotPerOccurrence(t *testing.T) {
	if inliningDisabled() {
		// With -gcflags=-l (GoLand's debug runs, dlv) the walk's per-occurrence
		// values escape through calls that are no longer inlined: K=8 N=1600
		// measures 410, and the count scales with N, so no bound or ratio
		// survives. The bound is a property of the optimised build.
		t.Skip("allocation bound assumes an inlining build; this binary was built with -l")
	}
	c := qt.New(t)
	const k = 8
	const n = 200
	body := refIndexAllocRegion(t, k, n)

	var refs refIndex
	allocs := testing.AllocsPerRun(20, func() {
		refs = newRefIndex(body)
	})

	total := 0
	for _, occ := range refs {
		total += len(occ)
	}
	c.Assert(len(refs), qt.Equals, k)
	c.Assert(total, qt.Equals, k*n)
	assertOneBlock(c, refs)

	const bound = 4.0
	c.Assert(allocs <= bound, qt.IsTrue,
		qt.Commentf("K=%d N=%d allocs=%v bound=%v", k, k*n, allocs, bound))
}

// inlineProbe is returned by pointer from newInlineProbe: inlined, the value
// stays on the caller's stack; called, it escapes to the heap.
type inlineProbe struct {
	n int
}

func newInlineProbe() *inlineProbe {
	return &inlineProbe{n: 1}
}

// inliningDisabled reports whether this test binary was compiled with inlining
// off, by whether a call that inlining makes allocation-free allocates.
func inliningDisabled() bool {
	allocs := testing.AllocsPerRun(10, func() {
		p := newInlineProbe()
		p.n++
	})
	return allocs > 0
}

// TestNewRefIndexContents pins what the allocation strategy must not move: each
// spelling's occurrences in walk order, in slices that tile one block.
func TestNewRefIndexContents(t *testing.T) {
	c := qt.New(t)
	const k = 8
	const n = 50
	refs := newRefIndex(refIndexAllocRegion(t, k, n))

	c.Assert(len(refs), qt.Equals, k)
	assertOneBlock(c, refs)
	for key, occ := range refs {
		c.Assert(len(occ), qt.Equals, n, qt.Commentf("key %q", key))
		for i, r := range occ {
			c.Assert(r.sym.Sym.Key, qt.Equals, key)
			c.Assert(r.origin, qt.Equals, i, qt.Commentf("key %q occurrence %d", key, i))
		}
	}
}

// TestNewRefIndexSizesEveryOccurrenceKind pins that the sizing pass counts the
// same occurrences the filling pass stores, for each kind newRefIndex records:
// a read, a set! target, and a live symbol inside an opaque subtree. A kind the
// count misses leaves its spelling's slice too small, so the fill reallocates
// it out of the block, which assertOneBlock reports as a gap or a spare
// capacity.
func TestNewRefIndexSizesEveryOccurrenceKind(t *testing.T) {
	c := qt.New(t)
	body := bodyOfLambda(t, "(lambda (a b c) (set! a 1) (set! a 2) (a b) (quasiquote ((unquote (set! b 1)) (unquote (set! b 2)) (unquote (set! b 3)) (unquote c))) (c))")
	refs := newRefIndex(body)

	c.Assert(len(refs) > 0, qt.IsTrue)
	assertOneBlock(c, refs)
}

// TestNewRefIndexSkipsNilTargets pins the nil guard on the walk's emit: a set!
// the validator left without a target names no binding and is not indexed.
func TestNewRefIndexSkipsNilTargets(t *testing.T) {
	c := qt.New(t)
	refs := newRefIndex([]validate.ValidatedExpr{&validate.ValidatedSetBang{}})

	c.Assert(len(refs), qt.Equals, 0)
}
