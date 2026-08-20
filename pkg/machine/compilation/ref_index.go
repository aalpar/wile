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

// ref_index.go turns the boxing pass's reference questions from a walk per
// question into one walk per region.
//
// THE SHAPE THIS REPLACES. binderIsBoxed, letrecRegionTier and
// letrecRegionForcesBox each answered "does this region reference this binder,
// there?" by re-walking the whole region. markBoxedBinders asks it once per
// binder, so a body of N internal defines walked its own body N times — O(N²)
// node visits, measured at 47.9% + 19.1% cumulative on a 40,000-define compile.
// Nothing about the questions requires that: the region is fixed while they are
// being asked, so the occurrences can be enumerated once.
//
// WHY A SPELLING-KEYED MULTIMAP RATHER THAN A SET. Identity here is binder
// identity, not a name: the spelling narrows and ScopesCompatible decides
// (sameBinder). A set keyed on the name alone would answer a DIFFERENT question
// and would conflate a macro-introduced binder with a user identifier of the
// same name — the failure the root CLAUDE.local.md's "Binding Identity" section
// forbids. So the index groups occurrences by spelling, which is the part that
// is cheap and exact, and still runs the subset test against each occurrence
// that shares one. The scan per query is therefore bounded by the number of
// same-SPELLED occurrences, not by the region's size.

import (
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/syntax"
)

// binderRef is one occurrence of an identifier that a binder-identity query can
// match: a variable reference, or the target of a set!.
//
// A set! target IS a reference — a write reaches the same location a read does —
// so isSet marks an occurrence rather than excluding it from the reference set.
type binderRef struct {
	sym *syntax.SyntaxSymbol
	// origin is the index, in the expression slice the index was built from, of
	// the top-level expression this occurrence sits under. It is what lets a
	// letrec* region ask about ONE init without a separate walk.
	origin int
	// inClosure records that the occurrence is inside the body of a lambda,
	// case-lambda clause or function-form define — WalkSubExprs's
	// RoleClosureBody, the same boundary referencesBinder used.
	inClosure bool
	// isSet records that the occurrence is a set! target.
	isSet bool
}

// refIndex holds every reference occurrence in a region, grouped by spelling.
type refIndex map[string][]binderRef

// newRefIndex walks exprs once and records every reference occurrence in it.
//
// origin is the index into exprs, so a caller that passes a letrec* region's
// inits can afterwards distinguish "some init references this" from "init j
// references this" without walking again.
//
// The closure boundary is WalkSubExprs's RoleClosureBody, which marks the body
// of every lambda, case-lambda clause and function-form define. That is the
// honest boundary: WalkBindingRefs's depth counts only ESCAPING closures, and an
// immediately-applied lambda's body is not an escaping closure but is still not
// evaluated where it is written.
//
// A set! target counts as a reference — a write reaches the same location a read
// does, so a closure holding only a set! of the binder has still captured it.
func newRefIndex(exprs []validate.ValidatedExpr) refIndex {
	q := refIndex{}
	var walk func(e validate.ValidatedExpr, origin int, inClosure bool)
	walk = func(e validate.ValidatedExpr, origin int, inClosure bool) {
		if e == nil {
			return
		}
		sym, ok := e.(*validate.ValidatedSymbol)
		if ok {
			q.add(binderRef{sym: sym.Symbol, origin: origin, inClosure: inClosure})
			return
		}
		setBang, ok := e.(*validate.ValidatedSetBang)
		if ok {
			q.add(binderRef{sym: setBang.Name, origin: origin, inClosure: inClosure, isSet: true})
		}
		validate.WalkSubExprs(e, func(child validate.ValidatedExpr, role validate.ChildRole) {
			walk(child, origin, inClosure || role == validate.RoleClosureBody)
		})
	}
	for i, e := range exprs {
		walk(e, i, false)
	}
	return q
}

// add records one occurrence, ignoring a nil symbol — a set! the validator left
// without a target names no binding and can match none.
func (p refIndex) add(r binderRef) {
	if r.sym == nil {
		return
	}
	p[r.sym.Sym.Key] = append(p[r.sym.Sym.Key], r)
}

// matches reports whether any occurrence of name's spelling denotes the binding
// name introduces AND satisfies keep.
//
// The subset test runs per occurrence, so the answer is the same one a walk
// would have produced; only the enumeration is shared.
func (p refIndex) matches(name *syntax.SyntaxSymbol, keep func(binderRef) bool) bool {
	if name == nil || len(p) == 0 {
		return false
	}
	for _, r := range p[name.Sym.Key] {
		if !keep(r) {
			continue
		}
		if sameBinder(name, r.sym) {
			return true
		}
	}
	return false
}

// capturedBy reports whether some closure in the indexed region references the
// binding name introduces — refInsideClosure over the whole region.
func (p refIndex) capturedBy(name *syntax.SyntaxSymbol) bool {
	return p.matches(name, func(r binderRef) bool {
		return r.inClosure
	})
}

// referencedOutsideClosure reports whether the indexed region references the
// binding name introduces from where it is written rather than from inside a
// closure body — refOutsideClosure over the whole region.
func (p refIndex) referencedOutsideClosure(name *syntax.SyntaxSymbol) bool {
	return p.matches(name, func(r binderRef) bool {
		return !r.inClosure
	})
}

// assignedBy reports whether some set! in the indexed region targets the binding
// name introduces, at any closure depth.
func (p refIndex) assignedBy(name *syntax.SyntaxSymbol) bool {
	return p.matches(name, func(r binderRef) bool {
		return r.isSet
	})
}

// referencedFrom reports whether the origin-th expression of the indexed region
// references the binding name introduces, at the requested position.
//
// This is referencesBinder(exprs[origin], name, where), answered from the index.
func (p refIndex) referencedFrom(name *syntax.SyntaxSymbol, origin int, where refPosition) bool {
	return p.matches(name, func(r binderRef) bool {
		if r.origin != origin {
			return false
		}
		switch where {
		case refInsideClosure:
			return r.inClosure
		case refOutsideClosure:
			return !r.inClosure
		}
		return true
	})
}
