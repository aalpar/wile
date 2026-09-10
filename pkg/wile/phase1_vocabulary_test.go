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

package wile

// The macro vocabulary: which base names a transformer body sees for free.
//
// Plan: plans/2026-09-08-flatt-binding-model-a-impl.local.md, Task 10; design
// section 9's Q2, which said explicitly to pin this with a ratchet during
// implementation rather than guess it in the design.
//
// This is a POLICY, not a measurement. What measurement settles is which names
// have no import route and would therefore be stranded; what policy settles is
// where the line between the macro-writing kernel and the runtime library
// falls. Widening the set is how D4's phase-distinctness break gets quietly
// undone — every name added here is a name a procedural transformer stops
// having to declare — so the ratchet asserts membership in BOTH directions.

import (
	"context"
	"slices"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestPhase1VocabularyMembership pins the set exactly.
//
// The failure message names what moved in which direction, because the two
// directions mean opposite things: an ADDED name is a narrowing of D4 that has
// to be argued, a REMOVED one is a transformer body that will stop compiling.
func TestPhase1VocabularyMembership(t *testing.T) {
	want := []string{
		// The declarative vocabulary a syntax-rules macro's own definition needs.
		"...", "=>", "_", "else", "syntax-rules",
		// Syntax introspection: registered at phase 1, exported by no library at
		// any phase, so an import cannot reach them.
		"bound-identifier=?", "datum->syntax", "er-macro-transformer",
		"expand", "expand-once", "free-identifier=?", "generate-temporaries",
		"identifier?", "make-synthetic-identifier", "quasisyntax", "quote-syntax",
		"syntax", "syntax->datum", "syntax-case", "syntax-local-identifier-as-binding",
		"syntax-local-introduce", "syntax-local-value", "syntax-violation",
		"unsyntax", "unsyntax-splicing", "with-syntax",
		// Predicates, equality and list basics the macro layer's own bodies use.
		"+", "-", "=", "append", "apply", "car", "cdr", "cons", "eq?", "equal?",
		"eqv?", "error", "length", "list", "not", "null?", "pair?", "procedure?",
		"reverse", "symbol?",
	}
	slices.Sort(want)

	got := []string{}
	for k := range defaultMacroVocabulary() {
		got = append(got, k)
	}
	slices.Sort(got)

	added := []string{}
	for _, n := range got {
		if slices.Contains(want, n) {
			continue
		}
		added = append(added, n)
	}
	removed := []string{}
	for _, n := range want {
		if slices.Contains(got, n) {
			continue
		}
		removed = append(removed, n)
	}
	qt.Assert(t, added, qt.HasLen, 0,
		qt.Commentf("names ADDED to the macro vocabulary: %v — each one is a name a procedural transformer no longer has to import, so each narrows D4 and needs its own argument", added))
	qt.Assert(t, removed, qt.HasLen, 0,
		qt.Commentf("names REMOVED from the macro vocabulary: %v — each one is a transformer body that stops compiling", removed))
}

// TestMacroVocabularyAdmitsTheBootstrapPrefix pins the OPEN arm.
//
// A %-prefixed name is bootstrap-private machinery: it lives in the base store,
// no .sld exports one, and none could. So it is unreachable by any import, and
// excluding it would strand the Scheme syntax layer with no route rather than
// making it declare one — bootstrap_syntax_procedures.scm alone defines 56.
//
// The arm cannot be enumerated (the names move whenever that file does), so what
// is pinned instead is the RULE and its boundary: %-prefixed in, everything else
// decided by the closed set. It does not widen D4's break, because a %-name is
// not something user code writes.
func TestMacroVocabularyAdmitsTheBootstrapPrefix(t *testing.T) {
	qt.Assert(t, macroVocabularyAdmits("%syntax-case-transform"), qt.IsTrue)
	qt.Assert(t, macroVocabularyAdmits("%pattern-variable?"), qt.IsTrue)
	qt.Assert(t, macroVocabularyAdmits("%"), qt.IsTrue)

	// The boundary: a name that merely CONTAINS % is not admitted, and neither
	// is an ordinary runtime name.
	qt.Assert(t, macroVocabularyAdmits("mod%"), qt.IsFalse)
	qt.Assert(t, macroVocabularyAdmits("cadr"), qt.IsFalse,
		qt.Commentf("cadr in the vocabulary would delete D4's break outright"))
	qt.Assert(t, macroVocabularyAdmits("assoc"), qt.IsFalse)
	qt.Assert(t, macroVocabularyAdmits("vector-map"), qt.IsFalse)
}

// TestPhase1VocabularyIsAStrictSubsetOfTheBase is the property that makes the
// vocabulary mean something.
//
// If it were the whole base, every row would install, rank and win, and the
// phase-distinctness break would be unobservable while every test still passed
// — the failure design section 6.3 says this shape defaults to. The subset is
// checked from the other end too: the names D4 is ABOUT must be reachable at
// phase 0 and not at phase 1.
func TestPhase1VocabularyIsAStrictSubsetOfTheBase(t *testing.T) {
	eng, err := NewEngine(context.Background(), WithProfile(KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	root := eng.Namespace().Runtime()
	expand := root.AtPhase(environment.PhaseExpand)

	// Group (c) of the census: bootstrap Scheme runtime definitions. Reachable at
	// phase 0, not at phase 1, and (import (for-syntax (scheme base))) is the
	// only route. These four ARE D4.
	for _, n := range []string{"assoc", "caar", "vector-map", "string-map"} {
		sym := values.NewSymbol(n)
		qt.Assert(t, root.GetBinding(sym, syntax.EmptyScopes()), qt.IsNotNil,
			qt.Commentf("%q must still resolve at phase 0", n))
		qt.Assert(t, expand.GetBinding(sym, syntax.EmptyScopes()), qt.IsNil,
			qt.Commentf("%q reaching phase 1 means the vocabulary widened into the runtime library, which is D4's break undone", n))
	}

	// Group (b): vocabulary members with no exact phase-1 slot of their own.
	// They reach phase 1 THROUGH THE ROW, which is what the row is for.
	for _, n := range []string{"...", "_", "else", "=>", "apply", "not"} {
		sym := values.NewSymbol(n)
		qt.Assert(t, expand.GetBinding(sym, syntax.EmptyScopes()), qt.IsNotNil,
			qt.Commentf("%q must reach phase 1 through the vocabulary row", n))
	}
}

// TestPhase1VocabularyIsPinnedAgainstTheGoLayer records the layer-dependence,
// which is D9's stated residual.
//
// Under the flag-gated Scheme syntax layer (WithSchemeSyntaxForms /
// WILE_SYNTAX_FORMS), syntax-rules becomes a Scheme macro with a procedural
// body, and that body's OWN transformer needs phase-2 visibility. The set above
// is pinned against the GO layer, and the flip revises it rather than
// contradicting it.
//
// Without this recorded, the flip will silently disagree with the ratchet
// instead of updating it — the failure mode being that someone widens the
// vocabulary to make the Scheme layer pass and never notices they deleted D4's
// break for the Go layer at the same time.
//
// What the test itself can assert today is the mechanism the flip depends on:
// the vocabulary reaches phase 2 as well as phase 1, by the same per-phase
// install, so a deeper tower is a matter of which names rather than which
// phases.
func TestPhase1VocabularyIsPinnedAgainstTheGoLayer(t *testing.T) {
	eng, err := NewEngine(context.Background(), WithProfile(KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	root := eng.Namespace().Runtime()
	for _, phase := range []environment.Phase{environment.PhaseExpand, 2, 3} {
		v := root.AtPhase(phase)
		qt.Assert(t, v.GetBinding(values.NewSymbol("syntax-rules"), syntax.EmptyScopes()), qt.IsNotNil,
			qt.Commentf("the vocabulary must reach phase %d; the tower is unbounded and the install is per view", phase))
		qt.Assert(t, v.GetBinding(values.NewSymbol("assoc"), syntax.EmptyScopes()), qt.IsNil,
			qt.Commentf("and the runtime library must reach phase %d no more than it reaches phase 1 — that uniformity is TestPhase2_UniformRule's subject", phase))
	}
}
