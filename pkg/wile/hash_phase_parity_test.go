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

package wile_test

// All four R6RS hash procedures are callable from a macro body, by ONE route:
// (import (for-syntax (rnrs hashtables))). All four register PhaseSetRuntime, so
// none is ambient at phase 1, and the export is how a phase-1 environment
// includes them, as (scheme char) is how it includes string-ci=?. Needing the
// import is a trade, not a cost: each environment chooses whether it includes
// the hashes, at the price of verbosity and of every environment being built
// alike.
//
// History: string-ci-hash was the filed row (I066, 2026-10-03), reachable from
// phase 1 by no route until (rnrs hashtables) exported it. The other three were
// ambient at phase 1 (PhaseSetRuntime|PhaseSetExpand in
// pkg/registry/core/hashes.go) until 2026-10-04, when they moved to the same
// route.
//
// The route is pinned, not just the reachability: each prologue imports its hash
// BY NAME through `only`, which fails if (rnrs hashtables) stops exporting it. A
// plain (for-syntax (rnrs hashtables)) import stayed green with string-ci-hash's
// export removed and its registration widened.
//
// Each row reveals the phase-1 value at phase 0 through an er-macro-transformer,
// the technique TestPhase1DefineSyntaxIsReachableFromItsOwnRung uses: a
// begin-for-syntax body cannot hand its value down a rung by itself. It then
// compares that value against the SAME call made at phase 0. A shape check —
// "an exact non-negative integer came back" — was the first cut and does not
// discriminate: measured, revealing (string-hash "ABC") under the string-ci-hash
// row passes it, so a phase-1 name resolving to a DIFFERENT hash stayed green.
// That is the live hazard and not a hypothetical, because the quartet is split
// across two files precisely so string-ci-hash can carry a fold string-hash does
// not. All four hashes are deterministic, so the comparison is not flaky.

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// r6rsHashCalls is the quartet, one deterministic call each.
var r6rsHashCalls = []struct {
	name string
	call string
}{
	{name: "equal-hash", call: `(equal-hash (list 1 2))`},
	{name: "string-hash", call: `(string-hash "abc")`},
	{name: "string-ci-hash", call: `(string-ci-hash "ABC")`},
	{name: "symbol-hash", call: `(symbol-hash 'foo)`},
}

func TestR6RSHashProceduresAreReachableAtPhaseOne(t *testing.T) {
	for _, tt := range r6rsHashCalls {
		t.Run(tt.name, func(t *testing.T) {
			src := `(import (for-syntax (only (rnrs hashtables) ` + tt.name + `)))
			        (begin-for-syntax (define zz ` + tt.call + `))
			        (define-syntax reveal (er-macro-transformer (lambda (form rename compare) zz)))
			        (and (exact-integer? (reveal)) (>= (reveal) 0) (= (reveal) ` + tt.call + `))`
			got, err := evalPhaseDistinctness(t, src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "#t")
		})
	}
}

// The case-insensitive pair agrees at phase 1 on the input the FULL fold exists
// for. This is the property the export buys and the reason the hash had to share
// string-ci=?'s fold rather than approximate it: under simple folding or
// strings.ToLower, "ß" and "SS" disagree, so the hash would contradict its own
// documented contract ("if (string-ci=? a b) then equal hashes") on exactly that
// input.
//
// Both halves need an import at phase 1 and they are DIFFERENT imports:
// string-ci=? comes from (scheme char), the hash from (rnrs hashtables). The
// import is the plain whole-library one the records document as the route; the
// reachability test above narrows it with `only` to pin the export itself.
func TestStringCiPairAgreesAtPhaseOne(t *testing.T) {
	got, err := evalPhaseDistinctness(t, `
		(import (for-syntax (scheme char)) (for-syntax (rnrs hashtables)))
		(begin-for-syntax
		  (define zz (and (string-ci=? "ß" "SS")
		                  (= (string-ci-hash "ß") (string-ci-hash "SS")))))
		(define-syntax reveal (er-macro-transformer (lambda (form rename compare) zz)))
		(reveal)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "#t")
}

// None of the four is ambient at phase 1: without the import, a macro body cannot
// see it. This is the other half of the route the reachability test pins, and the
// only thing that pins "do not widen" (pkg/registry/core/hashes.go,
// pkg/internal/extensions/all/register.go): widening any one registration to
// PhaseSetRuntime|PhaseSetExpand keeps every other test green and flips its row
// here.
func TestR6RSHashesAreNotAmbientAtPhaseOne(t *testing.T) {
	for _, tt := range r6rsHashCalls {
		t.Run(tt.name, func(t *testing.T) {
			// Positive control, same configuration: the name exists at phase 0, so
			// the refusal below is about the phase and not a missing registration.
			got, err := evalPhaseDistinctness(t, `(exact-integer? `+tt.call+`)`)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "#t")

			_, err = evalPhaseDistinctness(t, `(begin-for-syntax (define zz `+tt.call+`))`)
			qt.Assert(t, err, qt.ErrorIs, werr.ErrNoSuchBinding)
			// By name as well: the sentinel alone is satisfied by any unbound
			// identifier.
			qt.Assert(t, err, qt.ErrorMatches, `(?s).*"`+tt.name+`" at phase 1.*`)
		})
	}
}
