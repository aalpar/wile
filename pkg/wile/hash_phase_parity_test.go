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

// All four R6RS hash procedures are callable from a macro body. Their ROUTES
// differ, deliberately, and each row names its own.
//
// RED on master on EXACTLY ONE ROW: string-ci-hash. equal-hash, string-hash and
// symbol-hash register PhaseSetRuntime|PhaseSetExpand in
// pkg/registry/core/hashes.go, so they are callable bare. string-ci-hash
// registers PhaseSetRuntime, because it lives in pkg/internal/extensions/all
// beside string-ci=? whose x/text FULL fold it must share and nothing in that
// package is registered at phase 1 — and no library exported it, so it was
// reachable from phase 1 by NO route. (rnrs hashtables) now exports it.
//
// WHY AN EXPORT AND NOT A WIDER PhaseSet. A PhaseSet governs AMBIENT
// availability; a for-syntax import is the R7RS-sanctioned way to get a binding
// at phase 1, and string-ci=? itself is only reachable at phase 1 that way
// (via (scheme char)). Widening the registration was the first fix and was
// reverted: it bought ambient parity at the price of making string-ci-hash the
// only both-phase registration outside pkg/registry/core, 1 against 34. So this
// test does NOT assert one phase set across the family. It asserts that each
// member is reachable, by its documented route, and the asymmetry in the
// `prologue` column is the decision rather than a gap in it.
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
)

func TestR6RSHashProceduresAreReachableAtPhaseOne(t *testing.T) {
	tests := []struct {
		name string
		// prologue is the import a macro body needs to see this procedure, or
		// empty when the sealed base carries it at phase 1 already.
		prologue string
		call     string
	}{
		{name: "equal-hash (ambient)", call: `(equal-hash (list 1 2))`},
		{name: "string-hash (ambient)", call: `(string-hash "abc")`},
		{name: "symbol-hash (ambient)", call: `(symbol-hash 'foo)`},
		// The filed row, and the only one with a prologue.
		{
			name:     "string-ci-hash (for-syntax import)",
			prologue: `(import (for-syntax (rnrs hashtables)))`,
			call:     `(string-ci-hash "ABC")`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			src := tt.prologue + `
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
// string-ci=? comes from (scheme char), the hash from (rnrs hashtables).
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
