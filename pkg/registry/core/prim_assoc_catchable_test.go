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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestAssocFamilyMalformedEntryIsCatchable pins that a malformed alist entry is
// a Scheme condition for every member of the assq/assv/assoc family.
//
// '(()) is outside assq's R7RS §6.3 domain ("alist must be a list of pairs"), so
// no correct answer is at stake and this is not a conformance question — the
// defect was the error CHANNEL. helpers.AssocLookup asserted values.Tuple, which
// the empty list satisfies, then called Car() on it; the panic escaped as an
// uncatchable "internal error: emptyList.Car" and aborted the whole evaluation.
// assoc, which is Scheme (bootstrap_procedures.scm) and reaches car through the
// ordinary primitive, answered "caught" on the same input — so the family
// disagreed with itself over which of its three members could be guarded.
func TestAssocFamilyMalformedEntryIsCatchable(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "assq on an alist holding the empty list",
			Code:     `(guard (e (#t 'caught)) (assq 'x '(())))`,
			Expected: values.NewSymbol("caught"),
		},
		{
			Name:     "assv on an alist holding the empty list",
			Code:     `(guard (e (#t 'caught)) (assv 'x '(())))`,
			Expected: values.NewSymbol("caught"),
		},
		{
			// The sibling that was already catchable, kept as the row that
			// states what the other two were being measured against.
			Name:     "assoc on an alist holding the empty list (control)",
			Code:     `(guard (e (#t 'caught)) (assoc 'x '(())))`,
			Expected: values.NewSymbol("caught"),
		},
		{
			// A non-pair, non-empty entry was already a returned error rather
			// than a panic. It shares the guard, so it pins that the added arm
			// did not displace the existing one.
			Name:     "assq on an alist holding a non-pair",
			Code:     `(guard (e (#t 'caught)) (assq 'x '(5)))`,
			Expected: values.NewSymbol("caught"),
		},
		{
			// Well-formed input is unaffected: the guard runs per entry, and a
			// hit must still return the entry rather than raise.
			Name:     "well-formed alist still resolves",
			Code:     `(cdr (assq 'b '((a . 1) (b . 2))))`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "well-formed alist still misses",
			Code:     `(assq 'z '((a . 1)))`,
			Expected: values.FalseValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
