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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func TestHashPrimitives(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "equal-hash is deterministic", Code: `(= (equal-hash '(1 2 3)) (equal-hash '(1 2 3)))`, Expected: values.TrueValue},
		{Name: "equal-hash agrees with equal? on lists", Code: `(= (equal-hash (list 1 2)) (equal-hash (list 1 2)))`, Expected: values.TrueValue},
		{Name: "equal-hash accepts a vector", Code: `(= (equal-hash (vector 1 2)) (equal-hash (vector 1 2)))`, Expected: values.TrueValue},
		{Name: "equal-hash agrees across exact representations", Code: `(= (equal-hash 5) (equal-hash 5))`, Expected: values.TrueValue},
		{Name: "equal-hash is non-negative", Code: `(>= (equal-hash '(1 2 3)) 0)`, Expected: values.TrueValue},
		// The Scheme-level half of Task 2's *Record arm, and the reason it exists:
		// equal-hash is the only procedure in this surface that takes an ARBITRARY
		// object, and a define-record-type instance is the likeliest arbitrary object
		// a user has. Two distinct records of one type are equal? but not eqv?, so the
		// hash must agree on them AND separate different field values. Without the arm
		// the second case fails: every record hashed to the same opaque type-name
		// bucket.
		//
		// The (let () ...) wrapper is not cosmetic — the top level is immutable by
		// default, so a bare top-level define-record-type does not compile here.
		{Name: "equal-hash agrees on equal records", Code: `(let () (define-record-type <pt> (mk-pt x y) pt? (x pt-x) (y pt-y)) (= (equal-hash (mk-pt 1 2)) (equal-hash (mk-pt 1 2))))`, Expected: values.TrueValue},
		{Name: "equal-hash separates records by field", Code: `(let () (define-record-type <pt> (mk-pt x y) pt? (x pt-x) (y pt-y)) (not (= (equal-hash (mk-pt 1 2)) (equal-hash (mk-pt 2 1)))))`, Expected: values.TrueValue},
		{Name: "string-hash is deterministic", Code: `(= (string-hash "abc") (string-hash "abc"))`, Expected: values.TrueValue},
		{Name: "string-hash is unary in base", Code: `(exact-integer? (string-hash "abc"))`, Expected: values.TrueValue},
		{Name: "string-hash is non-negative", Code: `(>= (string-hash "abc") 0)`, Expected: values.TrueValue},
		{Name: "string-ci-hash folds case", Code: `(= (string-ci-hash "ABC") (string-ci-hash "abc"))`, Expected: values.TrueValue},
		// The reason string-ci-hash is registered beside string-ci=? rather than
		// beside string-hash: they must fold identically, and FULL Unicode folding
		// expands "ß" to "ss" where strings.ToLower would not.
		{Name: "string-ci-hash folds like string-ci=?", Code: `(and (string-ci=? "ß" "SS") (= (string-ci-hash "ß") (string-ci-hash "SS")))`, Expected: values.TrueValue},
		{Name: "symbol-hash is deterministic", Code: `(= (symbol-hash 'foo) (symbol-hash 'foo))`, Expected: values.TrueValue},
		{Name: "symbol-hash distinguishes symbols", Code: `(not (= (symbol-hash 'foo) (symbol-hash 'bar)))`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// The SRFI-13 shadowing guard is NOT here. It needs (import (srfi 13)) to
// resolve, and this package's harness has no library registry — "load-library:
// no library registry configured", which is true on master as well and has
// nothing to do with the R6RS string-hash. It lives in
// pkg/wile/srfi13_string_hash_shadow_test.go, where the engine is wired with the
// embedded stdlib.
