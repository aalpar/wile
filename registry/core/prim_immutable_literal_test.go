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
	"errors"
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

// TestImmutableLiteral_MutatorsRaise verifies the list/vector/bytevector
// mutators reject a quoted literal (R7RS §4.1.2) with the per-type sentinel,
// matching the existing immutable-string behavior.
func TestImmutableLiteral_MutatorsRaise(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want error
	}{
		{"set-car! on literal", `(set-car! '(a b) 'x)`, werr.ErrImmutablePair},
		{"set-cdr! on literal", `(set-cdr! '(a b) 'x)`, werr.ErrImmutablePair},
		{"list-set! on literal", `(list-set! '(a b c) 1 'x)`, werr.ErrImmutablePair},
		{"vector-set! on literal", `(vector-set! '#(1 2 3) 0 9)`, werr.ErrImmutableVector},
		{"vector-fill! on literal", `(vector-fill! '#(1 2 3) 0)`, werr.ErrImmutableVector},
		{"bytevector-u8-set! on literal", `(bytevector-u8-set! '#u8(1 2 3) 0 9)`, werr.ErrImmutableBytevector},
		{"bytevector-copy! on literal", `(bytevector-copy! '#u8(1 2 3) 0 (bytevector 9))`, werr.ErrImmutableBytevector},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, tc.want), qt.IsTrue,
				qt.Commentf("want %v, got %v", tc.want, err))
		})
	}
}

// TestImmutableLiteral_AllocatedStillMutable verifies freshly-allocated
// pairs/vectors remain mutable — the guard keys on literal identity, not type.
func TestImmutableLiteral_AllocatedStillMutable(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "set-car! on allocated pair",
			Code: `(let ((p (list 1 2))) (set-car! p 9) (car p))`, Expected: values.NewInteger(9)},
		{Name: "set-cdr! on allocated pair",
			Code: `(let ((p (list 1 2))) (set-cdr! p 9) (cdr p))`, Expected: values.NewInteger(9)},
		{Name: "list-set! on allocated list",
			Code: `(let ((p (list 1 2 3))) (list-set! p 1 9) (list-ref p 1))`, Expected: values.NewInteger(9)},
		{Name: "vector-set! on allocated vector",
			Code: `(let ((v (make-vector 2 0))) (vector-set! v 0 9) (vector-ref v 0))`, Expected: values.NewInteger(9)},
		{Name: "vector-fill! on allocated vector",
			Code: `(let ((v (make-vector 2 0))) (vector-fill! v 7) (vector-ref v 1))`, Expected: values.NewInteger(7)},
		{Name: "bytevector-u8-set! on allocated bytevector",
			Code: `(let ((b (make-bytevector 2 0))) (bytevector-u8-set! b 0 9) (bytevector-u8-ref b 0))`, Expected: values.NewInteger(9)},
		{Name: "bytevector-copy! on allocated bytevector",
			Code: `(let ((b (make-bytevector 2 0))) (bytevector-copy! b 0 (bytevector 7)) (bytevector-u8-ref b 0))`, Expected: values.NewInteger(7)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestImmutableLiteral_StructureSharingProbe is the corruption probe from the
// design draft: two equal? literals in ONE compilation unit dedup to one
// object, so set-cdr! through one must RAISE rather than silently corrupt the
// other. The (begin ...) wrapper forces a single compilation unit (per-template
// equal? dedup); separate REPL units would give distinct objects.
func TestImmutableLiteral_StructureSharingProbe(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, `(begin
		(define x '(1 2 3))
		(define y '(1 2 3))
		(set-cdr! x 99))`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrImmutablePair), qt.IsTrue,
		qt.Commentf("set-cdr! through a shared literal must raise ErrImmutablePair, got %v", err))
}

// TestImmutableLiteral_StringParityPreserved confirms literal strings remain
// immutable — the new pair/vector behavior is consistent with strings and did
// not regress them.
func TestImmutableLiteral_StringParityPreserved(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, `(string-set! "abc" 0 #\z)`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrImmutableString), qt.IsTrue,
		qt.Commentf("string-set! on a literal must still raise ErrImmutableString, got %v", err))
}
