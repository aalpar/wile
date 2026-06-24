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

// call-with-values Tests (R7RS §6.4 - Multiple values)

func TestCallWithValuesComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Single value
		{Name: "single value", Code: `(call-with-values (lambda () 42) (lambda (x) x))`, Expected: values.NewInteger(42)},

		// Multiple values
		{Name: "two values", Code: `(call-with-values (lambda () (values 1 2)) (lambda (a b) (+ a b)))`, Expected: values.NewInteger(3)},
		{Name: "three values", Code: `(call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (* a b c)))`, Expected: values.NewInteger(6)},
		{Name: "five values", Code: `(call-with-values (lambda () (values 1 2 3 4 5)) (lambda (a b c d e) (+ a b c d e)))`, Expected: values.NewInteger(15)},

		// Zero values
		{Name: "zero values", Code: `(call-with-values (lambda () (values)) (lambda () 'done))`, Expected: values.NewSymbol("done")},

		// Consumer uses list
		{Name: "consumer builds list", Code: `(call-with-values (lambda () (values 'a 'b 'c)) list)`, Expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},

		// Values from arithmetic
		{Name: "floor/ values", Code: `(call-with-values (lambda () (floor/ 13 4)) (lambda (q r) (+ (* q 10) r)))`, Expected: values.NewInteger(31)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCallWithValuesErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "producer not procedure", Code: `(call-with-values 5 (lambda (x) x))`},
		{Name: "consumer not procedure", Code: `(call-with-values (lambda () 1) 5)`},
		{Name: "arity mismatch", Code: `(call-with-values (lambda () (values 1 2)) (lambda (x) x))`},
		{Name: "exception in producer", Code: `(call-with-values (lambda () (error "boom")) list)`},
		{Name: "exception in consumer", Code: `(call-with-values (lambda () (values 1 2)) (lambda (x y) (error "boom")))`},
		{Name: "arity mismatch three to two", Code: `(call-with-values (lambda () (values 1 2 3)) (lambda (x y) (+ x y)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestCallWithValuesTailCall is a regression test for proper tail recursion of
// the call-with-values consumer (R7RS §3.5: "the call to consumer is a tail
// call"). Before the fix, PrimCallWithValues ran the consumer in a sub-context
// via sub2.Run(), nesting one Go stack frame per call. A deep tail loop through
// call-with-values — directly, or via the let-values macro which expands to it —
// therefore grew the host goroutine stack to its 1 GB limit and died with an
// uncatchable "fatal error: stack overflow" rather than running in O(1) frames.
//
// Each case is a single named-let expression so the only non-constant frame
// growth would come from the consumer call. If the fix regresses, these crash
// the whole test binary (a Go stack overflow cannot be recovered) instead of
// failing softly — that loud signal is intentional for a host-crash-class bug.
func TestCallWithValuesTailCall(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "direct call-with-values tail loop runs in O(1) frames",
			Code: `(let loop ((n 2000000))
			         (call-with-values (lambda () (values n))
			           (lambda (m) (if (= m 0) 'done (loop (- m 1))))))`,
			Expected: values.NewSymbol("done"),
		},
		{
			Name: "let-values tail loop runs in O(1) frames",
			Code: `(let loop ((n 2000000))
			         (let-values (((a) (values n)))
			           (if (= a 0) 'done (loop (- a 1)))))`,
			Expected: values.NewSymbol("done"),
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
