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

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// dynamic-wind Tests (R7RS §6.4 - Cleanup handlers)

func TestDynamicWindComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic - returns thunk value
		{Name: "returns thunk value", Code: `(dynamic-wind (lambda () #f) (lambda () 42) (lambda () #f))`, Expected: values.NewInteger(42)},

		// Execution order
		{
			Name: "before runs first",
			Code: `(let ((log '()))
				(dynamic-wind
					(lambda () (set! log (cons 'before log)))
					(lambda () (set! log (cons 'during log)) 'result)
					(lambda () (set! log (cons 'after log))))
				(reverse log))`,
			Expected: values.List(values.NewSymbol("before"), values.NewSymbol("during"), values.NewSymbol("after")),
		},

		// After runs even on error (caught)
		{
			Name: "after runs on normal exit",
			Code: `(let ((after-ran #f))
				(dynamic-wind
					(lambda () #f)
					(lambda () 42)
					(lambda () (set! after-ran #t)))
				after-ran)`,
			Expected: values.TrueValue,
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

func TestDynamicWindWithContinuations(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// After runs on continuation escape
		{
			Name: "after runs on escape",
			Code: `(let ((after-ran #f))
				(call/cc (lambda (k)
					(dynamic-wind
						(lambda () #f)
						(lambda () (k 'escaped))
						(lambda () (set! after-ran #t)))))
				after-ran)`,
			Expected: values.TrueValue,
		},

		// Escape value is correct
		{
			Name: "escape returns correct value",
			Code: `(call/cc (lambda (k)
				(dynamic-wind
					(lambda () #f)
					(lambda () (k 77))
					(lambda () #f))))`,
			Expected: values.NewInteger(77),
		},

		// Before/after state mutation visible
		{
			Name: "before sets state",
			Code: `(let ((v (make-vector 1 0)))
				(dynamic-wind
					(lambda () (vector-set! v 0 1))
					(lambda () (vector-ref v 0))
					(lambda () (vector-set! v 0 2))))`,
			Expected: values.NewInteger(1),
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

func TestDynamicWindErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "before not procedure", Code: `(dynamic-wind 5 (lambda () 1) (lambda () 2))`},
		{Name: "thunk not procedure", Code: `(dynamic-wind (lambda () 1) 5 (lambda () 2))`},
		{Name: "after not procedure", Code: `(dynamic-wind (lambda () 1) (lambda () 2) 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestDynamicWindR7RSExample tests the classic R7RS §6.10 example.
// This is the canonical test for dynamic-wind + call/cc interaction.
func TestDynamicWindR7RSExample(t *testing.T) {
	// R7RS §6.10 example:
	// (let ((path '()) (c #f))
	//   (let ((add (lambda (s) (set! path (cons s path)))))
	//     (dynamic-wind
	//       (lambda () (add 'connect))
	//       (lambda () (add (call/cc (lambda (c0) (set! c c0) 'talk1))))
	//       (lambda () (add 'disconnect)))
	//     (if (< (length path) 4)
	//         (c 'talk2)
	//         (reverse path))))
	// => (connect talk1 disconnect connect talk2 disconnect)
	code := `
		(let ((path '()) (c #f))
			(let ((add (lambda (s) (set! path (cons s path)))))
				(dynamic-wind
					(lambda () (add 'connect))
					(lambda () (add (call/cc (lambda (c0) (set! c c0) 'talk1))))
					(lambda () (add 'disconnect)))
				(if (< (length path) 4)
					(c 'talk2)
					(reverse path))))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	expected := values.List(
		values.NewSymbol("connect"),
		values.NewSymbol("talk1"),
		values.NewSymbol("disconnect"),
		values.NewSymbol("connect"),
		values.NewSymbol("talk2"),
		values.NewSymbol("disconnect"),
	)
	qt.Assert(t, result, valuestest.SchemeEquals, expected)
}

// TestDynamicWindNestedContinuations tests nested dynamic-wind with continuations.
func TestDynamicWindNestedContinuations(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Nested dynamic-wind - inner escape triggers both afters
		{
			Name: "nested escape runs both afters",
			Code: `
				(let ((log '()))
					(call/cc (lambda (outer-k)
						(dynamic-wind
							(lambda () (set! log (cons 'outer-before log)))
							(lambda ()
								(dynamic-wind
									(lambda () (set! log (cons 'inner-before log)))
									(lambda () (outer-k 'escaped))
									(lambda () (set! log (cons 'inner-after log)))))
							(lambda () (set! log (cons 'outer-after log))))))
					(reverse log))
			`,
			Expected: values.List(
				values.NewSymbol("outer-before"),
				values.NewSymbol("inner-before"),
				values.NewSymbol("inner-after"),
				values.NewSymbol("outer-after"),
			),
		},
		// Nested dynamic-wind - normal exit
		{
			Name: "nested normal exit",
			Code: `
				(let ((log '()))
					(dynamic-wind
						(lambda () (set! log (cons 'outer-before log)))
						(lambda ()
							(dynamic-wind
								(lambda () (set! log (cons 'inner-before log)))
								(lambda () (set! log (cons 'inner-thunk log)) 'result)
								(lambda () (set! log (cons 'inner-after log)))))
						(lambda () (set! log (cons 'outer-after log))))
					(reverse log))
			`,
			Expected: values.List(
				values.NewSymbol("outer-before"),
				values.NewSymbol("inner-before"),
				values.NewSymbol("inner-thunk"),
				values.NewSymbol("inner-after"),
				values.NewSymbol("outer-after"),
			),
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

// TestDynamicWindContinuationReentry tests calling a captured continuation multiple times.
func TestDynamicWindContinuationReentry(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Re-entry: continuation invoked twice
		{
			Name: "continuation invoked twice",
			Code: `
				(let ((count 0) (c #f))
					(dynamic-wind
						(lambda () (set! count (+ count 1)))
						(lambda () (call/cc (lambda (k) (set! c k) 'first)))
						(lambda () #f))
					(if (< count 3)
						(c 'again)
						count))
			`,
			Expected: values.NewInteger(3),
		},
		// Re-entry tracks before calls
		{
			Name: "reentry runs before thunk each time",
			Code: `
				(let ((before-count 0) (c #f) (iterations 0))
					(dynamic-wind
						(lambda () (set! before-count (+ before-count 1)))
						(lambda () (call/cc (lambda (k) (set! c k) 'first)))
						(lambda () #f))
					(set! iterations (+ iterations 1))
					(if (< iterations 3)
						(c 'again)
						before-count))
			`,
			Expected: values.NewInteger(3),
		},
		// Re-entry tracks after calls
		{
			Name: "reentry runs after thunk on exit",
			Code: `
				(let ((after-count 0) (c #f) (iterations 0))
					(dynamic-wind
						(lambda () #f)
						(lambda () (call/cc (lambda (k) (set! c k) 'first)))
						(lambda () (set! after-count (+ after-count 1))))
					(set! iterations (+ iterations 1))
					(if (< iterations 3)
						(c 'again)
						after-count))
			`,
			Expected: values.NewInteger(3),
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

// TestCallCCComprehensive tests various call/cc scenarios.
func TestCallCCComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic: return without invoking continuation
		{
			Name:     "normal return",
			Code:     `(call/cc (lambda (k) 42))`,
			Expected: values.NewInteger(42),
		},
		// Basic: invoke continuation immediately
		{
			Name:     "immediate escape",
			Code:     `(call/cc (lambda (k) (k 42)))`,
			Expected: values.NewInteger(42),
		},
		// Continuation skips remaining computation
		{
			Name:     "escape skips computation",
			Code:     `(+ 1 (call/cc (lambda (k) (+ 100 (k 10)))))`,
			Expected: values.NewInteger(11), // 1 + 10, not 1 + 100 + 10
		},
		// Continuation captured and stored for later
		{
			Name: "stored continuation invoked later",
			Code: `
				(let ((saved #f))
					(+ 1 (call/cc (lambda (k) (set! saved k) 10)))
					(if saved
						(let ((k saved))
							(set! saved #f)
							(k 20))
						'done))
			`,
			Expected: values.NewSymbol("done"),
		},
		// Nested call/cc
		{
			Name: "nested call/cc inner escape",
			Code: `
				(call/cc (lambda (outer)
					(call/cc (lambda (inner)
						(inner 'from-inner)))
					'after-inner))
			`,
			Expected: values.NewSymbol("after-inner"),
		},
		{
			Name: "nested call/cc outer escape",
			Code: `
				(call/cc (lambda (outer)
					(call/cc (lambda (inner)
						(outer 'from-outer)))
					'after-inner))
			`,
			Expected: values.NewSymbol("from-outer"),
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
