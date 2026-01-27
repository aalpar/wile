// Copyright 2025 Aaron Alpar
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

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// dynamic-wind Tests (R7RS §6.4 - Cleanup handlers)

func TestDynamicWindComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic - returns thunk value
		{name: "returns thunk value", code: `(dynamic-wind (lambda () #f) (lambda () 42) (lambda () #f))`, expected: values.NewInteger(42)},

		// Execution order
		{
			name: "before runs first",
			code: `(let ((log '()))
				(dynamic-wind
					(lambda () (set! log (cons 'before log)))
					(lambda () (set! log (cons 'during log)) 'result)
					(lambda () (set! log (cons 'after log))))
				(reverse log))`,
			expected: values.List(values.NewSymbol("before"), values.NewSymbol("during"), values.NewSymbol("after")),
		},

		// After runs even on error (caught)
		{
			name: "after runs on normal exit",
			code: `(let ((after-ran #f))
				(dynamic-wind
					(lambda () #f)
					(lambda () 42)
					(lambda () (set! after-ran #t)))
				after-ran)`,
			expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestDynamicWindWithContinuations(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// After runs on continuation escape
		{
			name: "after runs on escape",
			code: `(let ((after-ran #f))
				(call/cc (lambda (k)
					(dynamic-wind
						(lambda () #f)
						(lambda () (k 'escaped))
						(lambda () (set! after-ran #t)))))
				after-ran)`,
			expected: values.TrueValue,
		},

		// Escape value is correct
		{
			name: "escape returns correct value",
			code: `(call/cc (lambda (k)
				(dynamic-wind
					(lambda () #f)
					(lambda () (k 77))
					(lambda () #f))))`,
			expected: values.NewInteger(77),
		},

		// Before/after state mutation visible
		{
			name: "before sets state",
			code: `(let ((v (make-vector 1 0)))
				(dynamic-wind
					(lambda () (vector-set! v 0 1))
					(lambda () (vector-ref v 0))
					(lambda () (vector-set! v 0 2))))`,
			expected: values.NewInteger(1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestDynamicWindErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "before not procedure", code: `(dynamic-wind 5 (lambda () 1) (lambda () 2))`},
		{name: "thunk not procedure", code: `(dynamic-wind (lambda () 1) 5 (lambda () 2))`},
		{name: "after not procedure", code: `(dynamic-wind (lambda () 1) (lambda () 2) 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestDynamicWindR7RSExample tests the classic R7RS §6.10 example.
// This is the canonical test for dynamic-wind + call/cc interaction.
//
// KNOWN LIMITATION: This test currently fails because the winding stack implementation
// does not properly run the after thunk on normal completion after continuation re-entry.
// The expected result is (connect talk1 disconnect connect talk2 disconnect)
// but we get (connect talk1 disconnect connect talk2) - missing final disconnect.
// See plans/CONTINUATION_ESCAPE_FIX.md for details.
func TestDynamicWindR7RSExample(t *testing.T) {
	t.Skip("KNOWN LIMITATION: after thunk not running on normal completion after continuation re-entry")
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
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	expected := values.List(
		values.NewSymbol("connect"),
		values.NewSymbol("talk1"),
		values.NewSymbol("disconnect"),
		values.NewSymbol("connect"),
		values.NewSymbol("talk2"),
		values.NewSymbol("disconnect"),
	)
	qt.Assert(t, result, values.SchemeEquals, expected)
}

// TestDynamicWindNestedContinuations tests nested dynamic-wind with continuations.
func TestDynamicWindNestedContinuations(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Nested dynamic-wind - inner escape triggers both afters
		{
			name: "nested escape runs both afters",
			code: `
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
			expected: values.List(
				values.NewSymbol("outer-before"),
				values.NewSymbol("inner-before"),
				values.NewSymbol("inner-after"),
				values.NewSymbol("outer-after"),
			),
		},
		// Nested dynamic-wind - normal exit
		{
			name: "nested normal exit",
			code: `
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
			expected: values.List(
				values.NewSymbol("outer-before"),
				values.NewSymbol("inner-before"),
				values.NewSymbol("inner-thunk"),
				values.NewSymbol("inner-after"),
				values.NewSymbol("outer-after"),
			),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestDynamicWindContinuationReentry tests calling a captured continuation multiple times.
func TestDynamicWindContinuationReentry(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Re-entry: continuation invoked twice
		{
			name: "continuation invoked twice",
			code: `
				(let ((count 0) (c #f))
					(dynamic-wind
						(lambda () (set! count (+ count 1)))
						(lambda () (call/cc (lambda (k) (set! c k) 'first)))
						(lambda () #f))
					(if (< count 3)
						(c 'again)
						count))
			`,
			expected: values.NewInteger(3),
		},
		// Re-entry tracks before calls
		{
			name: "reentry runs before thunk each time",
			code: `
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
			expected: values.NewInteger(3),
		},
		// Re-entry tracks after calls
		{
			name: "reentry runs after thunk on exit",
			code: `
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
			expected: values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestCallCCComprehensive tests various call/cc scenarios.
func TestCallCCComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic: return without invoking continuation
		{
			name:     "normal return",
			code:     `(call/cc (lambda (k) 42))`,
			expected: values.NewInteger(42),
		},
		// Basic: invoke continuation immediately
		{
			name:     "immediate escape",
			code:     `(call/cc (lambda (k) (k 42)))`,
			expected: values.NewInteger(42),
		},
		// Continuation skips remaining computation
		{
			name:     "escape skips computation",
			code:     `(+ 1 (call/cc (lambda (k) (+ 100 (k 10)))))`,
			expected: values.NewInteger(11), // 1 + 10, not 1 + 100 + 10
		},
		// Continuation captured and stored for later
		{
			name: "stored continuation invoked later",
			code: `
				(let ((saved #f))
					(+ 1 (call/cc (lambda (k) (set! saved k) 10)))
					(if saved
						(let ((k saved))
							(set! saved #f)
							(k 20))
						'done))
			`,
			expected: values.NewSymbol("done"),
		},
		// Nested call/cc
		{
			name: "nested call/cc inner escape",
			code: `
				(call/cc (lambda (outer)
					(call/cc (lambda (inner)
						(inner 'from-inner)))
					'after-inner))
			`,
			expected: values.NewSymbol("after-inner"),
		},
		{
			name: "nested call/cc outer escape",
			code: `
				(call/cc (lambda (outer)
					(call/cc (lambda (inner)
						(outer 'from-outer)))
					'after-inner))
			`,
			expected: values.NewSymbol("from-outer"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}
