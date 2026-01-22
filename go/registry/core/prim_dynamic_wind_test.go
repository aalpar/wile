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
