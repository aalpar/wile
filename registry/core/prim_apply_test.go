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
	"time"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// apply Tests (R7RS §6.4 - Function application)

func TestApplyComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic apply
		{Name: "apply + to list", Code: `(apply + '(1 2 3))`, Expected: values.NewInteger(6)},
		{Name: "apply * to list", Code: `(apply * '(2 3 4))`, Expected: values.NewInteger(24)},
		{Name: "apply - to list", Code: `(apply - '(10 3 2))`, Expected: values.NewInteger(5)},

		// Apply with prefix arguments
		{Name: "apply with one prefix", Code: `(apply + 1 '(2 3))`, Expected: values.NewInteger(6)},
		{Name: "apply with two prefix", Code: `(apply + 1 2 '(3 4))`, Expected: values.NewInteger(10)},
		{Name: "apply with many prefix", Code: `(apply + 1 2 3 4 '(5))`, Expected: values.NewInteger(15)},

		// Empty list
		{Name: "apply + to empty list", Code: `(apply + '())`, Expected: values.NewInteger(0)},
		{Name: "apply * to empty list", Code: `(apply * '())`, Expected: values.NewInteger(1)},
		{Name: "apply list to empty list", Code: `(apply list '())`, Expected: values.EmptyList},

		// Apply with lambda
		{Name: "apply lambda", Code: `(apply (lambda (x y) (+ x y)) '(3 4))`, Expected: values.NewInteger(7)},
		{Name: "apply variadic lambda", Code: `(apply (lambda args args) '(1 2 3))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},

		// Apply with cons
		{Name: "apply cons", Code: `(apply cons '(1 2))`, Expected: values.NewCons(values.NewInteger(1), values.NewInteger(2))},
		{Name: "apply car", Code: `(apply car '((1 2 3)))`, Expected: values.NewInteger(1)},

		// case-lambda dispatch
		{Name: "case-lambda two args", Code: `(apply (case-lambda ((x) x) ((x y) (+ x y))) '(3 4))`, Expected: values.NewInteger(7)},
		{Name: "case-lambda one arg", Code: `(apply (case-lambda ((x) x) ((x y) (+ x y))) '(42))`, Expected: values.NewInteger(42)},

		// Nested apply
		{Name: "nested apply", Code: `(apply apply (list + '(1 2 3)))`, Expected: values.NewInteger(6)},

		// Build list with prefix args
		{Name: "build list with prefix", Code: `(apply list 1 2 '(3 4 5))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4), values.NewInteger(5))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestApplyMultipleValues tests that apply correctly propagates multiple values.
// R7RS §6.4: apply should preserve multiple return values from the applied procedure.
func TestApplyMultipleValues(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// apply values should return multiple values
		{
			Name:     "apply values with two args",
			Code:     `(call-with-values (lambda () (apply values '(1 2))) list)`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			Name:     "apply values with three args",
			Code:     `(call-with-values (lambda () (apply values '(a b c))) list)`,
			Expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			Name:     "apply values with zero args",
			Code:     `(call-with-values (lambda () (apply values '())) (lambda () 'empty))`,
			Expected: values.NewSymbol("empty"),
		},
		{
			Name:     "apply values with one arg",
			Code:     `(call-with-values (lambda () (apply values '(42))) (lambda (x) x))`,
			Expected: values.NewInteger(42),
		},
		// apply a multi-value returning procedure
		{
			Name:     "apply floor/ (multi-value)",
			Code:     `(call-with-values (lambda () (apply floor/ '(17 5))) list)`,
			Expected: values.List(values.NewInteger(3), values.NewInteger(2)),
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

// TestApplyWindingStackInheritance verifies that continuations captured inside
// apply preserve the enclosing dynamic-wind context. Without winding stack
// inheritance, call/cc inside apply's sub-context captures an empty winding
// stack, so re-invocation skips before/after thunks.
//
// The outer call-with-continuation-prompt delimits the continuation so it only
// captures the dynamic-wind section, preventing infinite re-invocation.
func TestApplyWindingStackInheritance(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "baseline: call/cc directly in dynamic-wind",
			Code: `
			(let ((k #f)
			      (before-count 0))
			  (call-with-continuation-prompt
			    (lambda ()
			      (dynamic-wind
			        (lambda () (set! before-count (+ before-count 1)))
			        (lambda ()
			          (call/cc (lambda (cont) (set! k cont) 'first)))
			        (lambda () #f)))
			    (default-continuation-prompt-tag)
			    #f)
			  (call-with-continuation-prompt
			    (lambda () (k 'second))
			    (default-continuation-prompt-tag)
			    (lambda (v) v))
			  before-count)`,
			Expected: values.NewInteger(2),
		},
		{
			Name: "call/cc inside apply in dynamic-wind",
			Code: `
			(let ((k #f)
			      (before-count 0))
			  (call-with-continuation-prompt
			    (lambda ()
			      (dynamic-wind
			        (lambda () (set! before-count (+ before-count 1)))
			        (lambda ()
			          (apply
			            (lambda ()
			              (call/cc (lambda (cont) (set! k cont) 'first)))
			            '()))
			        (lambda () #f)))
			    (default-continuation-prompt-tag)
			    #f)
			  (call-with-continuation-prompt
			    (lambda () (k 'second))
			    (default-continuation-prompt-tag)
			    (lambda (v) v))
			  before-count)`,
			Expected: values.NewInteger(2),
		},
		{
			Name: "call/cc inside call-with-values in dynamic-wind",
			Code: `
			(let ((k #f)
			      (before-count 0))
			  (call-with-continuation-prompt
			    (lambda ()
			      (dynamic-wind
			        (lambda () (set! before-count (+ before-count 1)))
			        (lambda ()
			          (call-with-values
			            (lambda ()
			              (call/cc (lambda (cont) (set! k cont) 'first)))
			            (lambda (v) v)))
			        (lambda () #f)))
			    (default-continuation-prompt-tag)
			    #f)
			  (call-with-continuation-prompt
			    (lambda () (k 'second))
			    (default-continuation-prompt-tag)
			    (lambda (v) v))
			  before-count)`,
			Expected: values.NewInteger(2),
		},
		{
			Name: "call/cc inside with-exception-handler in dynamic-wind",
			Code: `
			(let ((k #f)
			      (before-count 0))
			  (call-with-continuation-prompt
			    (lambda ()
			      (dynamic-wind
			        (lambda () (set! before-count (+ before-count 1)))
			        (lambda ()
			          (with-exception-handler
			            (lambda (e) e)
			            (lambda ()
			              (call/cc (lambda (cont) (set! k cont) 'first)))))
			        (lambda () #f)))
			    (default-continuation-prompt-tag)
			    #f)
			  (call-with-continuation-prompt
			    (lambda () (k 'second))
			    (default-continuation-prompt-tag)
			    (lambda (v) v))
			  before-count)`,
			Expected: values.NewInteger(2),
		},
		{
			Name: "call/cc inside force in dynamic-wind",
			Code: `
			(let ((k #f)
			      (before-count 0))
			  (call-with-continuation-prompt
			    (lambda ()
			      (dynamic-wind
			        (lambda () (set! before-count (+ before-count 1)))
			        (lambda ()
			          (force (delay (call/cc (lambda (cont) (set! k cont) 'first)))))
			        (lambda () #f)))
			    (default-continuation-prompt-tag)
			    #f)
			  (call-with-continuation-prompt
			    (lambda () (k 'second))
			    (default-continuation-prompt-tag)
			    (lambda (v) v))
			  before-count)`,
			Expected: values.NewInteger(2),
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

func TestApplyErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "apply non-procedure", Code: `(apply 5 '(1 2))`},
		{Name: "apply without list", Code: `(apply + 1 2 3)`},
		{Name: "apply with improper list", Code: `(apply + '(1 . 2))`},
		{Name: "too many args", Code: `(apply (lambda (x y) (+ x y)) '(1 2 3))`},
		{Name: "too few args", Code: `(apply (lambda (x y) (+ x y)) '(1))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestApplyTailRecursion_NoStackOverflow is the H1 acceptance test:
// apply in tail position must not grow the Go stack.
// Before compiled apply, Go stack overflow occurred at ~300K iterations
// because PrimApply created a new sub-context per call. With compiled apply,
// tail-position apply emits OpUnpackListToStack + Pull + Apply bytecode,
// running in constant Go stack space.
func TestApplyTailRecursion_NoStackOverflow(t *testing.T) {
	code := `
		(begin
			(define (f n)
				(if (zero? n)
					'done
					(apply f (list (- n 1)))))
			(f 1000000))
	`
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, 30*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("done"))
}

// TestCompiledApply tests correctness of the compiled apply path.
// These cases exercise paths not covered by TestApplyComprehensive:
// non-tail position, first-class apply, and call/cc interaction.
func TestCompiledApply(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "non-tail position",
			Code:     `(+ 1 (apply + '(2 3)))`,
			Expected: values.NewInteger(6),
		},
		{
			Name:     "first-class apply",
			Code:     `(let ((a apply)) (a + '(1 2)))`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "apply with call/cc",
			Code:     `(call-with-current-continuation (lambda (k) (apply k '(42))))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "apply in tail position of lambda",
			Code:     `((lambda (x y) (apply + (list x y))) 10 20)`,
			Expected: values.NewInteger(30),
		},
		{
			Name:     "apply chain",
			Code:     `(apply apply (list apply (list + '(1 2 3))))`,
			Expected: values.NewInteger(6),
		},
		{
			Name:     "apply with rest args lambda",
			Code:     `(apply (lambda (x . rest) rest) '(1 2 3))`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(3)),
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

// TestCompiledApply_Errors tests error conditions for the compiled apply path.
func TestCompiledApply_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "improper list", Code: `(apply + '(1 . 2))`},
		{Name: "non-list final arg", Code: `(apply + 42)`},
		{Name: "non-procedure", Code: `(apply 42 '(1 2))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}
