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

import (
	"context"
	"testing"

	"github.com/aalpar/wile"

	qt "github.com/frankban/quicktest"
)

// evalFlatClosureTest creates a fresh engine, evaluates the Scheme code,
// and returns the result's SchemeString representation. Fatals on error.
func evalFlatClosureTest(t *testing.T, code string) string {
	t.Helper()
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)
	result, err := engine.EvalMultiple(ctx, code)
	qt.Assert(t, err, qt.IsNil)
	return result.SchemeString()
}

func TestFlatClosure_Integration_SimpleCapture(t *testing.T) {
	// Simple closure capturing a free variable from an enclosing let.
	qt.Assert(t, evalFlatClosureTest(t,
		`(let ((x 42)) ((lambda () x)))`),
		qt.Equals, "42")
}

func TestFlatClosure_Integration_SetBangBoxed(t *testing.T) {
	// Closure with set! (boxed variable): inc mutates x, then reads x.
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((x 0))
		  (let ((inc (lambda () (set! x (+ x 1)))))
		    (inc) (inc) x))
	`), qt.Equals, "2")
}

func TestFlatClosure_Integration_SharedBoxedVar(t *testing.T) {
	// Multiple closures sharing a boxed variable.
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((x 0))
		  (let ((inc (lambda () (set! x (+ x 1))))
		        (get (lambda () x)))
		    (inc) (inc) (get)))
	`), qt.Equals, "2")
}

func TestFlatClosure_Integration_NestedClosures(t *testing.T) {
	// Nested closures: outer captures x, inner captures both x and y.
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((x 10))
		  (let ((f (lambda (y) (lambda () (+ x y)))))
		    ((f 5))))
	`), qt.Equals, "15")
}

func TestFlatClosure_Integration_NoCapture(t *testing.T) {
	// Closure capturing nothing — should work identically to linked model.
	qt.Assert(t, evalFlatClosureTest(t,
		`((lambda (x) (+ x 1)) 10)`),
		qt.Equals, "11")
}

func TestFlatClosure_Integration_RecursiveFib(t *testing.T) {
	// Recursive closure (fibonacci) using letrec.
	qt.Assert(t, evalFlatClosureTest(t, `
		(letrec ((fib (lambda (n)
		               (if (<= n 1) n
		                   (+ (fib (- n 1)) (fib (- n 2)))))))
		  (fib 10))
	`), qt.Equals, "55")
}

func TestFlatClosure_Integration_ClosureAsArgument(t *testing.T) {
	// Passing a closure as an argument to a higher-order function.
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((x 100))
		  (let ((add-x (lambda (y) (+ x y))))
		    (let ((apply-fn (lambda (f v) (f v))))
		      (apply-fn add-x 23))))
	`), qt.Equals, "123")
}

func TestFlatClosure_Integration_MakeCounter(t *testing.T) {
	// Classic counter pattern: closure over mutable state.
	qt.Assert(t, evalFlatClosureTest(t, `
		(define (make-counter)
		  (let ((n 0))
		    (lambda ()
		      (set! n (+ n 1))
		      n)))
		(let ((c (make-counter)))
		  (c) (c) (c))
	`), qt.Equals, "3")
}

func TestFlatClosure_Integration_LetrecMutualRecursion(t *testing.T) {
	// Mutual recursion via letrec — even?/odd? pattern.
	qt.Assert(t, evalFlatClosureTest(t, `
		(letrec ((my-even? (lambda (n) (if (= n 0) #t (my-odd? (- n 1)))))
		         (my-odd?  (lambda (n) (if (= n 0) #f (my-even? (- n 1))))))
		  (my-even? 10))
	`), qt.Equals, "#t")
}

func TestFlatClosure_Integration_DeepNesting(t *testing.T) {
	// Three levels of closure nesting, each capturing from the one above.
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((a 1))
		  (let ((b 2))
		    (let ((c 3))
		      ((lambda () (+ a b c))))))
	`), qt.Equals, "6")
}

func TestFlatClosure_Integration_CaseLambdaCapture(t *testing.T) {
	// case-lambda where clauses capture a free variable.
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((base 10))
		  (let ((f (case-lambda
		             ((x) (+ base x))
		             ((x y) (+ base x y)))))
		    (+ (f 5) (f 3 7))))
	`), qt.Equals, "35")
}

func TestFlatClosure_Integration_MapWithClosure(t *testing.T) {
	// Using a closure with map (higher-order standard library function).
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((offset 10))
		  (map (lambda (x) (+ x offset)) '(1 2 3)))
	`), qt.Equals, "(11 12 13)")
}

func TestFlatClosure_Integration_MixedBoxedNonBoxed(t *testing.T) {
	// x is captured+mutated (boxed), y is captured-only (not boxed).
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((x 0) (y 10))
		  (let ((inc (lambda () (set! x (+ x 1))))
		        (get (lambda () (+ x y))))
		    (inc) (inc) (get)))
	`), qt.Equals, "12")
}

func TestFlatClosure_Integration_CallCC(t *testing.T) {
	// call/cc captures and restores freeVars across continuation invocation.
	// Uses define for letrec semantics so k is visible in f's body.
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((x 10))
		  (define k #f)
		  (define (f)
		    (call-with-current-continuation
		      (lambda (c) (set! k c) 1)))
		  (let ((r (f)))
		    (if (= r 1)
		        (k 2)
		        (+ x r))))
	`), qt.Equals, "12")
}

func TestFlatClosure_Integration_RecursiveWithSetBang(t *testing.T) {
	// Recursive closure using set! on a captured counter variable.
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((count 0))
		  (letrec ((loop (lambda (n)
		                   (if (= n 0) count
		                       (begin (set! count (+ count 1))
		                              (loop (- n 1)))))))
		    (loop 5)))
	`), qt.Equals, "5")
}

func TestFlatClosure_Integration_VariadicCapture(t *testing.T) {
	// Variadic closure that captures a free variable.
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((base 100))
		  (let ((sum-with-base (lambda args
		                         (apply + base args))))
		    (sum-with-base 1 2 3)))
	`), qt.Equals, "106")
}

func TestFlatClosure_Integration_DynamicWind(t *testing.T) {
	// dynamic-wind with flat closure: before/after thunks capture a log variable.
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((log '()))
		  (dynamic-wind
		    (lambda () (set! log (cons 'before log)))
		    (lambda () (set! log (cons 'during log)) 42)
		    (lambda () (set! log (cons 'after log))))
		  (reverse log))
	`), qt.Equals, "(before during after)")
}

func TestFlatClosure_Integration_TailCallBetweenFlat(t *testing.T) {
	// Tail calls between two different flat closures: freeVars must update.
	qt.Assert(t, evalFlatClosureTest(t, `
		(let ((x 1) (y 2))
		  (letrec ((f (lambda (n) (if (= n 0) x (g (- n 1)))))
		           (g (lambda (n) (if (= n 0) y (f (- n 1))))))
		    (+ (f 0) (f 1) (f 2) (f 3))))
	`), qt.Equals, "6")
}
