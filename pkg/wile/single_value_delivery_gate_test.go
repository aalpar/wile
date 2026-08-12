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

package wile

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// evalStringErr evaluates src on a fresh engine and returns the error, if any.
// The assertions below are Scheme-level because the defect is reachable through
// four opcodes -- OpSelfTailCall, a fused promoted op, OpStoreLocal and
// OpStoreGlobal -- and only a Scheme witness crosses all four.
func evalStringErr(t *testing.T, src string) error {
	t.Helper()
	ctx := context.Background()
	engine, err := NewEngine(ctx, WithProfile(Small))
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	_, err = engine.EvalMultiple(ctx, src)
	return err
}

// TestSingleValueDelivery_Raises is the must-fail-first gate for reviews/2026-08-07
// item 2.2.20/2.2.21 (Wave 1 §4).
//
// OpPush used to deliver the value register's LIVE ARITY into an argument slot:
// PushAll for multiple values, nothing at all for zero. Every drain that reads a
// count fixed at compile time -- DrainN(argCount), Stack.Pop2, Stack.Pop -- then
// read a depth the stack did not have. The observed answer at 77f23e03 is recorded
// per row, so a future reader can tell a regression from a re-specification.
//
// R7RS §6.10 leaves the count unspecified, so no particular number was owed. Chez
// and Racket both raise at the delivery point, and that is the answer adopted here.
func TestSingleValueDelivery_Raises(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name string
		src  string
	}{
		{
			// 77f23e03: 6 -- both values spliced into the variadic +
			name: "fused promoted op, first operand",
			src:  "(+ (values 1 2) 3)",
		},
		{
			// 77f23e03: 6
			name: "fused promoted op, second operand",
			src:  "(+ 3 (values 1 2))",
		},
		{
			// 77f23e03: panic "Stack.Pop2: fewer than two values"
			name: "fused promoted op, zero values",
			src:  "(define (h x) (+ (values) x)) (h 3)",
		},
		{
			// 77f23e03: panic "Stack.Pop: stack is empty"
			name: "OpStoreGlobal, zero values",
			src:  "(define x (values)) x",
		},
		{
			// 77f23e03: panic "Stack.Pop: stack is empty"
			name: "OpStoreLocal, zero values",
			src:  "(let ((x (values))) x)",
		},
		{
			// 77f23e03: (1 a) -- Pop took the top value and left the other on
			// the eval stack, where the BODY's list call drained it. This row is
			// the residue case: the drain that misbehaves belongs to another
			// expression entirely, so no per-drain pre-check can see it.
			name: "OpStoreLocal residue leaks into a later call",
			src:  "(define (f) (let ((x (values 1 2))) (list 'a))) (f)",
		},
		{
			// 77f23e03: (1 2 9)
			name: "generic apply splices",
			src:  "(list (values 1 2) 9)",
		},
		{
			// 77f23e03: 2 -- through tryInlineCall; the direct form
			// ((lambda (x) x) (values 1 2)) errored cleanly only because the
			// operator-position gate had not been lifted yet.
			name: "inlined let-bound lambda",
			src:  "(let ((f (lambda (x) x))) (f (values 1 2)))",
		},
		{
			// 77f23e03: (1 99 7) -- DrainN(argCount) took a compile-time 2 off a
			// 3-deep stack and bound the wrong parameters. This reproduces the
			// review's loopH symptom verbatim. Top-level define under the default
			// immutable top level is what arms OpSelfTailCall; named-let and
			// letrec take the generic path and error cleanly.
			name: "OpSelfTailCall",
			src: "(define (loop i n) (if (>= i n) (list i n) (loop (values (+ i 1) 99) n)))" +
				" (loop 0 7)",
		},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			err := evalStringErr(t, tt.src)
			c.Assert(err, qt.ErrorIs, werr.ErrWrongNumberOfArguments)
		})
	}
}

// TestSingleValueDelivery_MultiValueSeamStillWorks pins the other half of the
// split: OpPushValues remains multiple-values-aware, so every form that legitimately
// delivers 0/1/N values keeps working. Without this the strict OpPush silently takes
// call-with-values, let-values, define-values and the raise escalator with it --
// which is exactly what a first attempt did, because applyToValuesCode's OpPush is
// hand-built in run_body_under_frame.go and does not appear among compilation's
// NewOperationPush sites.
func TestSingleValueDelivery_MultiValueSeamStillWorks(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "call-with-values, two values",
			src:  "(call-with-values (lambda () (values 1 2)) +)",
			want: "3",
		},
		{
			name: "call-with-values, zero values",
			src:  "(call-with-values (lambda () (values)) list)",
			want: "()",
		},
		{
			name: "call-with-values, one value",
			src:  "(call-with-values (lambda () 7) list)",
			want: "(7)",
		},
		{
			name: "let-values",
			src:  "(let-values (((a b) (values 1 2))) (list a b))",
			want: "(1 2)",
		},
		{
			name: "define-values",
			src:  "(define-values (a b) (values 1 2)) (list a b)",
			want: "(1 2)",
		},
		{
			// dynamic-wind boxes its thunk result for its own fixed stack layout;
			// the unbox must still deliver both values to the consumer.
			name: "multiple values propagate out of dynamic-wind",
			src: "(call-with-values" +
				" (lambda () (dynamic-wind (lambda () 1) (lambda () (values 1 2)) (lambda () 3)))" +
				" list)",
			want: "(1 2)",
		},
		{
			// One value through `values` is a single-value register, so it is
			// delivered by OpPush like any other expression.
			name: "single value passes through OpPush",
			src:  "(car (values '(1 2)))",
			want: "1",
		},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			c.Assert(evalString(t, tt.src), qt.Equals, tt.want)
		})
	}
}
