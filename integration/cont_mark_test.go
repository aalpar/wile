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

package integration_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

func TestWithContinuationMark_BasicValue(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	result, err := engine.Eval(context.Background(),
		engine.MustParse(context.Background(), "(with-continuation-mark 'k 1 42)"))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "42")
}

func TestWithContinuationMark_TailCallPreservation(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	// Tail-recursive loop with mark — should not stack overflow
	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(letrec ((loop (lambda (n)
			(with-continuation-mark 'iter n
				(if (= n 0)
					'done
					(loop (- n 1)))))))
			(loop 100000))
	`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "done")
}

func TestWithContinuationMark_NonTailRestore(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	// After with-continuation-mark in non-tail, subsequent code runs normally
	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(let ((x (with-continuation-mark 'k 1 42)))
			(+ x 1))
	`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "43")
}

func TestWithContinuationMark_WithLambda(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(with-continuation-mark 'k 'outer
			((lambda () 'inner-result)))
	`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "inner-result")
}

func TestWithContinuationMark_NestedNonTail(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	// Nested non-tail marks both save/restore correctly
	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(+ (with-continuation-mark 'a 1
				(with-continuation-mark 'b 2
					10))
			(with-continuation-mark 'c 3
				20))
	`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "30")
}

// TestWithContinuationMark_CrossTemplateSymbolKey verifies that a mark
// installed with a quoted symbol key is found when queried with the same
// symbol name from a different compiled function. Without EqIdentity-based
// lookup, the map uses pointer identity and the mark is silently lost because
// two NativeTemplates produce distinct *Symbol pointers for the same name.
func TestWithContinuationMark_CrossTemplateSymbolKey(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	// The lambda body is a separate NativeTemplate from the outer expression,
	// so 'my-key in the lambda and 'my-key in the with-continuation-mark are
	// distinct *Symbol pointers. EqIdentity must be used for lookup.
	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(let ((read-marks (lambda ()
				(continuation-mark-set->list
					(current-continuation-marks)
					'my-key))))
			(with-continuation-mark 'my-key 42
				(read-marks)))
	`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(42)")
}

func TestWithContinuationMark_BodySideEffects(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	// Mark doesn't interfere with mutation
	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(let ((x 0))
			(with-continuation-mark 'k 1
				(set! x 42))
			x)
	`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "42")
}
