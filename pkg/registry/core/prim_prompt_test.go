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
	"context"
	"regexp"
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

func TestPrompt_BasicAbortRoundTrip(t *testing.T) {
	t.Parallel()

	c := qt.New(t)

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "abort with single value",
			Code: `(let ((tag (make-continuation-prompt-tag 'test)))
				(call-with-continuation-prompt
				  (lambda () (abort-current-continuation tag 42))
				  tag
				  (lambda (v) v)))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "normal return through prompt",
			Code: `(let ((tag (make-continuation-prompt-tag 'test)))
				(call-with-continuation-prompt
				  (lambda () 99)
				  tag
				  (lambda (v) v)))`,
			Expected: values.NewInteger(99),
		},
		{
			Name: "abort with multiple values to handler",
			Code: `(let ((tag (make-continuation-prompt-tag 'test)))
				(call-with-continuation-prompt
				  (lambda () (abort-current-continuation tag 1 2 3))
				  tag
				  (lambda (a b c) (+ a b c))))`,
			Expected: values.NewInteger(6),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.DeepEquals, tc.Expected)
		})
	}
}

func TestPrompt_ContinuationPromptTagPredicate(t *testing.T) {
	t.Parallel()

	c := qt.New(t)

	tcs := []struct {
		name     string
		code     string
		expected bool
	}{
		{
			name:     "prompt tag is a prompt tag",
			code:     `(continuation-prompt-tag? (make-continuation-prompt-tag))`,
			expected: true,
		},
		{
			name:     "default prompt tag is a prompt tag",
			code:     `(continuation-prompt-tag? (default-continuation-prompt-tag))`,
			expected: true,
		},
		{
			name:     "number is not a prompt tag",
			code:     `(continuation-prompt-tag? 42)`,
			expected: false,
		},
		{
			name:     "string is not a prompt tag",
			code:     `(continuation-prompt-tag? "hello")`,
			expected: false,
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			if tc.expected {
				testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
			} else {
				testhelpers.RunSchemeCodeExpectFalse(t, tc.code)
			}
		})
	}
}

func TestPrompt_ComposableContinuation(t *testing.T) {
	t.Parallel()

	c := qt.New(t)

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "capture and apply composable continuation",
			Code: `(let ((tag (make-continuation-prompt-tag 'test)))
				(call-with-continuation-prompt
				  (lambda ()
				    (+ 1 (call-with-composable-continuation
				            (lambda (k) (k 10))
				            tag)))
				  tag
				  #f))`,
			Expected: values.NewInteger(12), // Racket v9.2: cwcc composes in place (was 11)
		},
		{
			Name: "composable continuation applied multiple times",
			Code: `(let ((tag (make-continuation-prompt-tag 'test)))
				(let ((k #f))
				  (let ((result
				    (call-with-continuation-prompt
				      (lambda ()
				        (+ 1 (call-with-composable-continuation
				                (lambda (c) (set! k c) 10)
				                tag)))
				      tag
				      #f)))
				    (if k
				        (let ((saved k))
				          (set! k #f)
				          (saved 20))
				        result))))`,
			Expected: values.NewInteger(21),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.DeepEquals, tc.Expected)
		})
	}
}

// TestPrompt_ComposableContinuationAtAThreadRoot exercises
// PrimCallWithComposableContinuation where mc.Parent() == nil: a thread root
// (the thread body's context has no chain above its tail call). proc is applied
// in place there, as everywhere, and its normal return ends the thread's run;
// (k v) inside proc composes in place. Until 2026-10 this shape selected a
// second arm that ran proc on a fresh sub-context under a driver of its own;
// the rows date from covering that arm and now pin that its removal changed no
// answer here. The rows with a live chain are in TestPrompt_ComposableContinuation.
func TestPrompt_ComposableContinuationAtAThreadRoot(t *testing.T) {
	t.Parallel()

	c := qt.New(t)

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "cwcc at a thread root composes and delivers the value",
			Code: `(thread-join!
			         (thread-start!
			           (make-thread
			             (lambda ()
			               (call-with-composable-continuation
			                 (lambda (k) (k 5))
			                 (default-continuation-prompt-tag))))))`,
			Expected: values.NewInteger(5),
		},
		{
			Name: "cwcc at a thread root returns proc's value when k is not invoked",
			Code: `(thread-join!
			         (thread-start!
			           (make-thread
			             (lambda ()
			               (call-with-composable-continuation
			                 (lambda (k) 7)
			                 (default-continuation-prompt-tag))))))`,
			Expected: values.NewInteger(7),
		},
		{
			// Discriminates compose-in-place from escape at a thread root:
			// invoking k twice and summing yields 3 (each (k v) composes and
			// returns v). An escaping continuation would abort at (k 1) and never
			// reach (k 2).
			Name: "cwcc at a thread root composes in place (invoked twice)",
			Code: `(thread-join!
			         (thread-start!
			           (make-thread
			             (lambda ()
			               (call-with-composable-continuation
			                 (lambda (k) (+ (k 1) (k 2)))
			                 (default-continuation-prompt-tag))))))`,
			Expected: values.NewInteger(3),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.DeepEquals, tc.Expected)
		})
	}
}

func TestPrompt_NestedPrompts(t *testing.T) {
	t.Parallel()

	c := qt.New(t)

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "nested prompts with different tags",
			Code: `(let ((tag1 (make-continuation-prompt-tag 'outer))
				       (tag2 (make-continuation-prompt-tag 'inner)))
				(call-with-continuation-prompt
				  (lambda ()
				    (call-with-continuation-prompt
				      (lambda () (abort-current-continuation tag2 42))
				      tag2
				      (lambda (v) (+ v 1))))
				  tag1
				  (lambda (v) (* v 100))))`,
			Expected: values.NewInteger(43),
		},
		{
			Name: "abort to outer prompt skipping inner prompt",
			Code: `(let ((tag1 (make-continuation-prompt-tag 'outer))
				       (tag2 (make-continuation-prompt-tag 'inner)))
				(call-with-continuation-prompt
				  (lambda ()
				    (call-with-continuation-prompt
				      (lambda () (abort-current-continuation tag1 42))
				      tag2
				      (lambda (v) (+ v 1))))
				  tag1
				  (lambda (v) (* v 100))))`,
			Expected: values.NewInteger(4200),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.DeepEquals, tc.Expected)
		})
	}
}

func TestPrompt_DynamicWind(t *testing.T) {
	t.Parallel()

	c := qt.New(t)

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "dynamic-wind after thunk runs on abort",
			Code: `(let ((tag (make-continuation-prompt-tag 'test))
				       (log '()))
				(call-with-continuation-prompt
				  (lambda ()
				    (dynamic-wind
				      (lambda () (set! log (cons 'before log)))
				      (lambda () (abort-current-continuation tag 42))
				      (lambda () (set! log (cons 'after log)))))
				  tag
				  (lambda (v) log)))`,
			Expected: values.List(
				values.NewSymbol("after"),
				values.NewSymbol("before")),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.DeepEquals, tc.Expected)
		})
	}
}

func TestPrompt_AvailableQ(t *testing.T) {
	t.Parallel()

	c := qt.New(t)
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "default-available", Code: `
			(continuation-prompt-available? (default-continuation-prompt-tag))`,
			Expected: values.TrueValue},
		{Name: "custom-not-available", Code: `
			(let ((tag (make-continuation-prompt-tag 'test)))
			  (continuation-prompt-available? tag))`,
			Expected: values.FalseValue},
		{Name: "custom-available-inside-prompt", Code: `
			(let ((tag (make-continuation-prompt-tag 'test)))
			  (call-with-continuation-prompt
			    (lambda ()
			      (continuation-prompt-available? tag))
			    tag
			    #f))`,
			Expected: values.TrueValue},
	}

	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.DeepEquals, tc.Expected)
		})
	}
}

func TestPrompt_AvailableQErrors(t *testing.T) {
	t.Parallel()

	c := qt.New(t)
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "not-a-tag", Code: `(continuation-prompt-available? 42)`},
	}

	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNotNil)
		})
	}
}

// TestPrompt_AvailableQSeesPastEval: code eval runs in a sub-context sees the
// prompts of the code that called eval, as in Racket (every row agrees with
// racket -e). Master answered #f for the first two. A thread root has no parent,
// so a thread does not see its starter's prompts (Racket: #f too). The two
// dynamic-wind rows pin the walk's stop at an isolated context; removing it
// flips both.
func TestPrompt_AvailableQSeesPastEval(t *testing.T) {
	t.Parallel()

	c := qt.New(t)
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"default tag inside eval", `(eval '(continuation-prompt-available? (default-continuation-prompt-tag)) (interaction-environment))`, "#t"},
		{"user tag whose prompt encloses the eval", `(define t (make-continuation-prompt-tag 'u))
			(call-with-continuation-prompt (lambda () (eval '(continuation-prompt-available? t) (interaction-environment))) t (lambda a 'h))`, "#t"},
		{"user tag with no prompt", `(define t (make-continuation-prompt-tag 'u))
			(eval '(continuation-prompt-available? t) (interaction-environment))`, "#f"},
		{"abort through eval still reaches the prompt", `(define t (make-continuation-prompt-tag 'u))
			(call-with-continuation-prompt (lambda () (eval '(abort-current-continuation t 5) (interaction-environment))) t (lambda (v) (list 'h v)))`, "(h 5)"},
		{"a thread does not see its starter's prompt", `(define t (make-continuation-prompt-tag 'u))
			(call-with-continuation-prompt
			  (lambda () (thread-join! (thread-start! (make-thread (lambda () (continuation-prompt-available? t))))))
			  t (lambda a 'h))`, "#f"},
		// A dynamic-wind thunk's sub-context is isolated: its parentMC is the
		// context doing the unwind or rewind, whose chain is not the thunk's
		// continuation. Walking past it reports the jump's origin (a prompt inside
		// the extent) or its target (a prompt around the re-entry) as #t.
		{"an after thunk does not see a prompt inside its extent", `(define t (make-continuation-prompt-tag 'u))
			(define r 'unset)
			(call/cc (lambda (out)
			  (dynamic-wind (lambda () #f)
			    (lambda () (call-with-continuation-prompt (lambda () (out 1)) t (lambda a 'h)))
			    (lambda () (set! r (continuation-prompt-available? t))))))
			r`, "#f"},
		{"a re-entry's before thunk does not see a prompt around the re-entry", `(define t (make-continuation-prompt-tag 'u))
			(define r '())
			(define kk #f)
			(define n 0)
			(dynamic-wind (lambda () (set! r (cons (continuation-prompt-available? t) r)))
			  (lambda () (call/cc (lambda (k) (set! kk k))) 1)
			  (lambda () #f))
			(set! n (+ n 1))
			(if (< n 2) (call-with-continuation-prompt (lambda () (kk 2)) t (lambda a 'h)))
			r`, "(#f #f)"},
		// A call/cc re-entry inside the eval resumes at the boundary it was
		// delimited at (outerIntact), so both entries see the prompt and the
		// caller's mark, as in Racket. Master: (aborted ((1 p1 #f ()) ...)).
		{"a call/cc re-entry inside eval still sees the caller's prompt", `(define t (make-continuation-prompt-tag 'u))
			(define p (make-parameter 'p0))
			(call-with-continuation-prompt
			  (lambda ()
			    (parameterize ((p 'p1))
			      (with-continuation-mark 'a 0
			        (car (list (eval '(let ((n 0) (k #f) (seen '()))
			                            (call/cc (lambda (c) (set! k c)))
			                            (set! seen (cons (list n (p) (continuation-prompt-available? t)
			                                                   (continuation-mark-set->list (current-continuation-marks) 'a))
			                                             seen))
			                            (set! n (+ n 1))
			                            (if (< n 2) (k #f) #f)
			                            (if (continuation-prompt-available? t) seen (abort-current-continuation t seen)))
			                          (interaction-environment)))))))
			  t (lambda (v) (list 'aborted v)))`, "((1 p1 #t (0)) (0 p1 #t (0)))"},
	}
	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink), wile.WithMutableTopLevel())
			c.Assert(err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestPrompt_ComposableCaptureAcrossEvalRefused: a prompt beyond an eval
// boundary is visible but not capturable (Racket captures; the segment would
// have two owners here). The refusal is ErrNotImplemented, names the eval call
// that entered the sub-context, and is catchable inside and outside the eval.
// Master refused with "no prompt found", as if the prompt were absent.
func TestPrompt_ComposableCaptureAcrossEvalRefused(t *testing.T) {
	t.Parallel()

	c := qt.New(t)
	run := func(code string) (wile.Value, error) {
		engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
		c.Assert(err, qt.IsNil)
		return engine.EvalMultiple(context.Background(), `(define t (make-continuation-prompt-tag 'u)) `+code)
	}
	refusal := regexp.MustCompile(`^"call-with-composable-continuation: the prompt for tag #<continuation-prompt-tag:u> is outside the sub-context entered at \S*:1:\d+; capturing across that boundary is not implemented"$`)

	_, err := run(`(call-with-continuation-prompt (lambda () (+ 1 (eval '(call-with-composable-continuation (lambda (k) (k 10)) t) (interaction-environment)))) t (lambda a 'h))`)
	c.Assert(err, qt.ErrorIs, werr.ErrNotImplemented)
	c.Assert(err, qt.ErrorMatches, `(?s).*the prompt for tag #<continuation-prompt-tag:u> is outside the sub-context entered at \S*:1:\d+;.*`)

	for _, code := range []string{
		`(call-with-continuation-prompt (lambda () (guard (e (#t (error-object-message e))) (eval '(call-with-composable-continuation (lambda (k) (k 10)) t) (interaction-environment)))) t (lambda a 'h))`,
		`(call-with-continuation-prompt (lambda () (eval '(guard (e (#t (error-object-message e))) (call-with-composable-continuation (lambda (k) (k 10)) t)) (interaction-environment))) t (lambda a 'h))`,
	} {
		result, err := run(code)
		c.Assert(err, qt.IsNil, qt.Commentf("%s", code))
		c.Assert(refusal.MatchString(result.SchemeString()), qt.IsTrue, qt.Commentf("%s => %s", code, result.SchemeString()))
	}

	// With no prompt anywhere the refusal is unchanged.
	_, err = run(`(eval '(call-with-composable-continuation (lambda (k) (k 10)) t) (interaction-environment))`)
	c.Assert(err, qt.ErrorIs, werr.ErrNotAPromptTag)
	c.Assert(err, qt.ErrorMatches, `(?s).*no prompt found for tag.*`)
}
