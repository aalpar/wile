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

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestPrompt_BasicAbortRoundTrip(t *testing.T) {
	c := qt.New(t)

	tcs := []schemeCodeTestCase{
		{
			name: "abort with single value",
			code: `(let ((tag (make-continuation-prompt-tag 'test)))
				(call-with-continuation-prompt
				  (lambda () (abort-current-continuation tag 42))
				  tag
				  (lambda (v) v)))`,
			expected: values.NewInteger(42),
		},
		{
			name: "normal return through prompt",
			code: `(let ((tag (make-continuation-prompt-tag 'test)))
				(call-with-continuation-prompt
				  (lambda () 99)
				  tag
				  (lambda (v) v)))`,
			expected: values.NewInteger(99),
		},
		{
			name: "abort with multiple values to handler",
			code: `(let ((tag (make-continuation-prompt-tag 'test)))
				(call-with-continuation-prompt
				  (lambda () (abort-current-continuation tag 1 2 3))
				  tag
				  (lambda (a b c) (+ a b c))))`,
			expected: values.NewInteger(6),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.DeepEquals, tc.expected)
		})
	}
}

func TestPrompt_ContinuationPromptTagPredicate(t *testing.T) {
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
				runSchemeCodeExpectTrue(t, tc.code)
			} else {
				runSchemeCodeExpectFalse(t, tc.code)
			}
		})
	}
}

func TestPrompt_ComposableContinuation(t *testing.T) {
	c := qt.New(t)

	tcs := []schemeCodeTestCase{
		{
			name: "capture and apply composable continuation",
			code: `(let ((tag (make-continuation-prompt-tag 'test)))
				(call-with-continuation-prompt
				  (lambda ()
				    (+ 1 (call-with-composable-continuation
				            (lambda (k) (k 10))
				            tag)))
				  tag
				  #f))`,
			expected: values.NewInteger(11),
		},
		{
			name: "composable continuation applied multiple times",
			code: `(let ((tag (make-continuation-prompt-tag 'test)))
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
			expected: values.NewInteger(21),
		},
		{
			// Regression: OpApply uses Drain which returns a slice aliasing
			// the evals backing array. When applyComposableContinuation calls
			// Restore, releaseStack nils elements in that shared array. The
			// argument value (42) must be saved to a local before Restore;
			// otherwise it reads as nil/0 and the result would be 1 not 43.
			name: "composable continuation preserves argument through Restore",
			code: `(let ((tag (make-continuation-prompt-tag 'test)))
				(let ((k #f))
				  (call-with-continuation-prompt
				    (lambda ()
				      (+ 1 (call-with-composable-continuation
				              (lambda (c) (set! k c) 0)
				              tag)))
				    tag
				    #f)
				  (k 42)))`,
			expected: values.NewInteger(43),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.DeepEquals, tc.expected)
		})
	}
}

func TestPrompt_NestedPrompts(t *testing.T) {
	c := qt.New(t)

	tcs := []schemeCodeTestCase{
		{
			name: "nested prompts with different tags",
			code: `(let ((tag1 (make-continuation-prompt-tag 'outer))
				       (tag2 (make-continuation-prompt-tag 'inner)))
				(call-with-continuation-prompt
				  (lambda ()
				    (call-with-continuation-prompt
				      (lambda () (abort-current-continuation tag2 42))
				      tag2
				      (lambda (v) (+ v 1))))
				  tag1
				  (lambda (v) (* v 100))))`,
			expected: values.NewInteger(43),
		},
		{
			name: "abort to outer prompt skipping inner prompt",
			code: `(let ((tag1 (make-continuation-prompt-tag 'outer))
				       (tag2 (make-continuation-prompt-tag 'inner)))
				(call-with-continuation-prompt
				  (lambda ()
				    (call-with-continuation-prompt
				      (lambda () (abort-current-continuation tag1 42))
				      tag2
				      (lambda (v) (+ v 1))))
				  tag1
				  (lambda (v) (* v 100))))`,
			expected: values.NewInteger(4200),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.DeepEquals, tc.expected)
		})
	}
}

func TestPrompt_DynamicWind(t *testing.T) {
	c := qt.New(t)

	tcs := []schemeCodeTestCase{
		{
			name: "dynamic-wind after thunk runs on abort",
			code: `(let ((tag (make-continuation-prompt-tag 'test))
				       (log '()))
				(call-with-continuation-prompt
				  (lambda ()
				    (dynamic-wind
				      (lambda () (set! log (cons 'before log)))
				      (lambda () (abort-current-continuation tag 42))
				      (lambda () (set! log (cons 'after log)))))
				  tag
				  (lambda (v) log)))`,
			expected: values.List(
				values.NewSymbol("after"),
				values.NewSymbol("before")),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.DeepEquals, tc.expected)
		})
	}
}
