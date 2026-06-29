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

	qt "github.com/frankban/quicktest"
)

func TestPrompt_BasicAbortRoundTrip(t *testing.T) {
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

func TestPrompt_NestedPrompts(t *testing.T) {
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
