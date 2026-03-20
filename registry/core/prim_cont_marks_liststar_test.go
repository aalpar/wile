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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

func TestContinuationMarkSetToListStar(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// No marks: empty list
		{Name: "no-marks", Code: `
			(continuation-mark-set->list*
			  (current-continuation-marks)
			  '(a b))`,
			Expected: values.EmptyList},

		// Single key, one frame
		{Name: "single-key", Code: `
			(with-continuation-mark 'a 1
			  (let ((result (continuation-mark-set->list*
			                  (current-continuation-marks)
			                  '(a))))
			    (equal? result (list (vector 1)))))`,
			Expected: values.TrueValue},

		// Two keys, one frame, both present
		{Name: "two-keys-both-present", Code: `
			(with-continuation-mark 'a 1
			  (with-continuation-mark 'b 2
			    (let ((result (continuation-mark-set->list*
			                    (current-continuation-marks)
			                    '(a b))))
			      (equal? result (list (vector 1 2))))))`,
			Expected: values.TrueValue},

		// Two keys, one frame, one missing uses none-v
		{Name: "missing-key-uses-none-v", Code: `
			(with-continuation-mark 'a 1
			  (let ((result (continuation-mark-set->list*
			                  (current-continuation-marks)
			                  '(a b)
			                  'missing)))
			    (equal? result (list (vector 1 'missing)))))`,
			Expected: values.TrueValue},

		// Missing key default is #f
		{Name: "missing-key-default-false", Code: `
			(with-continuation-mark 'a 1
			  (let ((result (continuation-mark-set->list*
			                  (current-continuation-marks)
			                  '(a b))))
			    (equal? result (list (vector 1 #f)))))`,
			Expected: values.TrueValue},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestContinuationMarkSetToListStarErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "not-a-mark-set", Code: `(continuation-mark-set->list* 42 '(a))`},
		{Name: "not-a-list", Code: `(continuation-mark-set->list* (current-continuation-marks) 42)`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestContinuationPromptAvailableQ(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Default prompt tag is always available (wraps each top-level interaction)
		{Name: "default-available", Code: `
			(continuation-prompt-available? (default-continuation-prompt-tag))`,
			Expected: values.TrueValue},

		// Custom tag not available
		{Name: "custom-not-available", Code: `
			(let ((tag (make-continuation-prompt-tag 'test)))
			  (continuation-prompt-available? tag))`,
			Expected: values.FalseValue},

		// Custom tag available inside prompt
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
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestContinuationPromptAvailableQErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "not-a-tag", Code: `(continuation-prompt-available? 42)`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
