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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

// TestContinuationMultipleValues pins R7RS §6.10: invoking a continuation with
// N values resumes the captured computation with all N values, not just the
// first. The bug (captured_continuation.go val:=args[0],
// machine_context_apply.go applyComposableContinuation len(args)!=1 +
// val:=args[0], AcceptsArity n==1) silently dropped every value past the first.
//
// Reference: Petite Chez yields (1 2 3) / 30 / zero for these forms; Wile
// yielded (1) / <error or 10> before the fix. These are TAIL-position call/cc
// inside a call-with-values producer, so they exercise the composable-
// continuation empty-segment path, not the sub-context truncation bug.
func TestContinuationMultipleValues(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "three values into list consumer",
			Code:     `(call-with-values (lambda () (call/cc (lambda (k) (k 1 2 3)))) list)`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name:     "two values into + consumer",
			Code:     `(call-with-values (lambda () (call/cc (lambda (k) (k 10 20)))) +)`,
			Expected: values.NewInteger(30),
		},
		{
			Name:     "zero values into nullary consumer",
			Code:     `(call-with-values (lambda () (call/cc (lambda (k) (k)))) (lambda () 'zero))`,
			Expected: values.NewSymbol("zero"),
		},
		{
			Name:     "single value still works",
			Code:     `(call-with-values (lambda () (call/cc (lambda (k) (k 42)))) (lambda (v) v))`,
			Expected: values.NewInteger(42),
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

// TestPromptNoHandlerMultipleValues pins R7RS §6.10 value forwarding through the
// no-handler (handler == #f) branch of call-with-continuation-prompt: an abort
// with N values must deliver all N to the prompt boundary, not just the first
// (and zero values must deliver zero, not a fabricated Void). This is the same
// value-truncation the captured/composable apply paths fixed; the no-handler
// prompt branch (prim_prompt.go) is a fourth site of the identical pattern.
func TestPromptNoHandlerMultipleValues(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "three values to no-handler prompt",
			Code: `(let ((t (make-continuation-prompt-tag)))
			          (call-with-values
			            (lambda () (call-with-continuation-prompt
			                         (lambda () (abort-current-continuation t 10 20 30))
			                         t #f))
			            list))`,
			Expected: values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
		},
		{
			Name: "single value to no-handler prompt",
			Code: `(let ((t (make-continuation-prompt-tag)))
			          (call-with-continuation-prompt
			            (lambda () (abort-current-continuation t 99))
			            t #f))`,
			Expected: values.NewInteger(99),
		},
		{
			Name: "zero values to no-handler prompt",
			Code: `(let ((t (make-continuation-prompt-tag)))
			          (call-with-values
			            (lambda () (call-with-continuation-prompt
			                         (lambda () (abort-current-continuation t))
			                         t #f))
			            (lambda () 'none)))`,
			Expected: values.NewSymbol("none"),
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
