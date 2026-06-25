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
