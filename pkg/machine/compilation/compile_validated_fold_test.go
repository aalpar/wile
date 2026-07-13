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

package compilation_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestTier2FormInIfTestIsNotFolded guards the R7RS feature-detection idiom.
//
// cond-expand/include/import are Tier-2 passthrough forms represented as
// *validate.ValidatedLiteral — the same Go type as a self-evaluating datum. The
// if constant-fold must not mistake them for truthy literals and discard them
// uncompiled.
func TestTier2FormInIfTestIsNotFolded(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "if with false cond-expand test",
			Code:     `(if (cond-expand (else #f)) 'yes 'no)`,
			Expected: values.NewSymbol("no"),
		},
		{
			Name:     "if with true cond-expand test",
			Code:     `(if (cond-expand (else #t)) 'yes 'no)`,
			Expected: values.NewSymbol("yes"),
		},
		{
			Name:     "cond with cond-expand test",
			Code:     `(cond ((cond-expand (else #f)) 'yes) (else 'no))`,
			Expected: values.NewSymbol("no"),
		},
		{
			Name:     "and with cond-expand test",
			Code:     `(and (cond-expand (else #f)) 'reached)`,
			Expected: values.FalseValue,
		},

		// Regression guards: these already pass today and must keep passing.
		// `or` and `unless` bind or negate the test rather than leaving the
		// literal in if-test position, so they never had the bug. Do not widen
		// the fix until one of these goes red.
		{
			Name:     "or is unaffected",
			Code:     `(or (cond-expand (else #f)) 'fallback)`,
			Expected: values.NewSymbol("fallback"),
		},
		{
			Name:     "bare #f still folds",
			Code:     `(if #f 'yes 'no)`,
			Expected: values.NewSymbol("no"),
		},
		{
			Name:     "bare truthy literal still folds (0 is truthy in Scheme)",
			Code:     `(if 0 'yes 'no)`,
			Expected: values.NewSymbol("yes"),
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
