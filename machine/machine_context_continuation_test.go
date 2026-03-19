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

package machine_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestMachineContextContinuation(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic call/cc
		{
			Name:     "call/cc normal return",
			Code:     `(call/cc (lambda (k) 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "call/cc with escape",
			Code:     `(call/cc (lambda (k) (k 42)))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "call/cc escape skips remaining",
			Code:     `(+ 1 (call/cc (lambda (k) (+ 2 (k 10)))))`,
			Expected: values.NewInteger(11),
		},
		{
			Name:     "call/cc normal return in expression",
			Code:     `(+ 1 (call/cc (lambda (k) 10)))`,
			Expected: values.NewInteger(11),
		},
		{
			Name:     "call-with-current-continuation alias",
			Code:     `(call-with-current-continuation (lambda (k) (k 99)))`,
			Expected: values.NewInteger(99),
		},

		// Delimited continuations
		{
			Name: "call-with-continuation-prompt basic",
			Code: `(call-with-continuation-prompt
                     (lambda () (abort-current-continuation
                                  (default-continuation-prompt-tag) 42))
                     (default-continuation-prompt-tag)
                     (lambda (v) v))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "call-with-continuation-prompt no abort",
			Code: `(call-with-continuation-prompt
                     (lambda () 99)
                     (default-continuation-prompt-tag)
                     (lambda (v) v))`,
			Expected: values.NewInteger(99),
		},

		// Continuation used to implement early return
		{
			Name: "call/cc early return from loop",
			Code: `(call/cc
                     (lambda (return)
                       (let loop ((i 0))
                         (if (= i 5)
                             (return i)
                             (loop (+ i 1))))))`,
			Expected: values.NewInteger(5),
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
