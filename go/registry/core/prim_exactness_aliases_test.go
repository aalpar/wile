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

	"github.com/aalpar/wile/go/values"

	qt "github.com/frankban/quicktest"
)

// TestExactnessAliases tests exact->inexact and inexact->exact R5RS aliases.
// R7RS §6.2.6: These are R5RS compatibility aliases.
func TestExactnessAliases(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// exact->inexact (alias for inexact)
		{name: "exact->inexact on integer", code: `(exact->inexact 3)`, expected: values.NewFloat(3.0)},
		{name: "exact->inexact on rational", code: `(exact->inexact 1/2)`, expected: values.NewFloat(0.5)},
		{name: "exact->inexact on float passthrough", code: `(exact->inexact 3.0)`, expected: values.NewFloat(3.0)},

		// inexact->exact (alias for exact)
		{name: "inexact->exact on float 0.5", code: `(inexact->exact 0.5)`, expected: values.NewRational(1, 2)},
		{name: "inexact->exact on integer float", code: `(inexact->exact 3.0)`, expected: values.NewInteger(3)},
		{name: "inexact->exact on integer passthrough", code: `(inexact->exact 3)`, expected: values.NewInteger(3)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}
