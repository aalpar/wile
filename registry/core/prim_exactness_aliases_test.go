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

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestExactnessAliases tests exact->inexact and inexact->exact R5RS aliases.
// R7RS §6.2.6: These are R5RS compatibility aliases.
func TestExactnessAliases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// exact->inexact (alias for inexact)
		{Name: "exact->inexact on integer", Code: `(exact->inexact 3)`, Expected: values.NewFloat(3.0)},
		{Name: "exact->inexact on rational", Code: `(exact->inexact 1/2)`, Expected: values.NewFloat(0.5)},
		{Name: "exact->inexact on float passthrough", Code: `(exact->inexact 3.0)`, Expected: values.NewFloat(3.0)},

		// inexact->exact (alias for exact)
		{Name: "inexact->exact on float 0.5", Code: `(inexact->exact 0.5)`, Expected: values.NewRational(1, 2)},
		{Name: "inexact->exact on integer float", Code: `(inexact->exact 3.0)`, Expected: values.NewInteger(3)},
		{Name: "inexact->exact on integer passthrough", Code: `(inexact->exact 3)`, Expected: values.NewInteger(3)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
