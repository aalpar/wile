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

// TestBootstrapMacroReferentialTransparency pins R7RS §4.3.2: a bootstrap macro's
// template free identifier must resolve in the macro's definition environment (the
// sealed base), NOT be captured by a use-site top-level redefinition. `unless`->`not`
// and `guard`->`with-exception-handler` reference bootstrap PROCEDURES; before the
// load-order fix their free-id snapshot was a nil pin that a use-site redefine
// captured. `case`->`memv` (a Go primitive, pinned) is the positive control.
func TestBootstrapMacroReferentialTransparency(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "unless not-shadow does not capture (skips body)",
			Code:     `(begin (define not (lambda (x) x)) (unless #t 'BODY))`,
			Expected: values.Void,
		},
		{
			Name:     "unless still runs body when test is false",
			Code:     `(begin (unless #f 'BODY))`,
			Expected: values.NewSymbol("BODY"),
		},
		{
			Name:     "guard with-exception-handler-shadow does not capture",
			Code:     `(begin (define with-exception-handler (lambda (h t) 'HIJACK)) (guard (e (#t 'caught)) (raise 'x)))`,
			Expected: values.NewSymbol("caught"),
		},
		{
			Name:     "case memv-shadow control still pins (Go primitive)",
			Code:     `(begin (define memv (lambda (x l) #f)) (case 2 ((2) 'ok) (else 'other)))`,
			Expected: values.NewSymbol("ok"),
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
