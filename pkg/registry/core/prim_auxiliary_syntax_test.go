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

// TestAuxiliarySyntax_CondElse verifies that auxiliary syntax `else` works
// correctly in `cond` forms after importing (scheme base).
func TestAuxiliarySyntax_CondElse(t *testing.T) {
	c := qt.New(t)

	result, err := testhelpers.RunSchemeCode(t, "(cond (else 42))")
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(42))
}

// TestAuxiliarySyntax_CondArrow verifies that auxiliary syntax `=>` works
// correctly in `cond` forms after importing (scheme base).
func TestAuxiliarySyntax_CondArrow(t *testing.T) {
	c := qt.New(t)

	result, err := testhelpers.RunSchemeCode(t, "(cond (#t => (lambda (x) x)))")
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.TrueValue)
}

// TestAuxiliarySyntax_CaseElse verifies that auxiliary syntax `else` works
// correctly in `case` forms after importing (scheme base).
func TestAuxiliarySyntax_CaseElse(t *testing.T) {
	c := qt.New(t)

	result, err := testhelpers.RunSchemeCode(t, "(case 1 (else 'ok))")
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewSymbol("ok"))
}
