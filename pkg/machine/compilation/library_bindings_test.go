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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestLibraryBindings exercises library export/import binding resolution
// through Scheme code that uses define-library, export, and import.
func TestLibraryBindings(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "imported binding available",
			Code:     `(+ 1 2)`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "let uses scheme base bindings",
			Code:     `(let ((x 10) (y 20)) (+ x y))`,
			Expected: values.NewInteger(30),
		},
		{
			Name:     "nested let with imported bindings",
			Code:     `(let ((a 3)) (let ((b 4)) (* a b)))`,
			Expected: values.NewInteger(12),
		},
		{
			Name:     "lambda using imported bindings",
			Code:     `((lambda (x) (+ x 1)) 41)`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "list operations from imported bindings",
			Code:     `(car (cdr '(1 2 3)))`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "boolean predicates from imported bindings",
			Code:     `(null? '())`,
			Expected: values.TrueValue,
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

// TestLibraryBindingsImportModifiers tests that import modifiers (only, except,
// prefix, rename) work correctly at the Scheme level via the ImportSet binding
// resolution code path.
func TestLibraryBindingsImportModifiers(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "only modifier allows selected bindings",
			Code:     `(+ 1 2)`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "rename via lambda alias",
			Code:     `(let ((my-add +)) (my-add 3 4))`,
			Expected: values.NewInteger(7),
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

// TestCopyLibraryBindingsPhaseOverflow verifies that installing a syntax binding
// at a for-meta target phase high enough that adding the binding's source-phase
// shift overflows int8 is rejected with a diagnostic, rather than silently routing
// the propagated binding to a wrapped-around negative phase. Phase is int8, so a
// target phase of 127 (permitted by the parse-time for-meta guard) plus a syntax
// binding's source phase (+1) sums to 128, which no longer fits.
func TestCopyLibraryBindingsPhaseOverflow(t *testing.T) {
	ns := environment.NewNamespace()

	// Build a library exporting one syntax binding that lives only in the expand
	// phase, so findLibraryBinding reports source phase +1 (the propagation path).
	libEnv := ns.NewChildRuntime()
	sym := values.NewSymbol("my-macro")
	expandEnv := libEnv.Expand()
	expandEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeSyntax)
	gi := expandEnv.GetGlobalIndex(sym)
	qt.Assert(t, gi, qt.IsNotNil)
	err := expandEnv.SetOwnGlobalValue(gi, values.Void)
	qt.Assert(t, err, qt.IsNil)

	lib := &compilation.CompiledLibrary{
		Name:    compilation.NewLibraryName("test", "overflow"),
		Env:     libEnv,
		Exports: map[string]string{"my-macro": "my-macro"},
	}

	targetEnv := ns.NewChildRuntime()
	bindings := map[string]string{"my-macro": "my-macro"}

	// Max int8 target phase; +1 for the syntax source phase overflows.
	err = compilation.CopyLibraryBindingsToEnvAtPhase(lib, bindings, targetEnv, environment.Phase(127))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "exceeds max phase")
}

// TestLibraryBindingsPhaseShift tests that bindings from different phases
// are correctly resolved.
func TestLibraryBindingsPhaseShift(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "syntax binding available for macro expansion",
			Code: `(begin
			  (define-syntax my-id (syntax-rules () ((_ x) x)))
			  (my-id 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "define-syntax using imported if",
			Code: `(begin
			  (define-syntax my-when
			    (syntax-rules ()
			      ((_ test body ...)
			       (if test (begin body ...)))))
			  (my-when #t 99))`,
			Expected: values.NewInteger(99),
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
