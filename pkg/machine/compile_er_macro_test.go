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
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestCompileErMacro tests ER macro transformer compilation and round-trip
// usage through the full pipeline.
//
// Source: compile_er_macro.go (compileERMacroTransformer).
//
// Uses the two-step pattern because define-syntax is a top-level form.
func TestCompileErMacro(t *testing.T) {
	tcs := []struct {
		Name     string
		MacroDef string
		UseCode  string
		Expected values.Value
	}{
		{
			Name: "identity ER macro via cadr",
			MacroDef: `(define-syntax my-id
				(er-macro-transformer
					(lambda (form rename compare)
						(cadr form))))`,
			UseCode:  `(my-id 42)`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "ER macro constructs list expression",
			MacroDef: `(define-syntax make-pair
				(er-macro-transformer
					(lambda (form rename compare)
						(list (rename 'list) (cadr form) (caddr form)))))`,
			UseCode:  `(make-pair 10 20)`,
			Expected: values.List(values.NewInteger(10), values.NewInteger(20)),
		},
		{
			Name: "ER macro with rename for hygiene",
			MacroDef: `(define-syntax add-one
				(er-macro-transformer
					(lambda (form rename compare)
						(list (rename '+) (cadr form) 1))))`,
			UseCode:  `(add-one 41)`,
			Expected: values.NewInteger(42),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			env := testhelpers.NewFullRuntimeEnv(t)

			// Step 1: Define the ER macro
			sv := testhelpers.ParseSchemeExpr(t, env, tc.MacroDef)
			cont, err := testhelpers.NewTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)

			// Step 2: Use the macro
			sv = testhelpers.ParseSchemeExpr(t, env, tc.UseCode)
			cont, err = testhelpers.NewTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc = machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, tc.Expected)
		})
	}
}
