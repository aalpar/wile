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

// TestCompileDefineSyntax tests define-syntax compilation and round-trip
// macro usage through the full pipeline.
//
// Source: compile_define_syntax.go (CompileDefineSyntax).
//
// Uses the two-step pattern (define macro, then use it) because define-syntax
// is a top-level form that modifies the expand environment, and RunSchemeCode
// reads only one form.
func TestCompileDefineSyntax(t *testing.T) {
	tcs := []struct {
		Name     string
		MacroDef string
		UseCode  string
		Expected values.Value
	}{
		{
			Name:     "basic identity macro",
			MacroDef: `(define-syntax my-const (syntax-rules () ((_ v) v)))`,
			UseCode:  `(my-const 42)`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "swap pattern variables",
			MacroDef: `(define-syntax swap (syntax-rules () ((_ a b) (list b a))))`,
			UseCode:  `(swap 1 2)`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(1)),
		},
		{
			Name:     "macro with arithmetic in template",
			MacroDef: `(define-syntax add1 (syntax-rules () ((_ x) (+ x 1))))`,
			UseCode:  `(add1 10)`,
			Expected: values.NewInteger(11),
		},
		{
			Name:     "multi-clause macro",
			MacroDef: `(define-syntax m (syntax-rules () ((m) 0) ((m x) x) ((m x y) (+ x y))))`,
			UseCode:  `(m 10 20)`,
			Expected: values.NewInteger(30),
		},
		{
			Name:     "macro with ellipsis",
			MacroDef: `(define-syntax my-list (syntax-rules () ((_ x ...) (list x ...))))`,
			UseCode:  `(my-list 1 2 3)`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			env := testhelpers.NewFullRuntimeEnv(t)

			// Step 1: Define the macro
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
