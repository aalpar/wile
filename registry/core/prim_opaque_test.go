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
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

func TestPrimOpaqueQ(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "number is not opaque", Code: `(opaque? 42)`, Expected: values.FalseValue},
		{Name: "string is not opaque", Code: `(opaque? "hello")`, Expected: values.FalseValue},
		{Name: "boolean is not opaque", Code: `(opaque? #t)`, Expected: values.FalseValue},
		{Name: "list is not opaque", Code: `(opaque? '(1 2 3))`, Expected: values.FalseValue},
		{Name: "vector is not opaque", Code: `(opaque? #(1 2))`, Expected: values.FalseValue},
		{Name: "symbol is not opaque", Code: `(opaque? 'foo)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestPrimOpaqueTag_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "number", Code: `(opaque-tag 42)`},
		{Name: "string", Code: `(opaque-tag "hello")`},
		{Name: "boolean", Code: `(opaque-tag #t)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// envWithOpaque creates a fresh test environment with an opaque value bound to the given name.
func envWithOpaque(t *testing.T, name string, opaque values.Value) *environment.EnvironmentFrame {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrameTiny(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	sym := values.NewSymbol(name)
	gi, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
	err = env.SetOwnGlobalValue(gi, opaque)
	qt.Assert(t, err, qt.IsNil)

	return env
}

func TestPrimOpaqueQ_WithOpaqueValue(t *testing.T) {
	env := envWithOpaque(t, "my-opaque", values.NewOpaqueValue("test-tag", "inner"))

	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "opaque value is opaque", Code: `(opaque? my-opaque)`, Expected: values.TrueValue},
		{Name: "opaque in conditional", Code: `(if (opaque? my-opaque) "yes" "no")`, Expected: values.NewString("yes")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithEnv(t, env, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestPrimOpaqueTag_WithOpaqueValue(t *testing.T) {
	env := envWithOpaque(t, "my-opaque", values.NewOpaqueValue("test-tag", "inner"))

	result, err := testhelpers.RunSchemeCodeWithEnv(t, env, `(opaque-tag my-opaque)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("test-tag"))
}
