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

package environment_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestDenotedForm(t *testing.T) {
	ns := environment.NewNamespace()
	env := ns.NewChildRuntime()
	bind := func(name string, bt environment.BindingType, v values.Value) *environment.Binding {
		_, err := env.DefineOwnGlobal(values.NewSymbol(name), bt, nil, v)
		qt.Assert(t, err, qt.IsNil)
		return env.GetBinding(values.NewSymbol(name), values.ScopesOf(nil))
	}
	tcs := []struct {
		name string
		b    *environment.Binding
		want string
	}{
		{"nil binding denotes nothing", nil, ""},
		{"keyword binding names its form",
			bind("my-if", environment.BindingTypePrimitive, environment.NewFormKeyword("if")), "if"},
		{"a variable denotes nothing even holding a keyword value",
			bind("v", environment.BindingTypeVariable, environment.NewFormKeyword("if")), ""},
		{"a keyword binding without a form-naming value denotes nothing",
			bind("=>", environment.BindingTypePrimitive, values.Void), ""},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, environment.DenotedForm(tc.b), qt.Equals, tc.want)
		})
	}
}
