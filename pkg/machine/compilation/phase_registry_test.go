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

package compilation

import (
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestRegisterAndLookupPhaseBindings(t *testing.T) {
	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "round-trip: register then lookup",
			checkFn: func(t *testing.T) {
				topLevel := environment.NewNamespace()
				env := topLevel.Runtime()

				entries := []PhaseEntry[PrimitiveExpanderFunc]{
					{Name: "alpha", Fn: func(_ *ExpanderTimeContinuation, _ *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
						return expr, nil
					}},
					{Name: "beta", Fn: func(_ *ExpanderTimeContinuation, _ *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
						return expr, nil
					}},
				}
				wrapper := func(name string, fn PrimitiveExpanderFunc) values.Value {
					return NewPrimitiveExpander(name, fn)
				}

				err := RegisterPhaseBindings(
					env,
					func() *environment.EnvironmentFrame {
						return topLevel.Expand()
					},
					entries,
					wrapper,
				)
				qt.Assert(t, err, qt.IsNil)

				// Lookup registered bindings
				sym := values.NewSymbol("alpha")
				got := LookupPhaseBinding[*PrimitiveExpander](topLevel.Expand(), sym, nil)
				qt.Assert(t, got, qt.IsNotNil)
				qt.Assert(t, got.Name(), qt.Equals, "alpha")

				sym2 := values.NewSymbol("beta")
				got2 := LookupPhaseBinding[*PrimitiveExpander](topLevel.Expand(), sym2, nil)
				qt.Assert(t, got2, qt.IsNotNil)
				qt.Assert(t, got2.Name(), qt.Equals, "beta")
			},
		},
		{
			name: "lookup missing key returns zero value",
			checkFn: func(t *testing.T) {
				topLevel := environment.NewNamespace()
				expandEnv := topLevel.Expand()

				sym := values.NewSymbol("nonexistent")
				got := LookupPhaseBinding[*PrimitiveExpander](expandEnv, sym, nil)
				qt.Assert(t, got, qt.IsNil)
			},
		},
		{
			name: "lookup wrong type returns zero value",
			checkFn: func(t *testing.T) {
				topLevel := environment.NewNamespace()
				env := topLevel.Runtime()

				entries := []PhaseEntry[PrimitiveExpanderFunc]{
					{Name: "gamma", Fn: func(_ *ExpanderTimeContinuation, _ *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
						return expr, nil
					}},
				}
				wrapper := func(name string, fn PrimitiveExpanderFunc) values.Value {
					return NewPrimitiveExpander(name, fn)
				}

				err := RegisterPhaseBindings(
					env,
					func() *environment.EnvironmentFrame {
						return topLevel.Expand()
					},
					entries,
					wrapper,
				)
				qt.Assert(t, err, qt.IsNil)

				// Lookup with wrong target type — returns zero value
				sym := values.NewSymbol("gamma")
				got := LookupPhaseBinding[*SyntaxCompiler](topLevel.Expand(), sym, nil)
				qt.Assert(t, got, qt.IsNil)
			},
		},
		{
			name: "register empty entries slice does not error",
			checkFn: func(t *testing.T) {
				topLevel := environment.NewNamespace()
				env := topLevel.Runtime()

				var entries []PhaseEntry[PrimitiveExpanderFunc]
				wrapper := func(name string, fn PrimitiveExpanderFunc) values.Value {
					return NewPrimitiveExpander(name, fn)
				}

				err := RegisterPhaseBindings(
					env,
					func() *environment.EnvironmentFrame {
						return topLevel.Expand()
					},
					entries,
					wrapper,
				)
				qt.Assert(t, err, qt.IsNil)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}
