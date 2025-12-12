// Copyright 2025 Aaron Alpar
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

package primitives_test

import (
	"testing"

	"wile/runtime"
	"wile/syntax"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestBoundIdentifierEqualQ(t *testing.T) {
	sctx := syntax.NewZeroValueSourceContext()
	scope1 := syntax.NewScope(nil)
	scope2 := syntax.NewScope(nil)

	// Create syntax symbols with various scope configurations
	idFoo := syntax.NewSyntaxSymbol("foo", sctx)
	idFoo2 := syntax.NewSyntaxSymbol("foo", sctx)
	idBar := syntax.NewSyntaxSymbol("bar", sctx)
	idFooWithScope1 := idFoo.AddScope(scope1).(*syntax.SyntaxSymbol)
	idFooWithScope2 := idFoo.AddScope(scope2).(*syntax.SyntaxSymbol)
	idFooWithScope1Again := idFoo.AddScope(scope1).(*syntax.SyntaxSymbol)

	tcs := []struct {
		name string
		id1  *syntax.SyntaxSymbol
		id2  *syntax.SyntaxSymbol
		want bool
	}{
		{
			name: "same identifier object",
			id1:  idFoo,
			id2:  idFoo,
			want: true,
		},
		{
			name: "same name no scopes",
			id1:  idFoo,
			id2:  idFoo2,
			want: true,
		},
		{
			name: "different names",
			id1:  idFoo,
			id2:  idBar,
			want: false,
		},
		{
			name: "same name same scope",
			id1:  idFooWithScope1,
			id2:  idFooWithScope1Again,
			want: true,
		},
		{
			name: "same name different scopes",
			id1:  idFooWithScope1,
			id2:  idFooWithScope2,
			want: false,
		},
		{
			name: "same name one has scope one doesn't",
			id1:  idFoo,
			id2:  idFooWithScope1,
			want: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Test bound-identifier=? by checking scope set equality directly
			// (This tests the underlying logic used by primBoundIdentifierEqualQ)

			// Same name?
			if tc.id1.Key != tc.id2.Key {
				qt.Assert(t, tc.want, qt.IsFalse)
				return
			}

			// Same scopes? (set equality = mutual subset)
			scopes1 := tc.id1.Scopes()
			scopes2 := tc.id2.Scopes()
			result := syntax.ScopesMatch(scopes1, scopes2) && syntax.ScopesMatch(scopes2, scopes1)
			qt.Assert(t, result, qt.Equals, tc.want)
		})
	}
}

func TestFreeIdentifierEqualQ(t *testing.T) {
	sctx := syntax.NewZeroValueSourceContext()

	// Create syntax symbols
	idFoo := syntax.NewSyntaxSymbol("foo", sctx)
	idFoo2 := syntax.NewSyntaxSymbol("foo", sctx)
	idBar := syntax.NewSyntaxSymbol("bar", sctx)
	idPlus := syntax.NewSyntaxSymbol("+", sctx)
	idPlus2 := syntax.NewSyntaxSymbol("+", sctx)

	tcs := []struct {
		name string
		id1  *syntax.SyntaxSymbol
		id2  *syntax.SyntaxSymbol
		want bool
	}{
		{
			name: "both unbound same name",
			id1:  idFoo,
			id2:  idFoo2,
			want: true,
		},
		{
			name: "both unbound different names",
			id1:  idFoo,
			id2:  idBar,
			want: false,
		},
		{
			name: "both bound to same primitive",
			id1:  idPlus,
			id2:  idPlus2,
			want: true,
		},
		{
			name: "one bound one unbound",
			id1:  idPlus,
			id2:  idFoo,
			want: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env, err := runtime.NewTopLevelEnvironmentFrameTiny()
			qt.Assert(t, err, qt.IsNil)

			// Test free-identifier=? by checking binding resolution
			// (This tests the underlying logic used by primFreeIdentifierEqualQ)
			sym1 := env.InternSymbol(values.NewSymbol(tc.id1.Key))
			sym2 := env.InternSymbol(values.NewSymbol(tc.id2.Key))

			binding1 := env.GetBindingWithScopes(sym1, tc.id1.Scopes())
			binding2 := env.GetBindingWithScopes(sym2, tc.id2.Scopes())

			var result bool
			if binding1 == nil && binding2 == nil {
				// Both unbound → compare names
				result = tc.id1.Key == tc.id2.Key
			} else if binding1 == nil || binding2 == nil {
				// One bound, one unbound → not equal
				result = false
			} else {
				// Both bound → same binding?
				result = binding1 == binding2
			}

			qt.Assert(t, result, qt.Equals, tc.want)
		})
	}
}

func TestBoundIdentifierEqualQPrimitiveExists(t *testing.T) {
	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// Verify the bound-identifier=? primitive exists
	boundIdSym := env.InternSymbol(values.NewSymbol("bound-identifier=?"))
	bnd := env.GetBinding(boundIdSym)
	qt.Assert(t, bnd, qt.IsNotNil)
}

func TestFreeIdentifierEqualQPrimitiveExists(t *testing.T) {
	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// Verify the free-identifier=? primitive exists
	freeIdSym := env.InternSymbol(values.NewSymbol("free-identifier=?"))
	bnd := env.GetBinding(freeIdSym)
	qt.Assert(t, bnd, qt.IsNotNil)
}
