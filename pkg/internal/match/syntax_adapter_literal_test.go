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

package match

import (
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// newLiteralTestBinding builds a standalone binding of the given kind, whose
// value is a plain symbol and so denotes no form (DenotedForm returns ""). A
// standalone binding has no cell, so UpdateMeta writes in place.
func newLiteralTestBinding(name string, bt environment.BindingType, imported bool) *environment.Binding {
	q := environment.NewBinding(values.NewSymbol(name), bt)
	if imported {
		q.UpdateMeta(func(m *environment.BindingMeta) bool {
			m.Imported = true
			return true
		})
	}
	return q
}

// newDenotingLiteralTestBinding builds a standalone binding whose value is a
// FormKeyword, so DenotedForm(q) == form — the shape a real keyword binding
// (else, =>, ...) has, as opposed to newLiteralTestBinding's plain symbol.
func newDenotingLiteralTestBinding(form string, bt environment.BindingType) *environment.Binding {
	return environment.NewBinding(environment.NewFormKeyword(form), bt)
}

// TestSameLiteralBinding pins the identity rule the definition-site pin is
// compared with, as a truth table.
//
// The widening arm — two DISTINCT bindings that are both BindingTypePrimitive
// AND denote the same form count as one literal — used to be reached by no
// end-to-end test in this repo (match.go's literal arm required identical
// spelling before any binding check ran). A renamed or prefixed import of an
// auxiliary keyword now reaches it, so the DenotedForm narrowing this table
// pins is load-bearing, not defensive.
func TestSameLiteralBinding(t *testing.T) {
	shared := newLiteralTestBinding("else", environment.BindingTypePrimitive, false)
	otherPrimitive := newLiteralTestBinding("else", environment.BindingTypePrimitive, false)
	variable := newLiteralTestBinding("else", environment.BindingTypeVariable, false)
	otherVariable := newLiteralTestBinding("else", environment.BindingTypeVariable, false)
	elseFormA := newDenotingLiteralTestBinding("else", environment.BindingTypePrimitive)
	elseFormB := newDenotingLiteralTestBinding("else", environment.BindingTypePrimitive)
	arrowForm := newDenotingLiteralTestBinding("=>", environment.BindingTypePrimitive)

	cases := []struct {
		name string
		a    *environment.Binding
		b    *environment.Binding
		want bool
	}{
		{name: "same pointer", a: shared, b: shared, want: true},
		{name: "both unbound", a: nil, b: nil, want: true},
		{name: "definition side unbound", a: nil, b: variable, want: false},
		{name: "use side unbound", a: shared, b: nil, want: false},
		{
			// THE WIDENING. Each library environment mints its own *Binding per
			// special form, so one ambient name can have two objects; they count
			// as one literal only when both denote the SAME form.
			name: "two distinct primitives denoting the same form are one literal",
			a:    elseFormA,
			b:    elseFormB,
			want: true,
		},
		{
			// "Both primitive" alone is not enough: else must not match =>.
			name: "two distinct primitives denoting different forms are not one literal",
			a:    elseFormA,
			b:    arrowForm,
			want: false,
		},
		{
			// shared/otherPrimitive hold a plain symbol value, not a FormKeyword,
			// so DenotedForm is "" for both — an empty denotation is not an
			// identity and never matches, even against another primitive that
			// does denote a form.
			name: "a primitive with no denotation never matches another primitive",
			a:    otherPrimitive,
			b:    elseFormA,
			want: false,
		},
		{name: "primitive versus variable", a: shared, b: variable, want: false},
		{name: "two distinct variables", a: variable, b: otherVariable, want: false},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := sameLiteralBinding(tc.a, tc.b)
			if got != tc.want {
				t.Errorf("sameLiteralBinding = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestLiteralNotShadowed pins the IsImported rider on top of the identity rule.
// The rider accepts ANY imported binding of the name, from any library, which is
// a deliberate over-acceptance; the cross-library rows of
// TestCrossLibraryPatternLiteralNeedsTheDefinitionSiteBinding (pkg/wile) pin what
// that costs at the program level.
func TestLiteralNotShadowed(t *testing.T) {
	pinned := newLiteralTestBinding("lit", environment.BindingTypeVariable, false)
	localShadow := newLiteralTestBinding("lit", environment.BindingTypeVariable, false)
	importedShadow := newLiteralTestBinding("lit", environment.BindingTypeVariable, true)

	cases := []struct {
		name string
		defB *environment.Binding
		useB *environment.Binding
		want bool
	}{
		{name: "same binding", defB: pinned, useB: pinned, want: true},
		{name: "unrelated local rebinding is a shadow", defB: pinned, useB: localShadow, want: false},
		{
			// An import mints a fresh *Binding for a re-exported name, so a
			// legitimately imported literal can never be pointer-equal.
			name: "any imported binding is accepted",
			defB: pinned,
			useB: importedShadow,
			want: true,
		},
		{name: "unbound at the use site", defB: pinned, useB: nil, want: false},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := literalNotShadowed(tc.defB, tc.useB)
			if got != tc.want {
				t.Errorf("literalNotShadowed = %v, want %v", got, tc.want)
			}
		})
	}
}
