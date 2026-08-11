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

// newLiteralTestBinding builds a standalone binding of the given kind. A
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

// TestSameLiteralBinding pins the identity rule the definition-site pin is
// compared with, as a truth table.
//
// It exists because the rule's widening arm — two DISTINCT bindings that are both
// BindingTypePrimitive count as one literal — is reached by no end-to-end test in
// this repo. Instrumenting the arm and running the Go suites plus `make cover-scm`
// prints it zero times, because a library-resolved ambient name arrives IMPORTED
// and literalNotShadowed's rider takes it first. The arm is nonetheless an
// ACCEPTING rule on the path W1-6 exists to tighten, so it is pinned here rather
// than left to a suite that does not observe it.
func TestSameLiteralBinding(t *testing.T) {
	shared := newLiteralTestBinding("else", environment.BindingTypePrimitive, false)
	otherPrimitive := newLiteralTestBinding("else", environment.BindingTypePrimitive, false)
	variable := newLiteralTestBinding("else", environment.BindingTypeVariable, false)
	otherVariable := newLiteralTestBinding("else", environment.BindingTypeVariable, false)

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
			// special form, so one ambient name can have two objects.
			name: "two distinct primitives are one literal",
			a:    shared,
			b:    otherPrimitive,
			want: true,
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
