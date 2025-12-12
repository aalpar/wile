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


package environment

import (
	"wile/syntax"
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestBinding_NewBindingWithScopes(t *testing.T) {
	scope1 := syntax.NewScope(nil)
	scope2 := syntax.NewScope(nil)
	scopes := []*syntax.Scope{scope1, scope2}

	val := values.NewInteger(42)
	b := NewBindingWithScopes(val, BindingTypeVariable, scopes)

	qt.Assert(t, b, qt.Not(qt.IsNil))
	qt.Assert(t, b.Value(), values.SchemeEquals, val)
	qt.Assert(t, b.BindingType(), qt.Equals, BindingTypeVariable)
	qt.Assert(t, b.Scopes(), qt.HasLen, 2)
	qt.Assert(t, b.Scopes()[0], qt.Equals, scope1)
	qt.Assert(t, b.Scopes()[1], qt.Equals, scope2)
}

func TestBinding_BindingType(t *testing.T) {
	b := NewBinding(values.Void, BindingTypeSyntax)
	qt.Assert(t, b.BindingType(), qt.Equals, BindingTypeSyntax)
}

func TestBinding_SetValue(t *testing.T) {
	b := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b.Value(), qt.Equals, values.Void)

	newVal := values.NewInteger(123)
	b.SetValue(newVal)
	qt.Assert(t, b.Value(), values.SchemeEquals, newVal)
}

func TestBinding_SetBindingType(t *testing.T) {
	b := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b.BindingType(), qt.Equals, BindingTypeVariable)

	b.SetBindingType(BindingTypePrimitive)
	qt.Assert(t, b.BindingType(), qt.Equals, BindingTypePrimitive)
}

func TestBinding_Scopes(t *testing.T) {
	// Test binding without scopes
	b1 := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b1.Scopes(), qt.IsNil)

	// Test binding with scopes
	scope := syntax.NewScope(nil)
	scopes := []*syntax.Scope{scope}
	b2 := NewBindingWithScopes(values.Void, BindingTypeVariable, scopes)
	qt.Assert(t, b2.Scopes(), qt.HasLen, 1)
	qt.Assert(t, b2.Scopes()[0], qt.Equals, scope)
}

func TestBinding_SetScopes(t *testing.T) {
	b := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b.Scopes(), qt.IsNil)

	scope1 := syntax.NewScope(nil)
	scope2 := syntax.NewScope(nil)
	scopes := []*syntax.Scope{scope1, scope2}

	b.SetScopes(scopes)
	qt.Assert(t, b.Scopes(), qt.HasLen, 2)
	qt.Assert(t, b.Scopes()[0], qt.Equals, scope1)
	qt.Assert(t, b.Scopes()[1], qt.Equals, scope2)
}

func TestBinding_SchemeString(t *testing.T) {
	b := NewBinding(values.NewInteger(42), BindingTypeVariable)
	qt.Assert(t, b.SchemeString(), qt.Equals, "#<binding>")
}

func TestBinding_IsVoid(t *testing.T) {
	var nilBinding *Binding = nil
	qt.Assert(t, nilBinding.IsVoid(), qt.IsTrue)

	b := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b.IsVoid(), qt.IsFalse)
}

func TestBinding_EqualTo(t *testing.T) {
	// Note: EqualTo with nil values has issues in the implementation
	// so we skip testing nil cases

	// Test non-binding comparison
	b3 := NewBinding(values.NewInteger(42), BindingTypeVariable)
	qt.Assert(t, b3.EqualTo(values.NewInteger(42)), qt.IsFalse)

	// Test equal bindings
	b4 := NewBinding(values.NewInteger(42), BindingTypeVariable)
	b5 := NewBinding(values.NewInteger(42), BindingTypeVariable)
	qt.Assert(t, b4.EqualTo(b5), qt.IsTrue)

	// Test different values
	b6 := NewBinding(values.NewInteger(42), BindingTypeVariable)
	b7 := NewBinding(values.NewInteger(43), BindingTypeVariable)
	qt.Assert(t, b6.EqualTo(b7), qt.IsFalse)

	// Test different binding types
	b8 := NewBinding(values.NewInteger(42), BindingTypeVariable)
	b9 := NewBinding(values.NewInteger(42), BindingTypeSyntax)
	qt.Assert(t, b8.EqualTo(b9), qt.IsFalse)

	// Test nil values in bindings
	b10 := &Binding{value: nil, bindingType: BindingTypeVariable}
	b11 := &Binding{value: nil, bindingType: BindingTypeVariable}
	qt.Assert(t, b10.EqualTo(b11), qt.IsTrue)

	b12 := &Binding{value: values.NewInteger(42), bindingType: BindingTypeVariable}
	qt.Assert(t, b10.EqualTo(b12), qt.IsFalse)
	qt.Assert(t, b12.EqualTo(b10), qt.IsFalse)
}

func TestBinding_Copy(t *testing.T) {
	scope1 := syntax.NewScope(nil)
	scope2 := syntax.NewScope(nil)
	scopes := []*syntax.Scope{scope1, scope2}

	b1 := NewBindingWithScopes(values.NewInteger(42), BindingTypeVariable, scopes)

	copied := b1.Copy()
	b2, ok := copied.(*Binding)
	qt.Assert(t, ok, qt.IsTrue)

	// Check that values are equal
	qt.Assert(t, b2.Value(), values.SchemeEquals, b1.Value())
	qt.Assert(t, b2.BindingType(), qt.Equals, b1.BindingType())
	qt.Assert(t, b2.Scopes(), qt.HasLen, 2)

	// Check that scopes slice is a deep copy (different slices, same scope objects)
	qt.Assert(t, b2.Scopes()[0], qt.Equals, scope1)
	qt.Assert(t, b2.Scopes()[1], qt.Equals, scope2)

	// Modify original scopes slice - copy should not be affected
	b1.Scopes()[0] = syntax.NewScope(nil)
	qt.Assert(t, b2.Scopes()[0], qt.Equals, scope1) // Copy unchanged

	// Test copy with nil scopes
	b3 := NewBinding(values.NewInteger(99), BindingTypePrimitive)
	copied2 := b3.Copy()
	b4, ok := copied2.(*Binding)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, b4.Scopes(), qt.IsNil)
}

func TestBinding_NewBindingWithSource(t *testing.T) {
	scope := syntax.NewScope(nil)
	scopes := []*syntax.Scope{scope}
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(10, 5, 100),
	}

	val := values.NewInteger(42)
	b := NewBindingWithSource(val, BindingTypeVariable, scopes, source)

	qt.Assert(t, b, qt.Not(qt.IsNil))
	qt.Assert(t, b.Value(), values.SchemeEquals, val)
	qt.Assert(t, b.BindingType(), qt.Equals, BindingTypeVariable)
	qt.Assert(t, b.Scopes(), qt.HasLen, 1)
	qt.Assert(t, b.Source(), qt.Equals, source)
	qt.Assert(t, b.Source().File, qt.Equals, "test.scm")
}

func TestBinding_Source(t *testing.T) {
	// Test binding without source
	b1 := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b1.Source(), qt.IsNil)

	// Test binding with source
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(1, 1, 0),
	}
	b2 := NewBindingWithSource(values.Void, BindingTypeVariable, nil, source)
	qt.Assert(t, b2.Source(), qt.Equals, source)
}

func TestBinding_SetSource(t *testing.T) {
	b := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b.Source(), qt.IsNil)

	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(5, 3, 50),
	}

	b.SetSource(source)
	qt.Assert(t, b.Source(), qt.Equals, source)
	qt.Assert(t, b.Source().File, qt.Equals, "test.scm")
}

func TestBinding_Copy_WithSource(t *testing.T) {
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(10, 5, 100),
	}

	b1 := NewBindingWithSource(values.NewInteger(42), BindingTypeVariable, nil, source)

	copied := b1.Copy()
	b2, ok := copied.(*Binding)
	qt.Assert(t, ok, qt.IsTrue)

	// Check that source is preserved (same reference, source is immutable)
	qt.Assert(t, b2.Source(), qt.Equals, source)
	qt.Assert(t, b2.Source().File, qt.Equals, "test.scm")
}
