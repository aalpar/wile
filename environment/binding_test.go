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

package environment

import (
	"testing"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestBinding_NewBindingWithScopes(t *testing.T) {
	scope1 := syntax.NewScope()
	scope2 := syntax.NewScope()
	scopes := []*syntax.Scope{scope1, scope2}

	val := values.NewInteger(42)
	b := NewBindingWithScopes(val, BindingTypeVariable, scopes)

	qt.Assert(t, b, qt.Not(qt.IsNil))
	qt.Assert(t, b.Value(), valuestest.SchemeEquals, val)
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
	qt.Assert(t, b.Value(), valuestest.SchemeEquals, values.Void)

	newVal := values.NewInteger(123)
	b.SetValue(newVal)
	qt.Assert(t, b.Value(), valuestest.SchemeEquals, newVal)
}

func TestBinding_Scopes(t *testing.T) {
	// Test binding without scopes
	b1 := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b1.Scopes(), qt.IsNil)

	// Test binding with scopes
	scope := syntax.NewScope()
	scopes := []*syntax.Scope{scope}
	b2 := NewBindingWithScopes(values.Void, BindingTypeVariable, scopes)
	qt.Assert(t, b2.Scopes(), qt.HasLen, 1)
	qt.Assert(t, b2.Scopes()[0], qt.Equals, scope)
}

func TestBinding_EnsureMeta_Scopes(t *testing.T) {
	b := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b.Scopes(), qt.IsNil)

	scope1 := syntax.NewScope()
	scope2 := syntax.NewScope()
	scopes := []*syntax.Scope{scope1, scope2}

	b.EnsureMeta().Scopes = scopes
	qt.Assert(t, b.Scopes(), qt.HasLen, 2)
	qt.Assert(t, b.Scopes()[0], qt.Equals, scope1)
	qt.Assert(t, b.Scopes()[1], qt.Equals, scope2)
}

func TestBinding_Copy(t *testing.T) {
	scope1 := syntax.NewScope()
	scope2 := syntax.NewScope()
	scopes := []*syntax.Scope{scope1, scope2}

	b1 := NewBindingWithScopes(values.NewInteger(42), BindingTypeVariable, scopes)

	b2 := b1.Copy()

	// Check that values are equal
	qt.Assert(t, b2.Value(), valuestest.SchemeEquals, b1.Value())
	qt.Assert(t, b2.BindingType(), qt.Equals, b1.BindingType())
	qt.Assert(t, b2.Scopes(), qt.HasLen, 2)

	// Scopes are shared by reference (immutable at runtime), same scope objects
	qt.Assert(t, b2.Scopes()[0], qt.Equals, scope1)
	qt.Assert(t, b2.Scopes()[1], qt.Equals, scope2)

	// Replacing the original's scopes via EnsureMeta does not touch the copy.
	b1.EnsureMeta().Scopes = []*syntax.Scope{syntax.NewScope()}
	qt.Assert(t, b2.Scopes()[0], qt.Equals, scope1) // Copy unchanged

	// Test copy with nil scopes
	b3 := NewBinding(values.NewInteger(99), BindingTypePrimitive)
	b4 := b3.Copy()
	qt.Assert(t, b4.Scopes(), qt.IsNil)
}

func TestBinding_NewBindingWithSource(t *testing.T) {
	scope := syntax.NewScope()
	scopes := []*syntax.Scope{scope}
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(10, 5, 100),
	}

	val := values.NewInteger(42)
	b := NewBindingWithSource(val, BindingTypeVariable, scopes, source)

	qt.Assert(t, b, qt.Not(qt.IsNil))
	qt.Assert(t, b.Value(), valuestest.SchemeEquals, val)
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

func TestBinding_EnsureMeta_Source(t *testing.T) {
	b := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b.Source(), qt.IsNil)

	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(5, 3, 50),
	}

	b.EnsureMeta().Source = source
	qt.Assert(t, b.Source(), qt.Equals, source)
	qt.Assert(t, b.Source().File, qt.Equals, "test.scm")
}

func TestBinding_EnsureMeta_Doc(t *testing.T) {
	b := NewBinding(values.Void, BindingTypePrimitive)
	qt.Assert(t, b.Doc(), qt.Equals, "")
	b.EnsureMeta().Doc = "Conditional expression."
	qt.Assert(t, b.Doc(), qt.Equals, "Conditional expression.")
}

func TestBinding_EnsureMeta_PreservesExistingMeta(t *testing.T) {
	scope := syntax.NewScope()
	b := NewBindingWithScopes(values.NewInteger(1), BindingTypeVariable, []*syntax.Scope{scope})
	b.EnsureMeta().Doc = "A documented binding."
	qt.Assert(t, b.Doc(), qt.Equals, "A documented binding.")
	qt.Assert(t, b.Scopes(), qt.HasLen, 1)
}

func TestBinding_Copy_WithDoc(t *testing.T) {
	b1 := NewBinding(values.Void, BindingTypePrimitive)
	b1.EnsureMeta().Doc = "Original doc."
	b2 := b1.Copy()
	qt.Assert(t, b2.Doc(), qt.Equals, "Original doc.")
	b2.EnsureMeta().Doc = "Changed doc."
	qt.Assert(t, b1.Doc(), qt.Equals, "Original doc.")
	qt.Assert(t, b2.Doc(), qt.Equals, "Changed doc.")
}

func TestBinding_EnsureMeta_Imported(t *testing.T) {
	b := NewBinding(values.NewInteger(1), BindingTypeVariable)
	qt.Assert(t, b.IsImported(), qt.IsFalse)
	b.EnsureMeta().Imported = true
	qt.Assert(t, b.IsImported(), qt.IsTrue)
}

func TestBinding_EnsureMeta_Stable(t *testing.T) {
	b := NewBinding(values.NewInteger(42), BindingTypeVariable)
	qt.Assert(t, b.IsStable(), qt.IsFalse)
	b.EnsureMeta().Stable = true
	qt.Assert(t, b.IsStable(), qt.IsTrue)
}

func TestBinding_Copy_PreservesImportedAndStable(t *testing.T) {
	b := NewBinding(values.NewInteger(1), BindingTypeVariable)
	m := b.EnsureMeta()
	m.Imported = true
	m.Stable = true
	cp := b.Copy()
	qt.Assert(t, cp.IsImported(), qt.IsTrue)
	qt.Assert(t, cp.IsStable(), qt.IsTrue)
	cp.EnsureMeta().Imported = false
	qt.Assert(t, b.IsImported(), qt.IsTrue)
}

// TestBinding_StableAndImmutable pins the orthogonal split: Imported ⟹ Stable
// ⟹ Immutable; a bare Stable (non-imported) is also Stable+Immutable; neither
// flag means mutable.
func TestBinding_StableAndImmutable(t *testing.T) {
	// Imported implies stable and immutable.
	b := NewBinding(values.NewInteger(1), BindingTypeVariable)
	b.EnsureMeta().Imported = true
	qt.Assert(t, b.IsImmutable(), qt.IsTrue)
	qt.Assert(t, b.IsStable(), qt.IsTrue)

	// A bare Stable (not imported) is immutable and stable but not imported.
	b2 := NewBinding(values.NewInteger(2), BindingTypeVariable)
	b2.EnsureMeta().Stable = true
	qt.Assert(t, b2.IsStable(), qt.IsTrue)
	qt.Assert(t, b2.IsImmutable(), qt.IsTrue)
	qt.Assert(t, b2.IsImported(), qt.IsFalse)

	// Neither: mutable, not stable.
	b3 := NewBinding(values.NewInteger(3), BindingTypeVariable)
	qt.Assert(t, b3.IsImmutable(), qt.IsFalse)
	qt.Assert(t, b3.IsStable(), qt.IsFalse)
}

func TestBinding_Copy_WithSource(t *testing.T) {
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(10, 5, 100),
	}

	b1 := NewBindingWithSource(values.NewInteger(42), BindingTypeVariable, nil, source)

	b2 := b1.Copy()

	// Check that source is preserved (same reference, source is immutable)
	qt.Assert(t, b2.Source(), qt.Equals, source)
	qt.Assert(t, b2.Source().File, qt.Equals, "test.scm")
}
