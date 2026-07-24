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

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

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

func TestBinding_UpdateMeta_Scopes(t *testing.T) {
	b := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b.Scopes(), qt.IsNil)

	scope1 := syntax.NewScope()
	scope2 := syntax.NewScope()
	scopes := []*syntax.Scope{scope1, scope2}

	b.UpdateMeta(func(m *BindingMeta) bool {
		m.Scopes = scopes
		return true
	})
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

	// Replacing the original's scopes via UpdateMeta does not touch the copy.
	b1.UpdateMeta(func(m *BindingMeta) bool {
		m.Scopes = []*syntax.Scope{syntax.NewScope()}
		return true
	})
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

func TestBinding_UpdateMeta_Source(t *testing.T) {
	b := NewBinding(values.Void, BindingTypeVariable)
	qt.Assert(t, b.Source(), qt.IsNil)

	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(5, 3, 50),
	}

	b.UpdateMeta(func(m *BindingMeta) bool {
		m.Source = source
		return true
	})
	qt.Assert(t, b.Source(), qt.Equals, source)
	qt.Assert(t, b.Source().File, qt.Equals, "test.scm")
}

func TestBinding_UpdateMeta_Doc(t *testing.T) {
	b := NewBinding(values.Void, BindingTypePrimitive)
	qt.Assert(t, b.Doc(), qt.Equals, "")
	b.UpdateMeta(func(m *BindingMeta) bool {
		m.Doc = "Conditional expression."
		return true
	})
	qt.Assert(t, b.Doc(), qt.Equals, "Conditional expression.")
}

func TestBinding_UpdateMeta_PreservesExistingMeta(t *testing.T) {
	scope := syntax.NewScope()
	b := NewBindingWithScopes(values.NewInteger(1), BindingTypeVariable, []*syntax.Scope{scope})
	b.UpdateMeta(func(m *BindingMeta) bool {
		m.Doc = "A documented binding."
		return true
	})
	qt.Assert(t, b.Doc(), qt.Equals, "A documented binding.")
	qt.Assert(t, b.Scopes(), qt.HasLen, 1)
}

func TestBinding_Copy_WithDoc(t *testing.T) {
	b1 := NewBinding(values.Void, BindingTypePrimitive)
	b1.UpdateMeta(func(m *BindingMeta) bool {
		m.Doc = "Original doc."
		return true
	})
	b2 := b1.Copy()
	qt.Assert(t, b2.Doc(), qt.Equals, "Original doc.")
	b2.UpdateMeta(func(m *BindingMeta) bool {
		m.Doc = "Changed doc."
		return true
	})
	qt.Assert(t, b1.Doc(), qt.Equals, "Original doc.")
	qt.Assert(t, b2.Doc(), qt.Equals, "Changed doc.")
}

func TestBinding_UpdateMeta_Imported(t *testing.T) {
	b := NewBinding(values.NewInteger(1), BindingTypeVariable)
	qt.Assert(t, b.IsImported(), qt.IsFalse)
	b.UpdateMeta(func(m *BindingMeta) bool {
		m.Imported = true
		return true
	})
	qt.Assert(t, b.IsImported(), qt.IsTrue)
}

func TestBinding_UpdateMeta_Stable(t *testing.T) {
	b := NewBinding(values.NewInteger(42), BindingTypeVariable)
	qt.Assert(t, b.IsStable(), qt.IsFalse)
	b.UpdateMeta(func(m *BindingMeta) bool {
		m.Stable = true
		return true
	})
	qt.Assert(t, b.IsStable(), qt.IsTrue)
}

func TestBinding_Copy_PreservesImportedAndStable(t *testing.T) {
	b := NewBinding(values.NewInteger(1), BindingTypeVariable)
	b.UpdateMeta(func(m *BindingMeta) bool {
		m.Imported = true
		m.Stable = true
		return true
	})
	cp := b.Copy()
	qt.Assert(t, cp.IsImported(), qt.IsTrue)
	qt.Assert(t, cp.IsStable(), qt.IsTrue)
	cp.UpdateMeta(func(m *BindingMeta) bool {
		m.Imported = false
		return true
	})
	qt.Assert(t, b.IsImported(), qt.IsTrue)
}

// TestBinding_StableEvidenceVsConclusion pins that Imported is standing
// evidence for the Stable conclusion, the Stable flag carries an independently
// proven conclusion, and neither is a set!-permission (that is IsImported).
func TestBinding_StableEvidenceVsConclusion(t *testing.T) {
	// Imported is evidence: IsStable holds, IsImported holds.
	b := NewBinding(values.NewInteger(1), BindingTypeVariable)
	b.UpdateMeta(func(m *BindingMeta) bool {
		m.Imported = true
		return true
	})
	qt.Assert(t, b.IsStable(), qt.IsTrue)
	qt.Assert(t, b.IsImported(), qt.IsTrue)

	// A proven Stable conclusion without import: stable but not imported, so
	// set! is still permitted (IsImported false).
	b2 := NewBinding(values.NewInteger(2), BindingTypeVariable)
	b2.UpdateMeta(func(m *BindingMeta) bool {
		m.Stable = true
		return true
	})
	qt.Assert(t, b2.IsStable(), qt.IsTrue)
	qt.Assert(t, b2.IsImported(), qt.IsFalse)

	// Neither: not stable, not imported.
	b3 := NewBinding(values.NewInteger(3), BindingTypeVariable)
	qt.Assert(t, b3.IsStable(), qt.IsFalse)
	qt.Assert(t, b3.IsImported(), qt.IsFalse)
}

// TestBinding_InlineHOFParam pins the inline-HOF capability (callback
// specialization Strategy A): a binding reports -1 ("not an inline HOF") by
// default — including a binding that has a BindingMeta but no InlineHOF stamp
// (the UpdateMeta &BindingMeta{} zero-value case, which a plain -1-sentinel int
// would mis-read as "callback param 0" and falsely mark every primitive an
// inline HOF). Once stamped, it reports the callback parameter index.
func TestBinding_InlineHOFParam(t *testing.T) {
	// No metadata at all -> -1.
	b := NewBinding(values.NewInteger(1), BindingTypeVariable)
	qt.Assert(t, b.InlineHOFParam(), qt.Equals, -1)

	// Metadata exists but no InlineHOF stamp (zero value) -> still -1. This is the
	// invariant that keeps UpdateMeta's &BindingMeta{} correct: Lever E stamps
	// every primitive via UpdateMeta setting CaptureSafe, and none of those are HOFs.
	b.UpdateMeta(func(m *BindingMeta) bool {
		m.CaptureSafe = true
		return true
	})
	qt.Assert(t, b.InlineHOFParam(), qt.Equals, -1)

	// Stamped -> the callback parameter index.
	b.UpdateMeta(func(m *BindingMeta) bool {
		m.InlineHOF = true
		m.InlineHOFCallbackParam = 0
		return true
	})
	qt.Assert(t, b.InlineHOFParam(), qt.Equals, 0)
}

// TestBinding_Copy_PreservesInlineHOF pins that Copy carries the inline-HOF
// capability onto the copy (the compile/expansion clone must not drop it). Uses a
// non-zero param index so the test proves the index itself is copied, not just
// the gating flag.
func TestBinding_Copy_PreservesInlineHOF(t *testing.T) {
	b := NewBinding(values.NewInteger(1), BindingTypeVariable)
	b.UpdateMeta(func(m *BindingMeta) bool {
		m.InlineHOF = true
		m.InlineHOFCallbackParam = 2
		return true
	})
	cp := b.Copy()
	qt.Assert(t, cp.InlineHOFParam(), qt.Equals, 2)
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

func bindingWithOrigin(lib, name string) *Binding {
	b := NewBinding(values.Void, BindingTypeVariable)
	b.UpdateMeta(func(m *BindingMeta) bool {
		m.Origin = &OriginRef{RootLib: lib, RootName: name}
		return true
	})
	return b
}

func TestSameBinding(t *testing.T) {
	same := bindingWithOrigin("(aa)", "foo")

	tcs := []struct {
		name string
		a, b *Binding
		want bool
	}{
		{"identical object", same, same, true},
		// Both-nil is unreachable from the two in-tree callers (each guards nil
		// first); pinned so the a==b short-circuit's result stays deliberate.
		{"both nil", nil, nil, true},
		{"one nil, one bound", NewBinding(values.Void, BindingTypeVariable), nil, false},
		{"both plain, distinct objects", NewBinding(values.Void, BindingTypeVariable), NewBinding(values.Void, BindingTypeVariable), false},
		{"equal origins, distinct objects", bindingWithOrigin("(aa)", "foo"), bindingWithOrigin("(aa)", "foo"), true},
		{"same lib, different defining name", bindingWithOrigin("(aa)", "foo"), bindingWithOrigin("(aa)", "bar"), false},
		{"different lib, same defining name", bindingWithOrigin("(aa)", "foo"), bindingWithOrigin("(bb)", "foo"), false},
		{"one has origin, one plain", bindingWithOrigin("(aa)", "foo"), NewBinding(values.Void, BindingTypeVariable), false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, SameBinding(tc.a, tc.b), qt.Equals, tc.want)
		})
	}
}

// TestBinding_Copy_PreservesOrigin pins that Copy carries the import-provenance
// root (the plan's "stable across CloneForMatch" requirement): the copy is a
// distinct object yet SameBinding still reports it identical to the original.
func TestBinding_Copy_PreservesOrigin(t *testing.T) {
	b := bindingWithOrigin("(aa)", "foo")

	cp := b.Copy()

	qt.Assert(t, cp == b, qt.IsFalse)
	qt.Assert(t, SameBinding(b, cp), qt.IsTrue)
	qt.Assert(t, *cp.Origin(), qt.Equals, OriginRef{RootLib: "(aa)", RootName: "foo"})
}
