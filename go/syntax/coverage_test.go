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

package syntax

import (
	"context"
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

// Test SourceIndexes methods
func TestSourceIndexes_Getters(t *testing.T) {
	sidx := NewSourceIndexes(10, 5, 2)
	qt.Assert(t, sidx.Index(), qt.Equals, 10)
	qt.Assert(t, sidx.Column(), qt.Equals, 5)
	qt.Assert(t, sidx.Line(), qt.Equals, 2)
}

func TestSourceIndexes_Inc(t *testing.T) {
	sidx := NewSourceIndexes(10, 5, 2)
	result := sidx.Inc(3)
	qt.Assert(t, result, qt.Equals, 13)
	qt.Assert(t, sidx.Index(), qt.Equals, 13)
	qt.Assert(t, sidx.Column(), qt.Equals, 8)
	qt.Assert(t, sidx.Line(), qt.Equals, 2)
}

func TestSourceIndexes_NewLine(t *testing.T) {
	sidx := NewSourceIndexes(10, 5, 2)
	result := sidx.NewLine()
	qt.Assert(t, result, qt.Equals, 11)
	qt.Assert(t, sidx.Index(), qt.Equals, 11)
	qt.Assert(t, sidx.Column(), qt.Equals, 0)
	qt.Assert(t, sidx.Line(), qt.Equals, 3)
}

func TestSourceIndexes_SchemeString(t *testing.T) {
	sidx := NewSourceIndexes(10, 5, 2)
	qt.Assert(t, sidx.SchemeString(), qt.Equals, "<indexes 10:5:2>")
}

func TestSourceIndexes_IsVoid(t *testing.T) {
	sidx := NewSourceIndexes(0, 0, 0)
	qt.Assert(t, sidx.IsVoid(), qt.IsFalse)
}

func TestSourceIndexes_EqualTo(t *testing.T) {
	sidx1 := NewSourceIndexes(10, 5, 2)
	sidx2 := NewSourceIndexes(10, 5, 2)
	sidx3 := NewSourceIndexes(11, 5, 2)
	sidx4 := NewSourceIndexes(10, 6, 2)
	sidx5 := NewSourceIndexes(10, 5, 3)

	qt.Assert(t, sidx1.EqualTo(sidx2), qt.IsTrue)
	qt.Assert(t, sidx1.EqualTo(sidx3), qt.IsFalse)
	qt.Assert(t, sidx1.EqualTo(sidx4), qt.IsFalse)
	qt.Assert(t, sidx1.EqualTo(sidx5), qt.IsFalse)
	qt.Assert(t, sidx1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

// Test SourceContext methods
func TestSourceContext_NewZeroValueSourceContext(t *testing.T) {
	sctx := NewZeroValueSourceContext()
	qt.Assert(t, sctx, qt.IsNotNil)
	qt.Assert(t, sctx.Text, qt.Equals, "")
	qt.Assert(t, sctx.File, qt.Equals, "")
}

func TestSourceContext_SchemeString(t *testing.T) {
	sidx0 := NewSourceIndexes(0, 0, 1)
	sidx1 := NewSourceIndexes(5, 5, 1)
	sctx := NewSourceContext("hello", "test.scm", sidx0, sidx1)
	result := sctx.SchemeString()
	qt.Assert(t, result, qt.Contains, "test.scm")
}

func TestSourceContext_IsVoid(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	qt.Assert(t, sctx.IsVoid(), qt.IsFalse)

	var nilCtx *SourceContext
	qt.Assert(t, nilCtx.IsVoid(), qt.IsTrue)
}

func TestSourceContext_EqualTo(t *testing.T) {
	sidx0 := NewSourceIndexes(0, 0, 1)
	sidx1 := NewSourceIndexes(5, 5, 1)
	sctx1 := NewSourceContext("hello", "test.scm", sidx0, sidx1)
	sctx2 := NewSourceContext("hello", "test.scm", sidx0, sidx1)
	sctx3 := NewSourceContext("world", "test.scm", sidx0, sidx1)
	sctx4 := NewSourceContext("hello", "other.scm", sidx0, sidx1)

	qt.Assert(t, sctx1.EqualTo(sctx2), qt.IsTrue)
	qt.Assert(t, sctx1.EqualTo(sctx1), qt.IsTrue)
	qt.Assert(t, sctx1.EqualTo(sctx3), qt.IsFalse)
	qt.Assert(t, sctx1.EqualTo(sctx4), qt.IsFalse)
	qt.Assert(t, sctx1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestSourceContext_WithScope(t *testing.T) {
	sidx0 := NewSourceIndexes(0, 0, 1)
	sidx1 := NewSourceIndexes(5, 5, 1)
	sctx := NewSourceContext("hello", "test.scm", sidx0, sidx1)

	scope1 := NewScope(nil)
	sctx2 := sctx.WithScope(scope1)

	qt.Assert(t, len(sctx.Scopes), qt.Equals, 0)
	qt.Assert(t, len(sctx2.Scopes), qt.Equals, 1)
	qt.Assert(t, sctx2.Scopes[0], qt.Equals, scope1)
	qt.Assert(t, sctx2.Text, qt.Equals, "hello")
	qt.Assert(t, sctx2.File, qt.Equals, "test.scm")

	scope2 := NewScope(nil)
	sctx3 := sctx2.WithScope(scope2)
	qt.Assert(t, len(sctx3.Scopes), qt.Equals, 2)
	qt.Assert(t, sctx3.Scopes[0], qt.Equals, scope2)
	qt.Assert(t, sctx3.Scopes[1], qt.Equals, scope1)
}

func TestSourceContext_WithScope_Nil(t *testing.T) {
	var sctx *SourceContext
	scope := NewScope(nil)
	sctx2 := sctx.WithScope(scope)

	qt.Assert(t, sctx2, qt.IsNotNil)
	qt.Assert(t, len(sctx2.Scopes), qt.Equals, 1)
	qt.Assert(t, sctx2.Scopes[0], qt.Equals, scope)
}

func TestSourceContext_WithScopes(t *testing.T) {
	sidx0 := NewSourceIndexes(0, 0, 1)
	sidx1 := NewSourceIndexes(5, 5, 1)
	sctx := NewSourceContext("hello", "test.scm", sidx0, sidx1)

	scope1 := NewScope(nil)
	scope2 := NewScope(nil)
	scopes := []*Scope{scope1, scope2}

	sctx2 := sctx.WithScopes(scopes)
	qt.Assert(t, len(sctx2.Scopes), qt.Equals, 2)
	qt.Assert(t, sctx2.Scopes[0], qt.Equals, scope1)
	qt.Assert(t, sctx2.Scopes[1], qt.Equals, scope2)

	scope3 := NewScope(nil)
	sctx3 := sctx2.WithScopes([]*Scope{scope3})
	qt.Assert(t, len(sctx3.Scopes), qt.Equals, 3)
	qt.Assert(t, sctx3.Scopes[0], qt.Equals, scope3)
	qt.Assert(t, sctx3.Scopes[1], qt.Equals, scope1)
	qt.Assert(t, sctx3.Scopes[2], qt.Equals, scope2)
}

func TestSourceContext_WithScopes_Empty(t *testing.T) {
	sidx0 := NewSourceIndexes(0, 0, 1)
	sidx1 := NewSourceIndexes(5, 5, 1)
	sctx := NewSourceContext("hello", "test.scm", sidx0, sidx1)

	sctx2 := sctx.WithScopes([]*Scope{})
	qt.Assert(t, sctx2, qt.Equals, sctx)
}

func TestSourceContext_WithScopes_Nil(t *testing.T) {
	var sctx *SourceContext
	scope := NewScope(nil)
	sctx2 := sctx.WithScopes([]*Scope{scope})

	qt.Assert(t, sctx2, qt.IsNotNil)
	qt.Assert(t, len(sctx2.Scopes), qt.Equals, 1)
	qt.Assert(t, sctx2.Scopes[0], qt.Equals, scope)
}

// Test scope utilities
func TestScopesMatch(t *testing.T) {
	scope1 := NewScope(nil)
	scope2 := NewScope(nil)
	scope3 := NewScope(nil)

	qt.Assert(t, ScopesMatch([]*Scope{}, []*Scope{}), qt.IsTrue)
	qt.Assert(t, ScopesMatch([]*Scope{scope1}, []*Scope{}), qt.IsTrue)
	qt.Assert(t, ScopesMatch([]*Scope{}, []*Scope{scope1}), qt.IsFalse)
	qt.Assert(t, ScopesMatch([]*Scope{scope1, scope2}, []*Scope{scope1}), qt.IsTrue)
	qt.Assert(t, ScopesMatch([]*Scope{scope1}, []*Scope{scope1, scope2}), qt.IsFalse)
	qt.Assert(t, ScopesMatch([]*Scope{scope1, scope2}, []*Scope{scope1, scope2}), qt.IsTrue)
	qt.Assert(t, ScopesMatch([]*Scope{scope2, scope1}, []*Scope{scope1, scope2}), qt.IsTrue)
	qt.Assert(t, ScopesMatch([]*Scope{scope1, scope2, scope3}, []*Scope{scope1, scope3}), qt.IsTrue)
	qt.Assert(t, ScopesMatch([]*Scope{scope1, scope3}, []*Scope{scope1, scope2}), qt.IsFalse)
}

func TestHasScope(t *testing.T) {
	scope1 := NewScope(nil)
	scope2 := NewScope(nil)
	scope3 := NewScope(nil)

	qt.Assert(t, HasScope([]*Scope{}, scope1), qt.IsFalse)
	qt.Assert(t, HasScope([]*Scope{scope1}, scope1), qt.IsTrue)
	qt.Assert(t, HasScope([]*Scope{scope1}, scope2), qt.IsFalse)
	qt.Assert(t, HasScope([]*Scope{scope1, scope2}, scope2), qt.IsTrue)
	qt.Assert(t, HasScope([]*Scope{scope1, scope2}, scope3), qt.IsFalse)
}

func TestAddScopeToSet(t *testing.T) {
	scope1 := NewScope(nil)
	scope2 := NewScope(nil)

	scopes := []*Scope{}
	scopes = AddScopeToSet(scopes, scope1)
	qt.Assert(t, len(scopes), qt.Equals, 1)
	qt.Assert(t, scopes[0], qt.Equals, scope1)

	scopes = AddScopeToSet(scopes, scope2)
	qt.Assert(t, len(scopes), qt.Equals, 2)
	qt.Assert(t, scopes[0], qt.Equals, scope2)
	qt.Assert(t, scopes[1], qt.Equals, scope1)

	scopes = AddScopeToSet(scopes, scope1)
	qt.Assert(t, len(scopes), qt.Equals, 2)
}

func TestRemoveScopeFromSet(t *testing.T) {
	scope1 := NewScope(nil)
	scope2 := NewScope(nil)
	scope3 := NewScope(nil)

	scopes := []*Scope{scope1, scope2, scope3}
	scopes = RemoveScopeFromSet(scopes, scope2)
	qt.Assert(t, len(scopes), qt.Equals, 2)
	qt.Assert(t, HasScope(scopes, scope1), qt.IsTrue)
	qt.Assert(t, HasScope(scopes, scope2), qt.IsFalse)
	qt.Assert(t, HasScope(scopes, scope3), qt.IsTrue)

	scopes = RemoveScopeFromSet(scopes, scope1)
	qt.Assert(t, len(scopes), qt.Equals, 1)
	qt.Assert(t, scopes[0], qt.Equals, scope3)

	scopes = RemoveScopeFromSet(scopes, scope3)
	qt.Assert(t, len(scopes), qt.Equals, 0)
}

// Test SyntaxComment
func TestSyntaxComment_New(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	comment := NewSyntaxComment("; hello", sctx)
	qt.Assert(t, comment, qt.IsNotNil)
	qt.Assert(t, comment.Text, qt.Equals, "; hello")
	qt.Assert(t, comment.SourceContext(), qt.Equals, sctx)
}

func TestSyntaxComment_Unwrap(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	comment := NewSyntaxComment("; hello", sctx)
	result := comment.Unwrap()
	qt.Assert(t, result, values.SchemeEquals, values.NewString("; hello"))
}

func TestSyntaxComment_UnwrapAll(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	comment := NewSyntaxComment("; hello", sctx)
	result := comment.UnwrapAll()
	qt.Assert(t, result, values.SchemeEquals, values.NewString("; hello"))
}

func TestSyntaxComment_IsVoid(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	comment := NewSyntaxComment("; hello", sctx)
	qt.Assert(t, comment.IsVoid(), qt.IsFalse)

	var nilComment *SyntaxComment
	qt.Assert(t, nilComment.IsVoid(), qt.IsTrue)
}

func TestSyntaxComment_EqualTo(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	comment1 := NewSyntaxComment("; hello", sctx)
	comment2 := NewSyntaxComment("; hello", sctx)
	comment3 := NewSyntaxComment("; world", sctx)

	qt.Assert(t, comment1.EqualTo(comment2), qt.IsTrue)
	qt.Assert(t, comment1.EqualTo(comment3), qt.IsFalse)
	qt.Assert(t, comment1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestSyntaxComment_SchemeString(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	comment := NewSyntaxComment("; hello", sctx)
	qt.Assert(t, comment.SchemeString(), qt.Equals, "; hello")
}

// Test SyntaxDirective
func TestSyntaxDirective_New(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	directive := NewSyntaxDirective("#!fold-case", sctx)
	qt.Assert(t, directive, qt.IsNotNil)
	qt.Assert(t, directive.Name, qt.Equals, "#!fold-case")
	qt.Assert(t, directive.SourceContext(), qt.Equals, sctx)
}

func TestSyntaxDirective_Unwrap(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	directive := NewSyntaxDirective("#!fold-case", sctx)
	result := directive.Unwrap()
	qt.Assert(t, result, values.SchemeEquals, values.NewString("#!fold-case"))
}

func TestSyntaxDirective_UnwrapAll(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	directive := NewSyntaxDirective("#!fold-case", sctx)
	result := directive.UnwrapAll()
	qt.Assert(t, result, values.SchemeEquals, values.NewString("#!fold-case"))
}

func TestSyntaxDirective_IsVoid(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	directive := NewSyntaxDirective("#!fold-case", sctx)
	qt.Assert(t, directive.IsVoid(), qt.IsFalse)

	var nilDirective *SyntaxDirective
	qt.Assert(t, nilDirective.IsVoid(), qt.IsTrue)
}

func TestSyntaxDirective_EqualTo(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	directive1 := NewSyntaxDirective("#!fold-case", sctx)
	directive2 := NewSyntaxDirective("#!fold-case", sctx)
	directive3 := NewSyntaxDirective("#!no-fold-case", sctx)

	qt.Assert(t, directive1.EqualTo(directive2), qt.IsTrue)
	qt.Assert(t, directive1.EqualTo(directive3), qt.IsFalse)
	qt.Assert(t, directive1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestSyntaxDirective_SchemeString(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	directive := NewSyntaxDirective("#!fold-case", sctx)
	qt.Assert(t, directive.SchemeString(), qt.Equals, "#!fold-case")
}

// Test SyntaxDatumComment
func TestSyntaxDatumComment_New(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value := NewSyntaxObject(values.NewInteger(42), sctx)
	datum := NewSyntaxDatumComment("#;", value, sctx)
	qt.Assert(t, datum, qt.IsNotNil)
	qt.Assert(t, datum.Label, qt.Equals, "#;")
	qt.Assert(t, datum.Value, qt.Equals, value)
	qt.Assert(t, datum.SourceContext(), qt.Equals, sctx)
}

func TestSyntaxDatumComment_Unwrap(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value := NewSyntaxObject(values.NewInteger(42), sctx)
	datum := NewSyntaxDatumComment("#;", value, sctx)
	result := datum.Unwrap()
	qt.Assert(t, result, qt.Equals, value)
}

func TestSyntaxDatumComment_UnwrapAll(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value := NewSyntaxObject(values.NewInteger(42), sctx)
	datum := NewSyntaxDatumComment("#;", value, sctx)
	result := datum.UnwrapAll()
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(42))
}

func TestSyntaxDatumComment_IsVoid(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value := NewSyntaxObject(values.NewInteger(42), sctx)
	datum := NewSyntaxDatumComment("#;", value, sctx)
	qt.Assert(t, datum.IsVoid(), qt.IsFalse)

	var nilDatum *SyntaxDatumComment
	qt.Assert(t, nilDatum.IsVoid(), qt.IsTrue)
}

func TestSyntaxDatumComment_EqualTo(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value1 := NewSyntaxObject(values.NewInteger(42), sctx)
	value2 := NewSyntaxObject(values.NewInteger(42), sctx)
	value3 := NewSyntaxObject(values.NewInteger(99), sctx)

	datum1 := NewSyntaxDatumComment("#;", value1, sctx)
	datum2 := NewSyntaxDatumComment("#;", value2, sctx)
	datum3 := NewSyntaxDatumComment("#;", value3, sctx)
	datum4 := NewSyntaxDatumComment("#|", value1, sctx)

	qt.Assert(t, datum1.EqualTo(datum2), qt.IsTrue)
	qt.Assert(t, datum1.EqualTo(datum3), qt.IsFalse)
	qt.Assert(t, datum1.EqualTo(datum4), qt.IsFalse)
	qt.Assert(t, datum1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestSyntaxDatumComment_SchemeString(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value := NewSyntaxObject(values.NewInteger(42), sctx)
	datum := NewSyntaxDatumComment("#;", value, sctx)
	result := datum.SchemeString()
	qt.Assert(t, result, qt.Contains, "#;")
	qt.Assert(t, result, qt.Contains, "42")
}

// Test SyntaxDatumLabel and SyntaxDatumLabelAssignment
func TestSyntaxDatumLabel_New(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	label := NewSyntaxDatumLabel(1, sctx)
	qt.Assert(t, label, qt.IsNotNil)
	qt.Assert(t, label.Label, qt.Equals, 1)
	qt.Assert(t, label.SourceContext(), qt.Equals, sctx)
}

func TestSyntaxDatumLabel_Unwrap(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	label := NewSyntaxDatumLabel(1, sctx)
	result := label.Unwrap()
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(1))
}

func TestSyntaxDatumLabel_UnwrapAll(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	label := NewSyntaxDatumLabel(1, sctx)
	result := label.UnwrapAll()
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(1))
}

func TestSyntaxDatumLabel_IsVoid(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	label := NewSyntaxDatumLabel(1, sctx)
	qt.Assert(t, label.IsVoid(), qt.IsFalse)

	var nilLabel *SyntaxDatumLabel
	qt.Assert(t, nilLabel.IsVoid(), qt.IsTrue)
}

func TestSyntaxDatumLabel_EqualTo(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	label1 := NewSyntaxDatumLabel(1, sctx)
	label2 := NewSyntaxDatumLabel(1, sctx)
	label3 := NewSyntaxDatumLabel(2, sctx)

	qt.Assert(t, label1.EqualTo(label2), qt.IsTrue)
	qt.Assert(t, label1.EqualTo(label3), qt.IsFalse)
	qt.Assert(t, label1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestSyntaxDatumLabel_SchemeString(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	label := NewSyntaxDatumLabel(1, sctx)
	qt.Assert(t, label.SchemeString(), qt.Equals, "1")
}

func TestSyntaxDatumLabelAssignment_New(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value := NewSyntaxObject(values.NewInteger(42), sctx)
	assignment := NewSyntaxDatumLabelAssignment(1, value, sctx)
	qt.Assert(t, assignment, qt.IsNotNil)
	qt.Assert(t, assignment.Label, qt.Equals, 1)
	qt.Assert(t, assignment.Value, qt.Equals, value)
	qt.Assert(t, assignment.SourceContext(), qt.Equals, sctx)
}

func TestSyntaxDatumLabelAssignment_Unwrap(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value := NewSyntaxObject(values.NewInteger(42), sctx)
	assignment := NewSyntaxDatumLabelAssignment(1, value, sctx)
	result := assignment.Unwrap()
	qt.Assert(t, result, qt.Equals, value)
}

func TestSyntaxDatumLabelAssignment_UnwrapAll(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value := NewSyntaxObject(values.NewInteger(42), sctx)
	assignment := NewSyntaxDatumLabelAssignment(1, value, sctx)
	result := assignment.UnwrapAll()
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(42))
}

func TestSyntaxDatumLabelAssignment_IsVoid(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value := NewSyntaxObject(values.NewInteger(42), sctx)
	assignment := NewSyntaxDatumLabelAssignment(1, value, sctx)
	qt.Assert(t, assignment.IsVoid(), qt.IsFalse)

	var nilAssignment *SyntaxDatumLabelAssignment
	qt.Assert(t, nilAssignment.IsVoid(), qt.IsTrue)
}

func TestSyntaxDatumLabelAssignment_EqualTo(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value1 := NewSyntaxObject(values.NewInteger(42), sctx)
	value2 := NewSyntaxObject(values.NewInteger(42), sctx)
	value3 := NewSyntaxObject(values.NewInteger(99), sctx)

	assignment1 := NewSyntaxDatumLabelAssignment(1, value1, sctx)
	assignment2 := NewSyntaxDatumLabelAssignment(1, value2, sctx)
	assignment3 := NewSyntaxDatumLabelAssignment(1, value3, sctx)
	assignment4 := NewSyntaxDatumLabelAssignment(2, value1, sctx)

	qt.Assert(t, assignment1.EqualTo(assignment2), qt.IsFalse)
	qt.Assert(t, assignment1.EqualTo(assignment1), qt.IsTrue)
	qt.Assert(t, assignment1.EqualTo(assignment3), qt.IsFalse)
	qt.Assert(t, assignment1.EqualTo(assignment4), qt.IsFalse)
	qt.Assert(t, assignment1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestSyntaxDatumLabelAssignment_SchemeString(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value := NewSyntaxObject(values.NewInteger(42), sctx)
	assignment := NewSyntaxDatumLabelAssignment(1, value, sctx)
	result := assignment.SchemeString()
	qt.Assert(t, result, qt.Equals, "1")
}

// Test SyntaxPair uncovered methods
func TestSyntaxPair_AddScope(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	value := NewSyntaxObject(values.NewInteger(42), sctx)
	pair := NewSyntaxCons(value, NewSyntaxEmptyList(sctx), sctx)

	scope := NewScope(nil)
	newPair := pair.AddScope(scope)

	qt.Assert(t, len(pair.sourceContext.Scopes), qt.Equals, 0)
	qt.Assert(t, len(newPair.(*SyntaxPair).sourceContext.Scopes), qt.Equals, 1)
	qt.Assert(t, newPair.(*SyntaxPair).sourceContext.Scopes[0], qt.Equals, scope)
}

func TestSyntaxPair_Scopes(t *testing.T) {
	scope := NewScope(nil)
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	sctx = sctx.WithScope(scope)
	pair := NewSyntaxCons(nil, nil, sctx)

	scopes := pair.Scopes()
	qt.Assert(t, len(scopes), qt.Equals, 1)
	qt.Assert(t, scopes[0], qt.Equals, scope)
}

func TestSyntaxPair_SourceContext(t *testing.T) {
	sctx := NewSourceContext("test", "file.scm", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	pair := NewSyntaxCons(nil, nil, sctx)
	qt.Assert(t, pair.SourceContext(), qt.Equals, sctx)
}

func TestSyntaxPair_Unwrap(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	car := NewSyntaxObject(values.NewInteger(1), sctx)
	cdr := NewSyntaxObject(values.NewInteger(2), sctx)
	pair := NewSyntaxCons(car, cdr, sctx)

	result := pair.Unwrap()
	resultPair, ok := result.(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, resultPair.Car(), qt.Equals, car)
	qt.Assert(t, resultPair.Cdr(), qt.Equals, cdr)
}

func TestSyntaxPair_SyntaxCar_SyntaxCdr(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	car := NewSyntaxObject(values.NewInteger(1), sctx)
	cdr := NewSyntaxEmptyList(sctx)
	pair := NewSyntaxCons(car, cdr, sctx)

	qt.Assert(t, pair.SyntaxCar(), qt.Equals, car)
	qt.Assert(t, pair.Car(), qt.Equals, car)
	qt.Assert(t, pair.Cdr(), qt.Equals, cdr)
}

func TestSyntaxPair_SetSyntaxCar_SetSyntaxCdr(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	car := NewSyntaxObject(values.NewInteger(1), sctx)
	cdr := NewSyntaxEmptyList(sctx)
	pair := NewSyntaxCons(car, cdr, sctx)

	newCar := NewSyntaxObject(values.NewInteger(10), sctx)
	cdr2 := NewSyntaxObject(values.NewInteger(20), sctx)

	pair.SetSyntaxCar(newCar)
	qt.Assert(t, pair.SyntaxCar(), qt.Equals, newCar)

	pair.SetSyntaxCdr(cdr2)
	qt.Assert(t, pair.Cdr(), qt.Equals, cdr2)
}

func TestSyntaxPair_Append_NonList(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	car := NewSyntaxObject(values.NewInteger(1), sctx)
	cdr := NewSyntaxObject(values.NewInteger(2), sctx)
	improperList := NewSyntaxCons(car, cdr, sctx)

	qt.Assert(t, func() {
		improperList.Append(values.NewInteger(3))
	}, qt.PanicMatches, "not a list")
}

func TestSyntaxPair_ForEach(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	list := NewSyntaxCons(
		NewSyntaxObject(values.NewInteger(1), sctx),
		NewSyntaxCons(
			NewSyntaxObject(values.NewInteger(2), sctx),
			NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(3), sctx),
				NewSyntaxEmptyList(sctx), sctx), sctx), sctx)

	count := 0
	sum := int64(0)
	list.ForEach(nil, func(_ context.Context, i int, hasNext bool, v values.Value) error { //nolint:errcheck
		count++
		syntaxVal := v.(SyntaxValue)
		intVal := syntaxVal.UnwrapAll().(*values.Integer)
		sum += intVal.Value
		return nil
	})

	qt.Assert(t, count, qt.Equals, 3)
	qt.Assert(t, sum, qt.Equals, int64(6))
}

func TestSyntaxPair_IsPair(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	car := NewSyntaxObject(values.NewInteger(1), sctx)
	cdr := NewSyntaxEmptyList(sctx)
	pair := NewSyntaxCons(car, cdr, sctx)
	qt.Assert(t, pair.IsPair(), qt.IsTrue)
}

// Test SyntaxSymbol uncovered methods
func TestSyntaxSymbol_AddScope(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	sym := NewSyntaxSymbol("foo", sctx)

	scope := NewScope(nil)
	newSym := sym.AddScope(scope)

	qt.Assert(t, len(sym.sourceContext.Scopes), qt.Equals, 0)
	qt.Assert(t, len(newSym.(*SyntaxSymbol).sourceContext.Scopes), qt.Equals, 1)
	qt.Assert(t, newSym.(*SyntaxSymbol).sourceContext.Scopes[0], qt.Equals, scope)
}

func TestSyntaxSymbol_Scopes(t *testing.T) {
	scope := NewScope(nil)
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	sctx = sctx.WithScope(scope)
	sym := NewSyntaxSymbol("foo", sctx)

	scopes := sym.Scopes()
	qt.Assert(t, len(scopes), qt.Equals, 1)
	qt.Assert(t, scopes[0], qt.Equals, scope)
}

func TestSyntaxSymbol_Datum(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	sym := NewSyntaxSymbol("foo", sctx)
	datum := sym.Datum()
	qt.Assert(t, datum.Key, qt.Equals, "foo")
}

func TestSyntaxSymbol_SourceContext(t *testing.T) {
	sctx := NewSourceContext("test", "file.scm", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	sym := NewSyntaxSymbol("foo", sctx)
	qt.Assert(t, sym.SourceContext(), qt.Equals, sctx)
}

func TestSyntaxSymbol_SchemeString(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	sym := NewSyntaxSymbol("foo", sctx)
	qt.Assert(t, sym.SchemeString(), qt.Contains, "foo")
}

// Test SyntaxObject uncovered methods
func TestSyntaxObject_AddScope(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	obj := NewSyntaxObject(values.NewInteger(42), sctx)

	scope := NewScope(nil)
	newObj := obj.AddScope(scope)

	qt.Assert(t, len(obj.sourceContext.Scopes), qt.Equals, 0)
	qt.Assert(t, len(newObj.(*SyntaxObject).sourceContext.Scopes), qt.Equals, 1)
	qt.Assert(t, newObj.(*SyntaxObject).sourceContext.Scopes[0], qt.Equals, scope)
}

func TestSyntaxObject_Scopes(t *testing.T) {
	scope := NewScope(nil)
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	sctx = sctx.WithScope(scope)
	obj := NewSyntaxObject(values.NewInteger(42), sctx)

	scopes := obj.Scopes()
	qt.Assert(t, len(scopes), qt.Equals, 1)
	qt.Assert(t, scopes[0], qt.Equals, scope)
}

func TestSyntaxObject_IsPair(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	obj := NewSyntaxObject(values.NewInteger(42), sctx)
	qt.Assert(t, obj.IsPair(), qt.IsFalse)
}

func TestSyntaxObject_IsEmptyList(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	obj := NewSyntaxObject(values.NewInteger(42), sctx)
	qt.Assert(t, obj.IsEmptyList(), qt.IsFalse)
}

func TestSyntaxObject_SourceContext(t *testing.T) {
	sctx := NewSourceContext("test", "file.scm", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	obj := NewSyntaxObject(values.NewInteger(42), sctx)
	qt.Assert(t, obj.SourceContext(), qt.Equals, sctx)
}

func TestSyntaxObject_IsVoid(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	obj := NewSyntaxObject(values.NewInteger(42), sctx)
	qt.Assert(t, obj.IsVoid(), qt.IsFalse)

	var nilObj *SyntaxObject
	qt.Assert(t, nilObj.IsVoid(), qt.IsTrue)
}

func TestSyntaxObject_EqualTo(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	obj1 := NewSyntaxObject(values.NewInteger(42), sctx)
	obj2 := NewSyntaxObject(values.NewInteger(42), sctx)
	obj3 := NewSyntaxObject(values.NewInteger(99), sctx)

	qt.Assert(t, obj1.EqualTo(obj1), qt.IsTrue)
	qt.Assert(t, obj1.EqualTo(obj2), qt.IsFalse)
	qt.Assert(t, obj1.EqualTo(obj3), qt.IsFalse)
	qt.Assert(t, obj1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

// Test NewScope and NewScopeID
func TestNewScope(t *testing.T) {
	scope := NewScope(nil)
	qt.Assert(t, scope, qt.IsNotNil)
}

func TestNewScopeID(t *testing.T) {
	id1 := NewScopeID()
	id2 := NewScopeID()
	qt.Assert(t, id1, qt.Not(qt.Equals), id2)
}

// Test NewSyntaxNil
func TestNewSyntaxNil(t *testing.T) {
	sctx := NewSourceContext("", "", NewSourceIndexes(0, 0, 0), NewSourceIndexes(0, 0, 0))
	syntaxNil := NewSyntaxNil(sctx)
	qt.Assert(t, syntaxNil, qt.IsNotNil)
	qt.Assert(t, syntaxNil.IsEmptyList(), qt.IsTrue)
}

// Test SyntaxEquals checker
func TestSyntaxEquals(t *testing.T) {
	checker := SyntaxEquals
	qt.Assert(t, checker.ArgNames(), qt.DeepEquals, []string{"got", "want"})

	sctx1 := NewSourceContext("test", "file.scm", NewSourceIndexes(0, 0, 1), NewSourceIndexes(5, 5, 1))
	sctx2 := NewSourceContext("test", "file.scm", NewSourceIndexes(0, 0, 1), NewSourceIndexes(5, 5, 1))
	sctx3 := NewSourceContext("other", "file.scm", NewSourceIndexes(0, 0, 1), NewSourceIndexes(5, 5, 1))

	sym1 := NewSyntaxSymbol("foo", sctx1)
	sym2 := NewSyntaxSymbol("foo", sctx2)
	sym3 := NewSyntaxSymbol("foo", sctx3)

	err := checker.Check(sym1, []interface{}{sym2}, nil)
	qt.Assert(t, err, qt.IsNil)

	err = checker.Check(sym1, []interface{}{sym3}, nil)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestSyntaxList_ElementSourceContext verifies that SyntaxList preserves
// individual element source contexts rather than applying the container's
// context to all elements.
func TestSyntaxList_ElementSourceContext(t *testing.T) {
	c := qt.New(t)

	// Create elements with different source contexts
	sc1 := NewSourceContext("a", "file.scm", NewSourceIndexes(0, 0, 1), NewSourceIndexes(1, 1, 1))
	sc2 := NewSourceContext("b", "file.scm", NewSourceIndexes(2, 2, 1), NewSourceIndexes(3, 3, 1))
	sc3 := NewSourceContext("c", "file.scm", NewSourceIndexes(4, 4, 1), NewSourceIndexes(5, 5, 1))

	elem1 := NewSyntaxSymbol("a", sc1)
	elem2 := NewSyntaxSymbol("b", sc2)
	elem3 := NewSyntaxSymbol("c", sc3)

	// Container context (should be used as fallback only)
	containerSc := NewSourceContext("(a b c)", "file.scm", NewSourceIndexes(0, 0, 1), NewSourceIndexes(7, 7, 1))

	// Build list
	list := SyntaxList(containerSc, elem1, elem2, elem3)

	// The first pair should use elem1's source context
	c.Assert(list.SourceContext(), qt.Equals, sc1)

	// Get second pair (cdr of first)
	cdr1, ok := list.Cdr().(*SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	c.Assert(cdr1.SourceContext(), qt.Equals, sc2)

	// Get third pair (cdr of second)
	cdr2, ok := cdr1.Cdr().(*SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	c.Assert(cdr2.SourceContext(), qt.Equals, sc3)
}

// TestSyntaxList_FallbackToContainer verifies that SyntaxList falls back
// to the container context when elements have no source context.
func TestSyntaxList_FallbackToContainer(t *testing.T) {
	c := qt.New(t)

	// Create elements without source contexts
	elem1 := NewSyntaxSymbol("a", nil)
	elem2 := NewSyntaxSymbol("b", nil)

	// Container context
	containerSc := NewSourceContext("(a b)", "file.scm", NewSourceIndexes(0, 0, 1), NewSourceIndexes(5, 5, 1))

	// Build list
	list := SyntaxList(containerSc, elem1, elem2)

	// Should fall back to container context
	c.Assert(list.SourceContext(), qt.Equals, containerSc)
}

// TestSyntaxList_EmptyList verifies empty list behavior.
func TestSyntaxList_EmptyList(t *testing.T) {
	c := qt.New(t)

	sc := NewSourceContext("()", "file.scm", NewSourceIndexes(0, 0, 1), NewSourceIndexes(2, 2, 1))
	list := SyntaxList(sc)

	c.Assert(list.IsEmptyList(), qt.IsTrue)
	c.Assert(list.SourceContext(), qt.Equals, sc)
}

// TestSyntaxList_SingleElement verifies single element list behavior.
func TestSyntaxList_SingleElement(t *testing.T) {
	c := qt.New(t)

	elemSc := NewSourceContext("x", "file.scm", NewSourceIndexes(1, 1, 1), NewSourceIndexes(2, 2, 1))
	elem := NewSyntaxSymbol("x", elemSc)

	containerSc := NewSourceContext("(x)", "file.scm", NewSourceIndexes(0, 0, 1), NewSourceIndexes(3, 3, 1))
	list := SyntaxList(containerSc, elem)

	// Should use element's source context for the pair
	c.Assert(list.SourceContext(), qt.Equals, elemSc)
}

// ============================================================================
// OriginInfo Tests
// ============================================================================

// TestOriginInfo_Depth tests the Depth method on OriginInfo.
func TestOriginInfo_Depth(t *testing.T) {
	c := qt.New(t)

	// Nil origin has depth 0
	var nilOrigin *OriginInfo
	c.Assert(nilOrigin.Depth(), qt.Equals, 0)

	// Single origin has depth 1
	origin1 := &OriginInfo{Identifier: "my-macro"}
	c.Assert(origin1.Depth(), qt.Equals, 1)

	// Chained origins have proper depth
	origin2 := &OriginInfo{Identifier: "inner-macro", Parent: origin1}
	c.Assert(origin2.Depth(), qt.Equals, 2)

	origin3 := &OriginInfo{Identifier: "outer-macro", Parent: origin2}
	c.Assert(origin3.Depth(), qt.Equals, 3)
}

// TestSourceContext_WithOrigin tests the WithOrigin method.
func TestSourceContext_WithOrigin(t *testing.T) {
	c := qt.New(t)

	// Create a source context
	sc := NewSourceContext("(my-macro x)", "test.scm",
		NewSourceIndexes(0, 0, 1), NewSourceIndexes(12, 12, 1))

	// Create origin info
	origin := &OriginInfo{
		Identifier: "my-macro",
		Location:   sc,
	}

	// WithOrigin should create a new context with origin attached
	scWithOrigin := sc.WithOrigin(origin)

	// Original should be unchanged (immutability)
	c.Assert(sc.Origin, qt.IsNil)

	// New context should have origin
	c.Assert(scWithOrigin.Origin, qt.Equals, origin)

	// Other fields should be preserved
	c.Assert(scWithOrigin.Text, qt.Equals, sc.Text)
	c.Assert(scWithOrigin.File, qt.Equals, sc.File)
	c.Assert(scWithOrigin.Start.EqualTo(sc.Start), qt.IsTrue)
	c.Assert(scWithOrigin.End.EqualTo(sc.End), qt.IsTrue)
}

// TestSourceContext_WithOrigin_Nil tests WithOrigin on nil SourceContext.
func TestSourceContext_WithOrigin_Nil(t *testing.T) {
	c := qt.New(t)

	var sc *SourceContext
	origin := &OriginInfo{Identifier: "my-macro"}

	scWithOrigin := sc.WithOrigin(origin)

	c.Assert(scWithOrigin, qt.IsNotNil)
	c.Assert(scWithOrigin.Origin, qt.Equals, origin)
}

// TestSourceContext_WithScope_PreservesOrigin tests that WithScope preserves Origin.
func TestSourceContext_WithScope_PreservesOrigin(t *testing.T) {
	c := qt.New(t)

	sc := NewSourceContext("x", "test.scm",
		NewSourceIndexes(0, 0, 1), NewSourceIndexes(1, 1, 1))
	origin := &OriginInfo{Identifier: "my-macro"}
	sc = sc.WithOrigin(origin)

	scope := NewScope(nil)
	scWithScope := sc.WithScope(scope)

	// Origin should be preserved
	c.Assert(scWithScope.Origin, qt.Equals, origin)
	// Scope should be added
	c.Assert(len(scWithScope.Scopes), qt.Equals, 1)
}

// TestSourceContext_WithScopes_PreservesOrigin tests that WithScopes preserves Origin.
func TestSourceContext_WithScopes_PreservesOrigin(t *testing.T) {
	c := qt.New(t)

	sc := NewSourceContext("x", "test.scm",
		NewSourceIndexes(0, 0, 1), NewSourceIndexes(1, 1, 1))
	origin := &OriginInfo{Identifier: "my-macro"}
	sc = sc.WithOrigin(origin)

	scopes := []*Scope{NewScope(nil), NewScope(nil)}
	scWithScopes := sc.WithScopes(scopes)

	// Origin should be preserved
	c.Assert(scWithScopes.Origin, qt.Equals, origin)
	// Scopes should be added
	c.Assert(len(scWithScopes.Scopes), qt.Equals, 2)
}

// TestFormatOriginChain tests the FormatOriginChain function.
func TestFormatOriginChain(t *testing.T) {
	c := qt.New(t)

	// Nil origin returns empty string
	c.Assert(FormatOriginChain(nil, 0), qt.Equals, "")

	// Single origin without location
	origin1 := &OriginInfo{Identifier: "my-macro"}
	result := FormatOriginChain(origin1, 0)
	c.Assert(result, qt.Contains, "expanded from 'my-macro'")

	// Single origin with location
	sc := NewSourceContext("", "test.scm",
		NewSourceIndexes(5, 5, 3), NewSourceIndexes(10, 10, 3))
	origin2 := &OriginInfo{Identifier: "other-macro", Location: sc}
	result = FormatOriginChain(origin2, 0)
	c.Assert(result, qt.Contains, "expanded from 'other-macro'")
	c.Assert(result, qt.Contains, "test.scm:3:5")

	// Chained origins
	inner := &OriginInfo{Identifier: "inner", Location: sc}
	outer := &OriginInfo{Identifier: "outer", Parent: inner}
	result = FormatOriginChain(outer, 0)
	c.Assert(result, qt.Contains, "expanded from 'outer'")
	c.Assert(result, qt.Contains, "expanded from 'inner'")
}

// TestFormatOriginChain_MaxDepth tests depth limiting in FormatOriginChain.
func TestFormatOriginChain_MaxDepth(t *testing.T) {
	c := qt.New(t)

	// Create a chain of 5 origins
	var origin *OriginInfo
	for i := 5; i >= 1; i-- {
		origin = &OriginInfo{
			Identifier: "macro" + string(rune('0'+i)),
			Parent:     origin,
		}
	}

	// With maxDepth=0 (unlimited), all 5 should appear
	result := FormatOriginChain(origin, 0)
	c.Assert(result, qt.Contains, "macro1")
	c.Assert(result, qt.Contains, "macro5")

	// With maxDepth=2, only first 2 plus truncation message
	result = FormatOriginChain(origin, 2)
	c.Assert(result, qt.Contains, "macro1")
	c.Assert(result, qt.Contains, "macro2")
	c.Assert(result, qt.Contains, "... and 3 more expansion(s)")
	c.Assert(result, qt.Not(qt.Contains), "macro3")
}
