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

package machine

import (
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestNewNativeTemplate(t *testing.T) {
	NewNativeTemplate(0, 0, false, NewOperationPush())
}

func TestNativeTemplate_DeduplicateLiteral(t *testing.T) {
	c := qt.New(t)

	tmpl := NewNativeTemplate(0, 0, false)

	sym1 := values.NewSymbol("foo")
	tmpl.MaybeAppendLiteral(sym1)

	sym2 := values.NewSymbol("foo")
	dedupedSym := tmpl.DeduplicateLiteral(sym2)
	c.Assert(dedupedSym == sym1, qt.IsTrue, qt.Commentf("symbol should be deduplicated to same instance"))
	c.Assert(dedupedSym == sym2, qt.IsFalse, qt.Commentf("symbol should not be the new instance"))

	int1 := values.NewInteger(42)
	tmpl.MaybeAppendLiteral(int1)

	int2 := values.NewInteger(42)
	dedupedInt := tmpl.DeduplicateLiteral(int2)
	c.Assert(dedupedInt == int1, qt.IsTrue, qt.Commentf("integer should be deduplicated to same instance"))
}

func TestNativeTemplate_DeduplicateLiteral_NestedPair(t *testing.T) {
	c := qt.New(t)

	tmpl := NewNativeTemplate(0, 0, false)

	symB := values.NewSymbol("b")
	tmpl.MaybeAppendLiteral(symB)

	innerSym := values.NewSymbol("b")
	innerInt := values.NewInteger(2)
	innerPair := values.NewCons(innerSym, values.NewCons(innerInt, values.EmptyList))
	outerPair := values.NewCons(innerPair, values.EmptyList)

	deduped := tmpl.DeduplicateLiteral(outerPair)
	dedupedPair, ok := deduped.(*values.Pair)
	c.Assert(ok, qt.IsTrue)

	dedupedInner, ok := dedupedPair.Car().(*values.Pair)
	c.Assert(ok, qt.IsTrue)

	dedupedSymInner := dedupedInner.Car()
	c.Assert(dedupedSymInner == symB, qt.IsTrue, qt.Commentf("symbol inside nested pair should be deduplicated"))
}

func TestNativeTemplate_DeduplicateLiteral_Vector(t *testing.T) {
	c := qt.New(t)

	tmpl := NewNativeTemplate(0, 0, false)

	sym := values.NewSymbol("x")
	tmpl.MaybeAppendLiteral(sym)

	vec := values.NewVector(values.NewSymbol("x"), values.NewInteger(1))
	deduped := tmpl.DeduplicateLiteral(vec)

	dedupedVec, ok := deduped.(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert((*dedupedVec)[0] == sym, qt.IsTrue, qt.Commentf("symbol inside vector should be deduplicated"))
}

func TestNativeTemplate_ValueCount(t *testing.T) {
	tpl := NewNativeTemplate(3, 5, false)
	qt.Assert(t, tpl.ValueCount(), qt.Equals, 5)
}

func TestNativeTemplate_SchemeString(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)
	qt.Assert(t, tpl.SchemeString(), qt.Equals, "#<native-template>")
}

func TestNativeTemplate_IsVoid(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)
	qt.Assert(t, tpl.IsVoid(), qt.IsFalse)

	var nilTpl *NativeTemplate
	qt.Assert(t, nilTpl.IsVoid(), qt.IsTrue)
}

func TestNativeTemplate_Copy(t *testing.T) {
	tpl := NewNativeTemplate(2, 3, true, NewOperationPush())
	tpl.MaybeAppendLiteral(values.NewInteger(42))
	tpl.MaybeAppendLiteral(values.NewSymbol("foo"))

	copy := tpl.Copy()

	// Verify copy is different object
	qt.Assert(t, copy != tpl, qt.IsTrue)
	// Verify fields match
	qt.Assert(t, copy.parameterCount, qt.Equals, tpl.parameterCount)
	qt.Assert(t, copy.valueCount, qt.Equals, tpl.valueCount)
	qt.Assert(t, copy.isVariadic, qt.Equals, tpl.isVariadic)
	qt.Assert(t, len(copy.literals), qt.Equals, len(tpl.literals))
	qt.Assert(t, len(copy.operations), qt.Equals, len(tpl.operations))
}

func TestNativeTemplate_Copy_Nil(t *testing.T) {
	var nilTpl *NativeTemplate
	copy := nilTpl.Copy()
	qt.Assert(t, copy, qt.IsNil)
}

func TestNativeTemplate_EqualTo(t *testing.T) {
	tpl1 := NewNativeTemplate(2, 3, true, NewOperationPush())
	tpl1.MaybeAppendLiteral(values.NewInteger(42))

	tpl2 := NewNativeTemplate(2, 3, true, NewOperationPush())
	tpl2.MaybeAppendLiteral(values.NewInteger(42))

	tpl3 := NewNativeTemplate(2, 3, true, NewOperationPush())
	tpl3.MaybeAppendLiteral(values.NewInteger(99)) // Different literal

	// Equal templates
	qt.Assert(t, tpl1.EqualTo(tpl2), qt.IsTrue)

	// Different literals
	qt.Assert(t, tpl1.EqualTo(tpl3), qt.IsFalse)

	// Different parameter count
	tpl4 := NewNativeTemplate(3, 3, true, NewOperationPush())
	tpl4.MaybeAppendLiteral(values.NewInteger(42))
	qt.Assert(t, tpl1.EqualTo(tpl4), qt.IsFalse)

	// Different value count
	tpl5 := NewNativeTemplate(2, 4, true, NewOperationPush())
	tpl5.MaybeAppendLiteral(values.NewInteger(42))
	qt.Assert(t, tpl1.EqualTo(tpl5), qt.IsFalse)

	// Different variadic flag
	tpl6 := NewNativeTemplate(2, 3, false, NewOperationPush())
	tpl6.MaybeAppendLiteral(values.NewInteger(42))
	qt.Assert(t, tpl1.EqualTo(tpl6), qt.IsFalse)

	// Different operation count
	tpl7 := NewNativeTemplate(2, 3, true, NewOperationPush(), NewOperationPush())
	tpl7.MaybeAppendLiteral(values.NewInteger(42))
	qt.Assert(t, tpl1.EqualTo(tpl7), qt.IsFalse)

	// Different type
	qt.Assert(t, tpl1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	// Nil comparison
	var nilTpl *NativeTemplate
	qt.Assert(t, nilTpl.EqualTo(nilTpl), qt.IsTrue)
}

func TestNativeTemplate_DeduplicateLiteral_EmptyPair(t *testing.T) {
	tmpl := NewNativeTemplate(0, 0, false)

	// Empty list
	result := tmpl.DeduplicateLiteral(values.EmptyList)
	qt.Assert(t, result, qt.Equals, values.EmptyList)
}

func TestNativeTemplate_DeduplicateLiteral_EmptyVector(t *testing.T) {
	tmpl := NewNativeTemplate(0, 0, false)

	// Empty vector
	emptyVec := values.NewVector()
	result := tmpl.DeduplicateLiteral(emptyVec)
	qt.Assert(t, result, qt.Equals, emptyVec)
}

func TestNativeTemplate_DeduplicateLiteral_NoChange(t *testing.T) {
	tmpl := NewNativeTemplate(0, 0, false)

	// Pair with elements that don't need deduplication
	pair := values.NewCons(values.NewFloat(3.14), values.EmptyList)
	result := tmpl.DeduplicateLiteral(pair)
	// Same object since no deduplication happened
	qt.Assert(t, result, qt.Equals, pair)

	// Vector with no change
	vec := values.NewVector(values.NewFloat(1.0), values.NewFloat(2.0))
	result = tmpl.DeduplicateLiteral(vec)
	qt.Assert(t, result, qt.Equals, vec)
}

func TestNativeTemplate_DeduplicateLiteral_OtherTypes(t *testing.T) {
	tmpl := NewNativeTemplate(0, 0, false)

	// Float - not deduplicated, just returned
	f := values.NewFloat(3.14)
	result := tmpl.DeduplicateLiteral(f)
	qt.Assert(t, result, qt.Equals, f)

	// String
	s := values.NewString("hello")
	result = tmpl.DeduplicateLiteral(s)
	qt.Assert(t, result, qt.Equals, s)
}

// Tests moved from coverage_additional_test.go
// TestNativeTemplateMethodsAdditional tests NativeTemplate methods
func TestNativeTemplateMethodsAdditional(t *testing.T) {
	tpl := NewNativeTemplate(2, 1, true)

	qt.Assert(t, tpl.ParameterCount(), qt.Equals, 2)
	qt.Assert(t, tpl.IsVariadic(), qt.IsTrue)
	qt.Assert(t, tpl.IsVoid(), qt.IsFalse)
	qt.Assert(t, tpl.SchemeString(), qt.Contains, "native-template")

	// Test EqualTo
	tpl2 := NewNativeTemplate(2, 1, true)
	qt.Assert(t, tpl.EqualTo(tpl2), qt.IsTrue)

	tpl3 := NewNativeTemplate(3, 1, true)
	qt.Assert(t, tpl.EqualTo(tpl3), qt.IsFalse)

	var nilTpl *NativeTemplate
	qt.Assert(t, tpl.EqualTo(nilTpl), qt.IsFalse)
}

// TestNativeTemplateDeduplicateLiteralVector tests vector literal deduplication
func TestNativeTemplateDeduplicateLiteralVector(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Create a vector with symbols
	sym := values.NewSymbol("test")
	vec := values.NewVector(sym, values.NewInteger(42))

	// Deduplicate
	deduped := tpl.DeduplicateLiteral(vec)
	qt.Assert(t, deduped, qt.IsNotNil)
}

// TestNativeTemplateDeduplicateLiteralEmptyVector tests empty vector deduplication
func TestNativeTemplateDeduplicateLiteralEmptyVector(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Empty vector
	vec := values.NewVector()
	deduped := tpl.DeduplicateLiteral(vec)
	qt.Assert(t, deduped, qt.Equals, vec)
}

// TestNativeTemplateCopyNil tests Copy on nil NativeTemplate
func TestNativeTemplateCopyNil(t *testing.T) {
	var tpl *NativeTemplate
	cpy := tpl.Copy()
	qt.Assert(t, cpy, qt.IsNil)
}

// TestNativeTemplateEqualToDifferent tests NativeTemplate EqualTo with different templates
func TestNativeTemplateEqualToDifferent(t *testing.T) {
	tpl1 := NewNativeTemplate(1, 1, false)
	tpl2 := NewNativeTemplate(2, 1, false) // Different parameter count
	qt.Assert(t, tpl1.EqualTo(tpl2), qt.IsFalse)

	tpl3 := NewNativeTemplate(1, 2, false) // Different value count
	qt.Assert(t, tpl1.EqualTo(tpl3), qt.IsFalse)

	tpl4 := NewNativeTemplate(1, 1, true) // Different variadic
	qt.Assert(t, tpl1.EqualTo(tpl4), qt.IsFalse)

	// Different type
	qt.Assert(t, tpl1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	// Same
	tpl5 := NewNativeTemplate(1, 1, false)
	qt.Assert(t, tpl1.EqualTo(tpl5), qt.IsTrue)
}

// TestNativeTemplateEqualToNil tests NativeTemplate EqualTo with nil
func TestNativeTemplateEqualToNil(t *testing.T) {
	tpl1 := NewNativeTemplate(1, 1, false)
	var tpl2 *NativeTemplate
	qt.Assert(t, tpl1.EqualTo(tpl2), qt.IsFalse)
	qt.Assert(t, tpl2.EqualTo(tpl1), qt.IsFalse)
	qt.Assert(t, tpl2.EqualTo(tpl2), qt.IsTrue) // nil == nil
}

// TestNativeTemplateLiterals tests NativeTemplate literal methods
func TestNativeTemplateLiterals(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Add some literals
	idx1 := tpl.MaybeAppendLiteral(values.NewInteger(1))
	idx2 := tpl.MaybeAppendLiteral(values.NewInteger(2))
	idx3 := tpl.MaybeAppendLiteral(values.NewInteger(1)) // duplicate

	qt.Assert(t, idx1, qt.Equals, LiteralIndex(0))
	qt.Assert(t, idx2, qt.Equals, LiteralIndex(1))
	qt.Assert(t, idx3, qt.Equals, LiteralIndex(0)) // should be same as idx1

	// Test findLiteral
	found := tpl.findLiteral(values.NewInteger(2))
	qt.Assert(t, found, qt.IsNotNil)

	notFound := tpl.findLiteral(values.NewInteger(999))
	qt.Assert(t, notFound, qt.IsNil)
}

// TestNativeTemplateDeduplicateLiteral tests deduplication edge cases
func TestNativeTemplateDeduplicateLiteral(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Test with nil Pair
	var nilPair *values.Pair = nil
	result := tpl.DeduplicateLiteral(nilPair)
	qt.Assert(t, result, qt.Equals, nilPair)

	// Test with empty list
	result = tpl.DeduplicateLiteral(values.EmptyList)
	qt.Assert(t, result, qt.Equals, values.EmptyList)

	// Test with nil Vector
	var nilVec *values.Vector = nil
	result = tpl.DeduplicateLiteral(nilVec)
	qt.Assert(t, result, qt.Equals, nilVec)

	// Test with empty vector
	emptyVec := values.NewVector()
	result = tpl.DeduplicateLiteral(emptyVec)
	qt.Assert(t, result, qt.Equals, emptyVec)

	// Test with non-deduplicatable type (string)
	str := values.NewString("hello")
	result = tpl.DeduplicateLiteral(str)
	qt.Assert(t, result, qt.Equals, str)
}
