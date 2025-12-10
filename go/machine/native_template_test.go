package machine

import (
	"testing"

	qt "github.com/frankban/quicktest"
	"skeme/values"
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
