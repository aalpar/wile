package utils

import (
	"skeme/values"
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/frankban/quicktest/qtsuite"
)

// PairIteratorSuite tests the PairIterator struct
type PairIteratorSuite struct{}

func (s *PairIteratorSuite) TestProperListIteration(c *qt.C) {
	// (1 2 3)
	list := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	iter := NewPairIterator(list)

	c.Assert(iter.Done(), qt.IsFalse)

	// First element
	car, done := iter.Next()
	c.Assert(car, values.SchemeEquals, values.NewInteger(1))
	c.Assert(done, qt.IsFalse)

	// Second element
	car, done = iter.Next()
	c.Assert(car, values.SchemeEquals, values.NewInteger(2))
	c.Assert(done, qt.IsFalse)

	// Third element (last)
	car, done = iter.Next()
	c.Assert(car, values.SchemeEquals, values.NewInteger(3))
	c.Assert(done, qt.IsTrue)

	c.Assert(iter.Done(), qt.IsTrue)
	c.Assert(values.IsEmptyList(iter.Tail()), qt.IsTrue)
}

func (s *PairIteratorSuite) TestSingleElementList(c *qt.C) {
	// (42)
	list := values.List(values.NewInteger(42))
	iter := NewPairIterator(list)

	c.Assert(iter.Done(), qt.IsFalse)

	car, done := iter.Next()
	c.Assert(car, values.SchemeEquals, values.NewInteger(42))
	c.Assert(done, qt.IsTrue)
	c.Assert(iter.Done(), qt.IsTrue)
	c.Assert(values.IsEmptyList(iter.Tail()), qt.IsTrue)
}

func (s *PairIteratorSuite) TestImproperList(c *qt.C) {
	// (1 . 2)
	pair := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	iter := NewPairIterator(pair)

	c.Assert(iter.Done(), qt.IsFalse)

	car, done := iter.Next()
	c.Assert(car, values.SchemeEquals, values.NewInteger(1))
	c.Assert(done, qt.IsTrue)
	c.Assert(iter.Done(), qt.IsTrue)
	// Tail should be the improper terminator
	c.Assert(iter.Tail(), values.SchemeEquals, values.NewInteger(2))
}

func (s *PairIteratorSuite) TestImproperListLonger(c *qt.C) {
	// (1 2 . 3)
	list := values.NewCons(values.NewInteger(1),
		values.NewCons(values.NewInteger(2), values.NewInteger(3)))
	iter := NewPairIterator(list)

	car, done := iter.Next()
	c.Assert(car, values.SchemeEquals, values.NewInteger(1))
	c.Assert(done, qt.IsFalse)

	car, done = iter.Next()
	c.Assert(car, values.SchemeEquals, values.NewInteger(2))
	c.Assert(done, qt.IsTrue)

	c.Assert(iter.Tail(), values.SchemeEquals, values.NewInteger(3))
}

func (s *PairIteratorSuite) TestHead(c *qt.C) {
	list := values.List(values.NewInteger(1), values.NewInteger(2))
	iter := NewPairIterator(list)

	c.Assert(iter.Head(), qt.Equals, values.Value(list))

	// Head should remain unchanged after iteration
	_, _ = iter.Next()
	_, _ = iter.Next()
	c.Assert(iter.Head(), qt.Equals, values.Value(list))
}

func (s *PairIteratorSuite) TestCurrent(c *qt.C) {
	// (1 2)
	list := values.List(values.NewInteger(1), values.NewInteger(2))
	iter := NewPairIterator(list)

	// Current starts at head
	c.Assert(iter.Current(), qt.Equals, values.Value(list))

	// After first Next, current advances
	_, _ = iter.Next()
	cur := iter.Current().(*values.Pair)
	c.Assert(cur.Car(), values.SchemeEquals, values.NewInteger(2))
}

func (s *PairIteratorSuite) TestReset(c *qt.C) {
	list := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	iter := NewPairIterator(list)

	// Iterate through all elements
	for !iter.Done() {
		_, _ = iter.Next()
	}
	c.Assert(iter.Done(), qt.IsTrue)

	// Reset and iterate again
	iter.Reset()
	c.Assert(iter.Done(), qt.IsFalse)

	var collected []int64
	for !iter.Done() {
		car, _ := iter.Next()
		collected = append(collected, int64(car.(*values.Integer).Value))
	}
	c.Assert(collected, qt.DeepEquals, []int64{1, 2, 3})
}

func (s *PairIteratorSuite) TestNestedList(c *qt.C) {
	// ((1 2) (3 4))
	inner1 := values.List(values.NewInteger(1), values.NewInteger(2))
	inner2 := values.List(values.NewInteger(3), values.NewInteger(4))
	outer := values.List(inner1, inner2)

	iter := NewPairIterator(outer)

	car, done := iter.Next()
	c.Assert(car, values.SchemeEquals, inner1)
	c.Assert(done, qt.IsFalse)

	car, done = iter.Next()
	c.Assert(car, values.SchemeEquals, inner2)
	c.Assert(done, qt.IsTrue)
}

func (s *PairIteratorSuite) TestMixedTypes(c *qt.C) {
	// (1 "hello" #t)
	list := values.List(
		values.NewInteger(1),
		values.NewString("hello"),
		values.TrueValue)
	iter := NewPairIterator(list)

	car, _ := iter.Next()
	c.Assert(car, values.SchemeEquals, values.NewInteger(1))

	car, _ = iter.Next()
	c.Assert(car, values.SchemeEquals, values.NewString("hello"))

	car, done := iter.Next()
	c.Assert(car, qt.Equals, values.TrueValue)
	c.Assert(done, qt.IsTrue)
}

func (s *PairIteratorSuite) TestSymbolList(c *qt.C) {
	// (a b c)
	list := values.List(
		values.NewSymbol("a"),
		values.NewSymbol("b"),
		values.NewSymbol("c"))
	iter := NewPairIterator(list)

	var symbols []string
	for !iter.Done() {
		car, _ := iter.Next()
		symbols = append(symbols, car.(*values.Symbol).Key)
	}
	c.Assert(symbols, qt.DeepEquals, []string{"a", "b", "c"})
}

func (s *PairIteratorSuite) TestCollectAllElements(c *qt.C) {
	// Test collecting all elements using the iterator pattern
	list := values.List(
		values.NewInteger(10),
		values.NewInteger(20),
		values.NewInteger(30),
		values.NewInteger(40),
		values.NewInteger(50))
	iter := NewPairIterator(list)

	var sum int64
	for !iter.Done() {
		car, _ := iter.Next()
		sum += car.(*values.Integer).Value
	}
	c.Assert(sum, qt.Equals, int64(150))
}

func TestPairIterator(t *testing.T) {
	qtsuite.Run(qt.New(t), &PairIteratorSuite{})
}
