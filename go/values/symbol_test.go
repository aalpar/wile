package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestSymbol_EqualTo(t *testing.T) {
	s0 := NewSymbol("a")
	s1 := NewSymbol("b")
	s2 := NewSymbol("a")
	// check pointer equality first
	qt.Assert(t, s0.EqualTo(s1), qt.Equals, false)
	qt.Assert(t, s0.EqualTo(s2), qt.Equals, true)
	qt.Assert(t, s0 == s1, qt.Equals, false)
	qt.Assert(t, s0 == s2, qt.Equals, false)
	qt.Assert(t, *s1 == *s2, qt.Equals, false)
	qt.Assert(t, *s0 == *s2, qt.Equals, true)
}
