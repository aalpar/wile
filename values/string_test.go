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

package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestString_EqualTo(t *testing.T) {
	s1 := NewString("hello")
	s2 := NewString("hello")
	qt.Assert(t, s1.EqualTo(s2), qt.IsTrue)

	s3 := NewString("world")
	qt.Assert(t, s1.EqualTo(s3), qt.IsFalse)

	i := NewInteger(42)
	qt.Assert(t, s1.EqualTo(i), qt.IsFalse)
}

func TestString_Datum(t *testing.T) {
	s := NewString("hello")
	qt.Assert(t, s.Datum(), qt.Equals, "hello")
}

func TestString_String(t *testing.T) {
	s := NewString("hello")
	qt.Assert(t, s.String(), qt.Equals, "hello")
}

func TestString_SchemeString(t *testing.T) {
	s := NewString("hello")
	qt.Assert(t, s.SchemeString(), qt.Equals, `"hello"`)
}

func TestString_IsVoid(t *testing.T) {
	s := NewString("hello")
	qt.Assert(t, s.IsVoid(), qt.IsFalse)

	var nilString *String
	qt.Assert(t, nilString.IsVoid(), qt.IsTrue)
}

func TestString_Interning(t *testing.T) {
	// Short strings should be interned (same pointer)
	s1 := NewString("hello")
	s2 := NewString("hello")
	qt.Assert(t, s1 == s2, qt.IsTrue, qt.Commentf("short strings should return same pointer"))

	// Different strings should have different pointers
	s3 := NewString("world")
	qt.Assert(t, s1 != s3, qt.IsTrue, qt.Commentf("different strings should have different pointers"))

	// Empty string should be interned
	sEmpty1 := NewString("")
	sEmpty2 := NewString("")
	qt.Assert(t, sEmpty1 == sEmpty2, qt.IsTrue, qt.Commentf("empty strings should return same pointer"))

	// Strings at the boundary (64 chars) should be interned
	boundary := "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" // 64 chars
	sBoundary1 := NewString(boundary)
	sBoundary2 := NewString(boundary)
	qt.Assert(t, sBoundary1 == sBoundary2, qt.IsTrue, qt.Commentf("64-char strings should be interned"))

	// Long strings (>64 chars) should NOT be interned
	longStr := "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" // 65 chars
	sLong1 := NewString(longStr)
	sLong2 := NewString(longStr)
	qt.Assert(t, sLong1 != sLong2, qt.IsTrue, qt.Commentf("strings over 64 chars should not be interned"))

	// Interned strings should still have correct values
	qt.Assert(t, s1.Value, qt.Equals, "hello")
	qt.Assert(t, sLong1.Value, qt.Equals, longStr)
}

func TestString_InternString(t *testing.T) {
	// InternString should always intern regardless of length
	longStr := "this is a very long string that exceeds the automatic interning threshold of 64 characters"
	sIntern1 := InternString(longStr)
	sIntern2 := InternString(longStr)
	qt.Assert(t, sIntern1 == sIntern2, qt.IsTrue, qt.Commentf("InternString should always return same pointer"))

	// InternString should work for short strings too
	sShort1 := InternString("short")
	sShort2 := InternString("short")
	qt.Assert(t, sShort1 == sShort2, qt.IsTrue)

	// Values should be correct
	qt.Assert(t, sIntern1.Value, qt.Equals, longStr)
}

func TestStringImmutability(t *testing.T) {
	c := qt.New(t)

	t.Run("interned strings are immutable", func(t *testing.T) {
		s := NewString("hello") // ≤64 bytes, gets interned
		c.Assert(s.IsImmutable(), qt.IsTrue)
	})

	t.Run("mutable strings are mutable", func(t *testing.T) {
		s := NewMutableString("hello")
		c.Assert(s.IsImmutable(), qt.IsFalse)
	})

	t.Run("SetChar fails on immutable", func(t *testing.T) {
		s := NewString("x")
		err := s.SetChar(0, 'y')
		c.Assert(err, qt.Not(qt.IsNil))
		c.Assert(err, qt.ErrorMatches, ".*immutable.*")
	})

	t.Run("SetChar succeeds on mutable", func(t *testing.T) {
		s := NewMutableString("x")
		err := s.SetChar(0, 'y')
		c.Assert(err, qt.IsNil)
		c.Assert(s.Value, qt.Equals, "y")
	})

	t.Run("Fill fails on immutable", func(t *testing.T) {
		s := NewString("hello")
		err := s.Fill('x', 0, 5)
		c.Assert(err, qt.Not(qt.IsNil))
		c.Assert(err, qt.ErrorMatches, ".*immutable.*")
	})

	t.Run("Fill succeeds on mutable", func(t *testing.T) {
		s := NewMutableString("hello")
		err := s.Fill('x', 0, 5)
		c.Assert(err, qt.IsNil)
		c.Assert(s.Value, qt.Equals, "xxxxx")
	})

	t.Run("SetValue fails on immutable", func(t *testing.T) {
		s := NewString("hello")
		err := s.SetValue("world")
		c.Assert(err, qt.Not(qt.IsNil))
		c.Assert(err, qt.ErrorMatches, ".*immutable.*")
	})

	t.Run("SetValue succeeds on mutable", func(t *testing.T) {
		s := NewMutableString("hello")
		err := s.SetValue("world")
		c.Assert(err, qt.IsNil)
		c.Assert(s.Value, qt.Equals, "world")
	})

	t.Run("Set fails on immutable", func(t *testing.T) {
		s := NewString("hello")
		err := s.Set(0, NewCharacter('H'))
		c.Assert(err, qt.Not(qt.IsNil))
		c.Assert(err, qt.ErrorMatches, ".*immutable.*")
	})

	t.Run("Set succeeds on mutable", func(t *testing.T) {
		s := NewMutableString("hello")
		err := s.Set(0, NewCharacter('H'))
		c.Assert(err, qt.IsNil)
		c.Assert(s.Value, qt.Equals, "Hello")
	})
}

func TestStringInternPreserved(t *testing.T) {
	c := qt.New(t)

	s1 := NewString("test")
	s2 := NewString("test")

	// Same interned pointer
	c.Assert(s1 == s2, qt.IsTrue)

	// Cannot mutate
	err := s1.SetChar(0, 'X')
	c.Assert(err, qt.Not(qt.IsNil))

	// Both still unchanged
	c.Assert(s1.Value, qt.Equals, "test")
	c.Assert(s2.Value, qt.Equals, "test")
}
