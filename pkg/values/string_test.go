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

package values_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

func TestString_EqualTo(t *testing.T) {
	s1 := values.NewString("hello")
	s2 := values.NewString("hello")
	qt.Assert(t, s1.EqualTo(s2), qt.IsTrue)

	s3 := values.NewString("world")
	qt.Assert(t, s1.EqualTo(s3), qt.IsFalse)

	i := values.NewInteger(42)
	qt.Assert(t, s1.EqualTo(i), qt.IsFalse)
}

func TestString_Datum(t *testing.T) {
	s := values.NewString("hello")
	qt.Assert(t, s.Value, qt.Equals, "hello")
}

func TestString_String(t *testing.T) {
	s := values.NewString("hello")
	qt.Assert(t, s.String(), qt.Equals, "hello")
}

func TestString_SchemeString(t *testing.T) {
	s := values.NewString("hello")
	qt.Assert(t, s.SchemeString(), qt.Equals, `"hello"`)
}

func TestString_IsVoid(t *testing.T) {
	s := values.NewString("hello")
	qt.Assert(t, s.IsVoid(), qt.IsFalse)

	var nilString *values.String
	qt.Assert(t, nilString.IsVoid(), qt.IsTrue)
}

func TestString_Allocation(t *testing.T) {
	// Each NewString call produces a fresh allocation
	s1 := values.NewString("hello")
	s2 := values.NewString("hello")
	qt.Assert(t, s1 != s2, qt.IsTrue, qt.Commentf("each NewString should allocate"))

	// But they are structurally equal
	qt.Assert(t, s1.EqualTo(s2), qt.IsTrue)

	// Different strings are not equal
	s3 := values.NewString("world")
	qt.Assert(t, s1.EqualTo(s3), qt.IsFalse)

	// Values are correct
	qt.Assert(t, s1.Value, qt.Equals, "hello")
	qt.Assert(t, s3.Value, qt.Equals, "world")
}

func TestString_NewStringImmutable(t *testing.T) {
	// NewString always returns immutable strings
	s := values.NewString("hello")
	qt.Assert(t, s.IsImmutable(), qt.IsTrue)

	// NewMutableString returns mutable strings
	m := values.NewMutableString("hello")
	qt.Assert(t, m.IsImmutable(), qt.IsFalse)

	// Both have correct values
	qt.Assert(t, s.Value, qt.Equals, "hello")
	qt.Assert(t, m.Value, qt.Equals, "hello")
}

func TestStringImmutability(t *testing.T) {
	c := qt.New(t)

	t.Run("NewString returns immutable", func(t *testing.T) {
		s := values.NewString("hello")
		c.Assert(s.IsImmutable(), qt.IsTrue)
	})

	t.Run("mutable strings are mutable", func(t *testing.T) {
		s := values.NewMutableString("hello")
		c.Assert(s.IsImmutable(), qt.IsFalse)
	})

	t.Run("SetChar fails on immutable", func(t *testing.T) {
		s := values.NewString("x")
		err := s.SetChar(0, 'y')
		c.Assert(err, qt.Not(qt.IsNil))
		c.Assert(err, qt.ErrorMatches, ".*immutable.*")
	})

	t.Run("SetChar succeeds on mutable", func(t *testing.T) {
		s := values.NewMutableString("x")
		err := s.SetChar(0, 'y')
		c.Assert(err, qt.IsNil)
		c.Assert(s.Value, qt.Equals, "y")
	})

	t.Run("Fill fails on immutable", func(t *testing.T) {
		s := values.NewString("hello")
		err := s.Fill('x', 0, 5)
		c.Assert(err, qt.Not(qt.IsNil))
		c.Assert(err, qt.ErrorMatches, ".*immutable.*")
	})

	t.Run("Fill succeeds on mutable", func(t *testing.T) {
		s := values.NewMutableString("hello")
		err := s.Fill('x', 0, 5)
		c.Assert(err, qt.IsNil)
		c.Assert(s.Value, qt.Equals, "xxxxx")
	})

	t.Run("SetValue fails on immutable", func(t *testing.T) {
		s := values.NewString("hello")
		err := s.SetValue("world")
		c.Assert(err, qt.Not(qt.IsNil))
		c.Assert(err, qt.ErrorMatches, ".*immutable.*")
	})

	t.Run("SetValue succeeds on mutable", func(t *testing.T) {
		s := values.NewMutableString("hello")
		err := s.SetValue("world")
		c.Assert(err, qt.IsNil)
		c.Assert(s.Value, qt.Equals, "world")
	})

	t.Run("Set fails on immutable", func(t *testing.T) {
		s := values.NewString("hello")
		err := s.Set(0, values.NewCharacter('H'))
		c.Assert(err, qt.Not(qt.IsNil))
		c.Assert(err, qt.ErrorMatches, ".*immutable.*")
	})

	t.Run("Set succeeds on mutable", func(t *testing.T) {
		s := values.NewMutableString("hello")
		s.Set(0, values.NewCharacter('H'))
		c.Assert(s.Value, qt.Equals, "Hello")
	})
}

func TestStringImmutablePreserved(t *testing.T) {
	c := qt.New(t)

	s1 := values.NewString("test")

	// Cannot mutate immutable strings
	err := s1.SetChar(0, 'X')
	c.Assert(err, qt.Not(qt.IsNil))

	// Still unchanged
	c.Assert(s1.Value, qt.Equals, "test")
}
