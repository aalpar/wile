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

package wile

import (
	"errors"
	"math/big"
	"testing"

	"github.com/aalpar/wile/registry"

	qt "github.com/frankban/quicktest"
)

// Value constructors

func TestNewFloat(t *testing.T) {
	c := qt.New(t)
	v := NewFloat(3.14)
	c.Assert(v.SchemeString(), qt.Equals, "3.14")
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewString(t *testing.T) {
	c := qt.New(t)
	v := NewString("hello")
	c.Assert(v.SchemeString(), qt.Equals, `"hello"`)
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewSymbol(t *testing.T) {
	c := qt.New(t)
	v := NewSymbol("foo")
	c.Assert(v.SchemeString(), qt.Equals, "foo")
}

func TestNewBoolean(t *testing.T) {
	c := qt.New(t)
	c.Assert(NewBoolean(true).SchemeString(), qt.Equals, "#t")
	c.Assert(NewBoolean(false).SchemeString(), qt.Equals, "#f")
}

func TestNewList(t *testing.T) {
	c := qt.New(t)

	t.Run("empty list", func(t *testing.T) {
		v := NewList()
		c.Assert(v.SchemeString(), qt.Equals, "()")
	})

	t.Run("non-empty list", func(t *testing.T) {
		v := NewList(NewInteger(1), NewInteger(2), NewInteger(3))
		c.Assert(v.SchemeString(), qt.Equals, "(1 2 3)")
	})
}

func TestNewBigInteger(t *testing.T) {
	c := qt.New(t)
	bigInt := big.NewInt(123456789)
	v := NewBigInteger(bigInt)
	c.Assert(v.SchemeString(), qt.Equals, "123456789")
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewBigIntegerFromInt64(t *testing.T) {
	c := qt.New(t)
	v := NewBigIntegerFromInt64(9223372036854775807)
	c.Assert(v.SchemeString(), qt.Equals, "9223372036854775807")
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewBigIntegerFromString(t *testing.T) {
	c := qt.New(t)

	t.Run("valid base 10", func(t *testing.T) {
		v := NewBigIntegerFromString("123456789012345678901234567890", 10)
		c.Assert(v, qt.IsNotNil)
		c.Assert(v.SchemeString(), qt.Equals, "123456789012345678901234567890")
	})

	t.Run("valid base 16", func(t *testing.T) {
		v := NewBigIntegerFromString("DEADBEEF", 16)
		c.Assert(v, qt.IsNotNil)
		c.Assert(v.SchemeString(), qt.Equals, "3735928559")
	})

	t.Run("invalid string", func(t *testing.T) {
		v := NewBigIntegerFromString("not a number", 10)
		c.Assert(v, qt.IsNil)
	})
}

func TestNewBigFloat(t *testing.T) {
	c := qt.New(t)
	bigFloat := big.NewFloat(3.141592653589793)
	v := NewBigFloat(bigFloat)
	c.Assert(v, qt.IsNotNil)
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewBigFloatFromFloat64(t *testing.T) {
	c := qt.New(t)
	v := NewBigFloatFromFloat64(2.718281828459045)
	c.Assert(v, qt.IsNotNil)
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewBigFloatFromString(t *testing.T) {
	c := qt.New(t)

	t.Run("valid float string", func(t *testing.T) {
		v := NewBigFloatFromString("1.23456789012345678901234567890")
		c.Assert(v, qt.IsNotNil)
	})

	t.Run("scientific notation", func(t *testing.T) {
		v := NewBigFloatFromString("1.23e10")
		c.Assert(v, qt.IsNotNil)
	})

	t.Run("invalid string", func(t *testing.T) {
		v := NewBigFloatFromString("not a float")
		c.Assert(v, qt.IsNil)
	})
}

// Value methods

func TestValue_IsVoid(t *testing.T) {
	c := qt.New(t)
	c.Assert(Void.IsVoid(), qt.IsTrue)
	c.Assert(NewInteger(1).IsVoid(), qt.IsFalse)
	c.Assert(Null.IsVoid(), qt.IsFalse)
}

func TestValue_Internal(t *testing.T) {
	c := qt.New(t)
	v := NewInteger(42)
	c.Assert(v.Internal(), qt.IsNotNil)
	c.Assert(v.Internal().SchemeString(), qt.Equals, "42")
}

// Engine accessors

func TestEngine_Environment(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine()
	c.Assert(err, qt.IsNil)
	c.Assert(engine.Environment(), qt.IsNotNil)
}

func TestEngine_TopLevelEnvironment(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine()
	c.Assert(err, qt.IsNil)
	c.Assert(engine.TopLevelEnvironment(), qt.IsNotNil)
}

// Error type

func TestError_Error(t *testing.T) {
	c := qt.New(t)

	t.Run("with cause", func(t *testing.T) {
		e := &Error{Message: "failed", Cause: errors.New("bad input")}
		c.Assert(e.Error(), qt.Equals, "failed: bad input")
	})

	t.Run("without cause", func(t *testing.T) {
		e := &Error{Message: "failed"}
		c.Assert(e.Error(), qt.Equals, "failed")
	})
}

func TestError_Unwrap(t *testing.T) {
	c := qt.New(t)
	cause := errors.New("root cause")
	e := &Error{Message: "wrapper", Cause: cause}
	c.Assert(e.Unwrap(), qt.Equals, cause)

	e2 := &Error{Message: "no cause"}
	c.Assert(e2.Unwrap(), qt.IsNil)
}

// CompiledCode.String

func TestCompiledCode_String(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine()
	c.Assert(err, qt.IsNil)

	compiled, err := engine.Compile("(+ 1 2)")
	c.Assert(err, qt.IsNil)
	c.Assert(compiled.String(), qt.Equals, "#<compiled-code>")
}

// WithRegistry option

func TestWithRegistry(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	// Engine with empty registry — should still create
	engine, err := NewEngine(WithRegistry(reg))
	c.Assert(err, qt.IsNil)
	c.Assert(engine, qt.IsNotNil)
}

// WithExtensions option

func TestWithExtensions(t *testing.T) {
	c := qt.New(t)
	// WithExtensions with no extensions should work fine
	engine, err := NewEngine(WithExtensions())
	c.Assert(err, qt.IsNil)
	c.Assert(engine, qt.IsNotNil)
}
