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

package helpers

import (
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

func TestRequireType_Success_ConcretePointer(t *testing.T) {
	c := qt.New(t)
	v := values.NewVector(values.NewInteger(1), values.NewInteger(2))
	result, err := RequireType[*values.Vector](v, werr.ErrNotAVector, "vector-length")
	c.Assert(err, qt.IsNil)
	c.Assert(result.Length(), qt.Equals, 2)
}

func TestRequireType_Success_String(t *testing.T) {
	c := qt.New(t)
	v := values.NewString("hello")
	result, err := RequireType[*values.String](v, werr.ErrNotAString, "string-length")
	c.Assert(err, qt.IsNil)
	c.Assert(result.Value, qt.Equals, "hello")
}

func TestRequireType_Success_Integer(t *testing.T) {
	c := qt.New(t)
	v := values.NewInteger(42)
	result, err := RequireType[*values.Integer](v, werr.ErrNotAnInteger, "exact")
	c.Assert(err, qt.IsNil)
	c.Assert(result.Value, qt.Equals, int64(42))
}

func TestRequireType_Success_Interface(t *testing.T) {
	c := qt.New(t)
	v := values.NewInteger(42)
	result, err := RequireType[values.Number](v, werr.ErrNotANumber, "add")
	c.Assert(err, qt.IsNil)
	c.Assert(result.IsExact(), qt.IsTrue)
}

func TestRequireType_Failure_WrongType(t *testing.T) {
	c := qt.New(t)
	v := values.NewInteger(42)
	_, err := RequireType[*values.Vector](v, werr.ErrNotAVector, "vector-length")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotAVector), qt.IsTrue)
}

func TestRequireType_Failure_ErrorMessage_Vector(t *testing.T) {
	c := qt.New(t)
	v := values.NewInteger(42)
	_, err := RequireType[*values.Vector](v, werr.ErrNotAVector, "vector-length")
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Matches, `vector-length: expected a vector but got \*values\.Integer.*`)
}

func TestRequireType_Failure_ErrorMessage_Integer(t *testing.T) {
	c := qt.New(t)
	v := values.NewString("hello")
	_, err := RequireType[*values.Integer](v, werr.ErrNotAnInteger, "exact")
	c.Assert(err, qt.IsNotNil)
	// ErrNotAnInteger.Error() = "not an integer" → TrimPrefix("not ") = "an integer"
	c.Assert(err.Error(), qt.Matches, `exact: expected an integer but got \*values\.String.*`)
}

func TestRequireType_Failure_ErrorMessage_ByteVector(t *testing.T) {
	c := qt.New(t)
	v := values.NewInteger(1)
	_, err := RequireType[*values.ByteVector](v, werr.ErrNotAByteVector, "bytevector-length")
	c.Assert(err, qt.IsNotNil)
	// ErrNotAByteVector.Error() = "not a bytevector" → TrimPrefix("not ") = "a bytevector"
	c.Assert(err.Error(), qt.Matches, `bytevector-length: expected a bytevector but got \*values\.Integer.*`)
}

func TestRequireType_Failure_NilValue(t *testing.T) {
	c := qt.New(t)
	_, err := RequireType[*values.Vector](nil, werr.ErrNotAVector, "vector-length")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotAVector), qt.IsTrue)
}

func TestRequireType_Failure_SentinelPreserved(t *testing.T) {
	c := qt.New(t)
	sentinels := []error{
		werr.ErrNotAVector,
		werr.ErrNotAString,
		werr.ErrNotAnInteger,
		werr.ErrNotACharacter,
		werr.ErrNotAPair,
		werr.ErrNotANumber,
		werr.ErrNotAByteVector,
		werr.ErrNotAHashtable,
		werr.ErrNotAProcedure,
		werr.ErrNotABox,
	}
	v := values.TrueValue
	for _, sentinel := range sentinels {
		_, err := RequireType[*values.Vector](v, sentinel, "test")
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, sentinel), qt.IsTrue, qt.Commentf("sentinel: %v", sentinel))
	}
}
