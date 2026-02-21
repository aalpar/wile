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
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

// NativeError comprehensive coverage

func TestNativeError_NewNativeError(t *testing.T) {
	c := qt.New(t)
	err := values.NewNativeError("something went wrong")
	c.Assert(err.Message().Datum(), qt.Equals, "something went wrong")
	c.Assert(err.Irritants().IsVoid(), qt.IsFalse) // EmptyList is not void
	c.Assert(err.Kind(), qt.Equals, values.NativeErrorKindGeneric)
	c.Assert(err.Error(), qt.Equals, "something went wrong")
}

func TestNativeError_NewErrorObject(t *testing.T) {
	c := qt.New(t)
	err := values.NewErrorObject("bad value", values.NewInteger(42), values.NewString("extra"))
	c.Assert(err.Message().Datum(), qt.Equals, "bad value")
	c.Assert(err.Kind(), qt.Equals, values.NativeErrorKindGeneric)
}

func TestNativeError_NewErrorObjectWithCause(t *testing.T) {
	c := qt.New(t)
	cause := errors.New("root cause")
	err := values.NewErrorObjectWithCause("wrapped", cause, values.NewInteger(1))
	c.Assert(err.Datum(), qt.Equals, cause)
	c.Assert(err.Unwrap(), qt.Equals, cause)
	c.Assert(err.Error(), qt.Equals, "wrapped")
}

func TestNativeError_IsVoid(t *testing.T) {
	c := qt.New(t)
	err := values.NewNativeError("test")
	c.Assert(err.IsVoid(), qt.IsFalse)

	var nilErr *values.NativeError
	c.Assert(nilErr.IsVoid(), qt.IsTrue)
}

func TestNativeError_SchemeString(t *testing.T) {
	c := qt.New(t)
	err := values.NewNativeError("hello")
	c.Assert(err.SchemeString(), qt.Equals, `#<error-object "hello">`)

	var nilErr *values.NativeError
	c.Assert(nilErr.SchemeString(), qt.Equals, "#<error-object>")
}

func TestNativeError_EqualTo_Coverage(t *testing.T) {
	c := qt.New(t)

	err1 := values.NewErrorObject("msg", values.NewInteger(1))
	err2 := values.NewErrorObject("msg", values.NewInteger(1))
	err3 := values.NewErrorObject("other", values.NewInteger(1))
	err4 := values.NewReadError("msg", values.NewInteger(1))

	c.Assert(err1.EqualTo(err1), qt.IsTrue)
	c.Assert(err1.EqualTo(err2), qt.IsTrue)
	c.Assert(err1.EqualTo(err3), qt.IsFalse) // different message
	c.Assert(err1.EqualTo(err4), qt.IsFalse) // different kind
	c.Assert(err1.EqualTo(values.NewInteger(1)), qt.IsFalse)

	var nilErr *values.NativeError
	c.Assert(nilErr.EqualTo(nilErr), qt.IsTrue)
}

func TestNativeError_NilReceiver(t *testing.T) {
	c := qt.New(t)
	var nilErr *values.NativeError
	c.Assert(nilErr.Message(), qt.IsNil)
	c.Assert(nilErr.Kind(), qt.Equals, values.NativeErrorKindGeneric)
	c.Assert(nilErr.Datum(), qt.IsNil)
	c.Assert(nilErr.Unwrap(), qt.IsNil)
	c.Assert(nilErr.Error(), qt.Equals, "")
	c.Assert(nilErr.IsReadError(), qt.IsFalse)
	c.Assert(nilErr.IsFileError(), qt.IsFalse)
}

func TestNativeError_Irritants(t *testing.T) {
	c := qt.New(t)
	err := values.NewErrorObject("msg", values.NewInteger(1), values.NewString("two"))
	irr := err.Irritants()
	c.Assert(irr, qt.IsNotNil)

	var nilErr *values.NativeError
	c.Assert(nilErr.Irritants(), qt.Equals, values.EmptyList)
}
