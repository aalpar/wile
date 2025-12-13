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

package values

import (
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestStringInputPort_NewStringInputPort(t *testing.T) {
	port := NewStringInputPort("hello")
	qt.Assert(t, port, qt.Not(qt.IsNil))
}

func TestStringInputPort_ReadRune(t *testing.T) {
	port := NewStringInputPort("abc")
	r1, _, err := port.ReadRune()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, r1, qt.Equals, 'a')

	r2, _, err := port.ReadRune()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, r2, qt.Equals, 'b')
}

func TestStringInputPort_UnreadRune(t *testing.T) {
	port := NewStringInputPort("abc")
	r1, _, _ := port.ReadRune()
	qt.Assert(t, r1, qt.Equals, 'a')

	err := port.UnreadRune()
	qt.Assert(t, err, qt.IsNil)

	r2, _, _ := port.ReadRune()
	qt.Assert(t, r2, qt.Equals, 'a')
}

func TestStringInputPort_IsVoid(t *testing.T) {
	port := NewStringInputPort("test")
	qt.Assert(t, port.IsVoid(), qt.IsFalse)

	var nilPort *StringInputPort
	qt.Assert(t, nilPort.IsVoid(), qt.IsTrue)
}

func TestStringInputPort_Datum(t *testing.T) {
	port := NewStringInputPort("test")
	datum := port.Datum()
	qt.Assert(t, datum, qt.Not(qt.IsNil))
}

func TestStringInputPort_EqualTo(t *testing.T) {
	port1 := NewStringInputPort("test")
	port2 := NewStringInputPort("test")
	qt.Assert(t, port1.EqualTo(port2), qt.IsFalse)

	qt.Assert(t, port1.EqualTo(port1), qt.IsTrue)
}

func TestStringInputPort_SchemeString(t *testing.T) {
	port := NewStringInputPort("test")
	s := port.SchemeString()
	qt.Assert(t, strings.Contains(s, "string-input-port"), qt.IsTrue)
}

func TestStringOutputPort_NewStringOutputPort(t *testing.T) {
	port := NewStringOutputPort()
	qt.Assert(t, port, qt.Not(qt.IsNil))
}

func TestStringOutputPort_Write(t *testing.T) {
	port := NewStringOutputPort()
	n, err := port.Write([]byte("hello"))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, n, qt.Equals, 5)

	s := port.GetString()
	qt.Assert(t, s, qt.Equals, "hello")
}

func TestStringOutputPort_GetString(t *testing.T) {
	port := NewStringOutputPort()
	port.Write([]byte("hello")) //nolint:errcheck
	port.Write([]byte(" "))     //nolint:errcheck
	port.Write([]byte("world")) //nolint:errcheck

	s := port.GetString()
	qt.Assert(t, s, qt.Equals, "hello world")
}

func TestStringOutputPort_IsVoid(t *testing.T) {
	port := NewStringOutputPort()
	qt.Assert(t, port.IsVoid(), qt.IsFalse)

	var nilPort *StringOutputPort
	qt.Assert(t, nilPort.IsVoid(), qt.IsTrue)
}

func TestStringOutputPort_Datum(t *testing.T) {
	port := NewStringOutputPort()
	datum := port.Datum()
	qt.Assert(t, datum, qt.Not(qt.IsNil))
}

func TestStringOutputPort_EqualTo(t *testing.T) {
	port1 := NewStringOutputPort()
	port2 := NewStringOutputPort()
	qt.Assert(t, port1.EqualTo(port2), qt.IsFalse)

	qt.Assert(t, port1.EqualTo(port1), qt.IsTrue)
}

func TestStringOutputPort_SchemeString(t *testing.T) {
	port := NewStringOutputPort()
	s := port.SchemeString()
	qt.Assert(t, strings.Contains(s, "string-output-port"), qt.IsTrue)
}
