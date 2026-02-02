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
	"bytes"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestStringInputOutputPort_NewStringInputPort(t *testing.T) {
	port := NewStringInputPortWithBuffer(bytes.NewBufferString("hello"))
	qt.Assert(t, port, qt.Not(qt.IsNil))
}

func TestStringInputOutputPort_ReadRune(t *testing.T) {
	port := NewStringInputPortWithBuffer(bytes.NewBufferString("abc"))
	r1, _, err := port.ReadRune()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, r1, qt.Equals, 'a')

	r2, _, err := port.ReadRune()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, r2, qt.Equals, 'b')
}

func TestStringInputOutputPort_UnreadRune(t *testing.T) {
	port := NewStringInputPortWithBuffer(bytes.NewBufferString("abc"))
	r1, _, _ := port.ReadRune()
	qt.Assert(t, r1, qt.Equals, 'a')

	err := port.UnreadRune()
	qt.Assert(t, err, qt.IsNil)

	r2, _, _ := port.ReadRune()
	qt.Assert(t, r2, qt.Equals, 'a')
}

func TestStringInputOutputPort_IsVoid(t *testing.T) {
	port := NewStringOutputPortWithBuffer(bytes.NewBufferString("test"))
	qt.Assert(t, port.IsVoid(), qt.IsFalse)

	var nilPort *StringOutputPort
	qt.Assert(t, nilPort.IsVoid(), qt.IsTrue)
}

func TestStringInputOutputPort_Datum(t *testing.T) {
	port := NewStringOutputPortWithBuffer(bytes.NewBufferString("test"))
	datum := port.Datum()
	qt.Assert(t, datum, qt.Not(qt.IsNil))
}

func TestStringInputOutputPort_EqualTo(t *testing.T) {
	port1 := NewStringOutputPortWithBuffer(bytes.NewBufferString("test"))
	port2 := NewStringOutputPortWithBuffer(bytes.NewBufferString("test"))
	qt.Assert(t, port1.EqualTo(port2), qt.IsFalse)

	qt.Assert(t, port1.EqualTo(port1), qt.IsTrue)
}

func TestStringInputOutputPort_SchemeString(t *testing.T) {
	port := NewStringOutputPortWithBuffer(bytes.NewBufferString("test"))
	s := port.SchemeString()
	qt.Assert(t, strings.Contains(s, "string-input-output-port"), qt.IsTrue)
}

func TestStringInputOutputPort_NewStringOutputPort(t *testing.T) {
	port := NewStringOutputPort()
	qt.Assert(t, port, qt.Not(qt.IsNil))
}

func TestStringInputOutputPort_Write(t *testing.T) {
	port := NewStringOutputPort()
	n, err := port.WriteString("hello")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, n, qt.Equals, 5)
	port.Flush()

	s := port.Datum().String()
	qt.Assert(t, s, qt.Equals, "hello")
}

func TestStringInputOutputPort_GetString(t *testing.T) {
	port := NewStringOutputPort()
	port.WriteString("hello") //nolint:errcheck
	port.WriteString(" ")     //nolint:errcheck
	port.WriteString("world") //nolint:errcheck
	port.Flush()

	s := port.Datum().String()
	qt.Assert(t, s, qt.Equals, "hello world")
}
