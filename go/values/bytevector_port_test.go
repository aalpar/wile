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
	qt "github.com/frankban/quicktest"
	"strings"
	"testing"
)

func TestBytevectorInputPort_NewBytevectorInputPort(t *testing.T) {
	bv := []byte{1, 2, 3}
	port := NewBytevectorInputPort(bv)
	qt.Assert(t, port, qt.Not(qt.IsNil))
}

func TestBytevectorInputPort_Read(t *testing.T) {
	bv := []byte{1, 2, 3, 4, 5}
	port := NewBytevectorInputPort(bv)

	buf := make([]byte, 3)
	n, err := port.Read(buf)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, n, qt.Equals, 3)
	qt.Assert(t, buf, qt.DeepEquals, []byte{1, 2, 3})
}

func TestBytevectorInputPort_ReadByte(t *testing.T) {
	bv := []byte{42, 99}
	port := NewBytevectorInputPort(bv)

	b1, err := port.ReadByte()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, b1, qt.Equals, byte(42))

	b2, err := port.ReadByte()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, b2, qt.Equals, byte(99))
}

func TestBytevectorInputPort_UnreadByte(t *testing.T) {
	bv := []byte{1, 2}
	port := NewBytevectorInputPort(bv)

	b1, _ := port.ReadByte()
	qt.Assert(t, b1, qt.Equals, byte(1))

	err := port.UnreadByte()
	qt.Assert(t, err, qt.IsNil)

	b2, _ := port.ReadByte()
	qt.Assert(t, b2, qt.Equals, byte(1))
}

func TestBytevectorInputPort_IsVoid(t *testing.T) {
	port := NewBytevectorInputPort([]byte{1})
	qt.Assert(t, port.IsVoid(), qt.IsFalse)

	var nilPort *BytevectorInputPort
	qt.Assert(t, nilPort.IsVoid(), qt.IsTrue)
}

func TestBytevectorInputPort_Datum(t *testing.T) {
	port := NewBytevectorInputPort([]byte{1})
	datum := port.Datum()
	qt.Assert(t, datum, qt.Not(qt.IsNil))
}

func TestBytevectorInputPort_EqualTo(t *testing.T) {
	port1 := NewBytevectorInputPort([]byte{1})
	port2 := NewBytevectorInputPort([]byte{1})
	qt.Assert(t, port1.EqualTo(port2), qt.IsFalse)

	qt.Assert(t, port1.EqualTo(port1), qt.IsTrue)
}

func TestBytevectorInputPort_SchemeString(t *testing.T) {
	port := NewBytevectorInputPort([]byte{1})
	s := port.SchemeString()
	qt.Assert(t, strings.Contains(s, "bytevector-input-port"), qt.IsTrue)
}

func TestBytevectorOutputPort_NewBytevectorOutputPort(t *testing.T) {
	port := NewBytevectorOutputPort()
	qt.Assert(t, port, qt.Not(qt.IsNil))
}

func TestBytevectorOutputPort_Write(t *testing.T) {
	port := NewBytevectorOutputPort()
	n, err := port.Write([]byte{1, 2, 3})
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, n, qt.Equals, 3)

	bv := port.GetBytevector()
	qt.Assert(t, bv, qt.DeepEquals, []byte{1, 2, 3})
}

func TestBytevectorOutputPort_WriteByte(t *testing.T) {
	port := NewBytevectorOutputPort()
	err := port.WriteByte(42)
	qt.Assert(t, err, qt.IsNil)

	bv := port.GetBytevector()
	qt.Assert(t, bv, qt.DeepEquals, []byte{42})
}

func TestBytevectorOutputPort_GetBytevector(t *testing.T) {
	port := NewBytevectorOutputPort()
	port.Write([]byte{1, 2})   //nolint:errcheck
	port.WriteByte(3)          //nolint:errcheck
	port.Write([]byte{4, 5})   //nolint:errcheck

	bv := port.GetBytevector()
	qt.Assert(t, bv, qt.DeepEquals, []byte{1, 2, 3, 4, 5})
}

func TestBytevectorOutputPort_IsVoid(t *testing.T) {
	port := NewBytevectorOutputPort()
	qt.Assert(t, port.IsVoid(), qt.IsFalse)

	var nilPort *BytevectorOutputPort
	qt.Assert(t, nilPort.IsVoid(), qt.IsTrue)
}

func TestBytevectorOutputPort_Datum(t *testing.T) {
	port := NewBytevectorOutputPort()
	datum := port.Datum()
	qt.Assert(t, datum, qt.Not(qt.IsNil))
}

func TestBytevectorOutputPort_EqualTo(t *testing.T) {
	port1 := NewBytevectorOutputPort()
	port2 := NewBytevectorOutputPort()
	qt.Assert(t, port1.EqualTo(port2), qt.IsFalse)

	qt.Assert(t, port1.EqualTo(port1), qt.IsTrue)
}

func TestBytevectorOutputPort_SchemeString(t *testing.T) {
	port := NewBytevectorOutputPort()
	s := port.SchemeString()
	qt.Assert(t, strings.Contains(s, "bytevector-output-port"), qt.IsTrue)
}
