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
	"bytes"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func toBytes(bv *values.ByteVector) []byte {
	out := make([]byte, len(*bv))
	for i, v := range *bv {
		out[i] = v.Value
	}
	return out
}

func TestBytevectorInputPort_NewBytevectorInputPort(t *testing.T) {
	bv := values.NewByteVectorFromBytes(1, 2, 3)
	port := values.NewByteVectorInputPortFromReader(bytes.NewBuffer(toBytes(bv)))
	qt.Assert(t, port, qt.Not(qt.IsNil))
}

func TestBytevectorInputPort_Read(t *testing.T) {
	bv := values.NewByteVectorFromBytes(1, 2, 3, 4, 5)
	port := values.NewByteVectorInputPortFromReader(bytes.NewBuffer(toBytes(bv)))

	buf := make([]byte, 3)
	n, err := port.Read(buf)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, n, qt.Equals, 3)
	qt.Assert(t, buf, qt.DeepEquals, []byte{1, 2, 3})
}

func TestBytevectorInputPort_ReadByte(t *testing.T) {
	bv := values.NewByteVectorFromBytes(42, 99)
	port := values.NewByteVectorInputPortFromReader(bytes.NewBuffer(toBytes(bv)))

	b1, err := port.ReadByte()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, b1, qt.Equals, values.NewByte(42).Value)

	b2, err := port.ReadByte()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, b2, qt.Equals, values.NewByte(99).Value)
}

func TestBytevectorInputPort_UnreadByte(t *testing.T) {
	bv := values.NewByteVectorFromBytes(1, 2)
	port := values.NewByteVectorInputPortFromReader(bytes.NewBuffer(toBytes(bv)))

	b1, _ := port.ReadByte()
	qt.Assert(t, b1, qt.Equals, values.NewByte(1).Value)

	err := port.UnreadByte()
	qt.Assert(t, err, qt.IsNil)

	b2, _ := port.ReadByte()
	qt.Assert(t, b2, qt.Equals, values.NewByte(1).Value)
}

func TestBytevectorInputPort_IsVoid(t *testing.T) {
	port := values.NewByteVectorInputPortFromReader(bytes.NewBuffer(toBytes(values.NewByteVectorFromBytes(1))))
	qt.Assert(t, port.IsVoid(), qt.IsFalse)

	var nilPort *values.ByteVectorInputPort
	qt.Assert(t, nilPort.IsVoid(), qt.IsTrue)
}

func TestBytevectorInputPort_Datum(t *testing.T) {
	port := values.NewByteVectorInputPortFromReader(bytes.NewBuffer(toBytes(values.NewByteVectorFromBytes(1))))
	datum := port.Datum()
	qt.Assert(t, datum, qt.Not(qt.IsNil))
}

func TestBytevectorInputPort_EqualTo(t *testing.T) {
	port1 := values.NewByteVectorInputPortFromReader(bytes.NewBuffer(toBytes(values.NewByteVectorFromBytes(1))))
	port2 := values.NewByteVectorInputPortFromReader(bytes.NewBuffer(toBytes(values.NewByteVectorFromBytes(1))))
	qt.Assert(t, port1.EqualTo(port2), qt.IsFalse)

	qt.Assert(t, port1.EqualTo(port1), qt.IsTrue)
}

func TestBytevectorInputPort_SchemeString(t *testing.T) {
	port := values.NewByteVectorInputPortFromReader(bytes.NewBuffer(toBytes(values.NewByteVectorFromBytes(1))))
	s := port.SchemeString()
	qt.Assert(t, strings.Contains(s, "bytevector-input-port"), qt.IsTrue)
}

func TestBytevectorOutputPort_NewBytevectorOutputPort(t *testing.T) {
	port := values.NewByteVectorOutputPortFromWriter(bytes.NewBuffer(nil))
	qt.Assert(t, port, qt.Not(qt.IsNil))
}

func TestBytevectorOutputPort_Write(t *testing.T) {
	buf := bytes.NewBuffer(nil)
	port := values.NewByteVectorOutputPortFromWriter(buf)
	n, err := port.Write(values.NewByteVectorFromBytes(1, 2, 3).AsBytes())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, n, qt.Equals, 3)
	port.Flush()

	bv := buf.Bytes()
	qt.Assert(t, bv, qt.DeepEquals, values.NewByteVectorFromBytes(1, 2, 3).AsBytes())
}

func TestBytevectorOutputPort_WriteByte(t *testing.T) {
	buf := bytes.NewBuffer(nil)
	port := values.NewByteVectorOutputPortFromWriter(buf)
	err := port.WriteByte(values.NewByte(42).Value)
	qt.Assert(t, err, qt.IsNil)
	port.Flush()

	bv := buf.Bytes()
	qt.Assert(t, bv, qt.DeepEquals, values.NewByteVectorFromBytes(42).AsBytes())
}

func TestBytevectorOutputPort_GetBytevector(t *testing.T) {
	buf := bytes.NewBuffer(nil)
	port := values.NewByteVectorOutputPortFromWriter(buf)
	// write using helpers; ignore errors where intended
	_, _ = port.Write(values.NewByteVectorFromBytes(1, 2).AsBytes())
	_ = port.WriteByte(values.NewByte(3).Value)
	_, _ = port.Write(values.NewByteVectorFromBytes(4, 5).AsBytes())
	_ = port.Flush()

	bv := buf.Bytes()
	qt.Assert(t, bv, qt.DeepEquals, []byte{1, 2, 3, 4, 5})
}

func TestBytevectorOutputPort_IsVoid(t *testing.T) {
	buf := bytes.NewBuffer(nil)
	port := values.NewByteVectorOutputPortFromWriter(buf)
	qt.Assert(t, port.IsVoid(), qt.IsFalse)

	var nilPort *values.ByteVectorOutputPort
	qt.Assert(t, nilPort.IsVoid(), qt.IsTrue)
}

func TestBytevectorOutputPort_Datum(t *testing.T) {
	buf := bytes.NewBuffer(nil)
	port := values.NewByteVectorOutputPortFromWriter(buf)
	datum := port.Datum()
	qt.Assert(t, datum, qt.Not(qt.IsNil))
}

func TestBytevectorOutputPort_EqualTo(t *testing.T) {
	port1 := values.NewByteVectorOutputPortFromWriter(bytes.NewBuffer(nil))
	port2 := values.NewByteVectorOutputPortFromWriter(bytes.NewBuffer(nil))
	qt.Assert(t, port1.EqualTo(port2), qt.IsFalse)
	qt.Assert(t, port1.EqualTo(port1), qt.IsTrue)
}

func TestBytevectorOutputPort_SchemeString(t *testing.T) {
	port := values.NewByteVectorOutputPortFromWriter(bytes.NewBuffer(nil))
	s := port.SchemeString()
	qt.Assert(t, strings.Contains(s, "bytevector-output-port"), qt.IsTrue)
}
