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

// ByteVectorInputOutputPort

func TestByteVectorInputOutputPort_Basic(t *testing.T) {
	c := qt.New(t)
	port := values.NewByteVectorInputOutputPort()
	c.Assert(port.IsVoid(), qt.IsFalse)
	c.Assert(port.SchemeString(), qt.Matches, ".*port.*")
	c.Assert(port.IsClosed(), qt.IsFalse)

	// Write and read
	n, err := port.Write([]byte{1, 2, 3})
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 3)

	buf := make([]byte, 3)
	n, err = port.Read(buf)
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 3)
	c.Assert(buf, qt.DeepEquals, []byte{1, 2, 3})

	// EqualTo
	port2 := values.NewByteVectorInputOutputPort()
	c.Assert(port.EqualTo(port), qt.IsTrue)
	c.Assert(port.EqualTo(port2), qt.IsFalse)
	c.Assert(port.EqualTo(values.NewInteger(1)), qt.IsFalse)

	// Close
	err = port.Close()
	c.Assert(err, qt.IsNil)
	c.Assert(port.IsClosed(), qt.IsTrue)
}

func TestByteVectorInputOutputPort_FromBuffer(t *testing.T) {
	c := qt.New(t)
	buf := bytes.NewBuffer([]byte{10, 20, 30})
	port := values.NewByteVectorInputOutputPortFromBuffer(buf)
	c.Assert(port, qt.IsNotNil)

	b := make([]byte, 3)
	n, err := port.Read(b)
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 3)
	c.Assert(b, qt.DeepEquals, []byte{10, 20, 30})
}

func TestByteVectorInputOutputPort_ByteOps(t *testing.T) {
	c := qt.New(t)
	port := values.NewByteVectorInputOutputPort()

	err := port.WriteByte(42)
	c.Assert(err, qt.IsNil)

	b, err := port.ReadByte()
	c.Assert(err, qt.IsNil)
	c.Assert(b, qt.Equals, byte(42))
}

func TestByteVectorInputOutputPort_ReadByteVector(t *testing.T) {
	c := qt.New(t)
	port := values.NewByteVectorInputOutputPort()
	_, err := port.Write([]byte{1, 2, 3})
	c.Assert(err, qt.IsNil)

	bv, err2 := port.ReadByteVector()
	c.Assert(err2, qt.IsNil)
	c.Assert(bv, qt.IsNotNil)
}

func TestByteVectorInputOutputPort_Flush(t *testing.T) {
	c := qt.New(t)
	port := values.NewByteVectorInputOutputPort()
	err := port.Flush()
	c.Assert(err, qt.IsNil)
}

// ByteVectorBufferedOutputPort (note: "Bufferd" is the actual spelling)

func TestByteVectorBufferedOutputPort_Basic(t *testing.T) {
	c := qt.New(t)
	port := values.NewByteVectorBufferedOutputPort()
	c.Assert(port.IsVoid(), qt.IsFalse)
	c.Assert(port.SchemeString(), qt.Matches, ".*port.*")
	c.Assert(port.IsClosed(), qt.IsFalse)

	// Write
	n, err := port.Write([]byte{4, 5, 6})
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 3)

	// WriteByte
	err = port.WriteByte(7)
	c.Assert(err, qt.IsNil)

	// Flush
	err = port.Flush()
	c.Assert(err, qt.IsNil)

	// ReadByteVector
	bv, err2 := port.ReadByteVector()
	c.Assert(err2, qt.IsNil)
	c.Assert(bv, qt.IsNotNil)

	// EqualTo
	port2 := values.NewByteVectorBufferedOutputPort()
	c.Assert(port.EqualTo(port), qt.IsTrue)
	c.Assert(port.EqualTo(port2), qt.IsFalse)
	c.Assert(port.EqualTo(values.NewInteger(1)), qt.IsFalse)

	// Close
	err = port.Close()
	c.Assert(err, qt.IsNil)
	c.Assert(port.IsClosed(), qt.IsTrue)
}

func TestByteVectorBufferedOutputPort_FromBuffer(t *testing.T) {
	c := qt.New(t)
	buf := &bytes.Buffer{}
	port := values.NewByteVectorBufferedOutputPortFromBuffer(buf)
	c.Assert(port, qt.IsNotNil)

	_, err := port.Write([]byte{1})
	c.Assert(err, qt.IsNil)
}

// StringInputPort

func TestStringInputPort_Basic(t *testing.T) {
	c := qt.New(t)
	port := values.NewStringInputPortWithBuffer(bytes.NewBufferString("hello world"))
	c.Assert(port.IsVoid(), qt.IsFalse)
	c.Assert(port.SchemeString(), qt.Matches, ".*port.*")
	c.Assert(port.IsClosed(), qt.IsFalse)

	// Read
	buf := make([]byte, 5)
	n, err := port.Read(buf)
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 5)
	c.Assert(string(buf), qt.Equals, "hello")

	// ReadRune
	r, _, err := port.ReadRune()
	c.Assert(err, qt.IsNil)
	c.Assert(r, qt.Equals, ' ')

	// UnreadRune
	err = port.UnreadRune()
	c.Assert(err, qt.IsNil)

	// EqualTo
	port2 := values.NewStringInputPortWithBuffer(bytes.NewBufferString("hello"))
	c.Assert(port.EqualTo(port), qt.IsTrue)
	c.Assert(port.EqualTo(port2), qt.IsFalse)
	c.Assert(port.EqualTo(values.NewInteger(1)), qt.IsFalse)

	// Flush and Close
	err = port.Flush()
	c.Assert(err, qt.IsNil)
	err = port.Close()
	c.Assert(err, qt.IsNil)
	c.Assert(port.IsClosed(), qt.IsTrue)
}

// StringOutputPort

func TestStringOutputPort_Basic(t *testing.T) {
	c := qt.New(t)
	port := values.NewStringOutputPort()
	c.Assert(port.IsVoid(), qt.IsFalse)
	c.Assert(port.SchemeString(), qt.Matches, ".*port.*")
	c.Assert(port.IsClosed(), qt.IsFalse)

	// WriteString
	n, err := port.WriteString("hi")
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 2)

	// WriteString
	n, err = port.WriteString(" there")
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 6)

	// WriteRune
	n, err = port.WriteRune('!')
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 1)

	// String
	c.Assert(port.String(), qt.Equals, "hi there!")

	// Flush
	err = port.Flush()
	c.Assert(err, qt.IsNil)

	// EqualTo
	port2 := values.NewStringOutputPort()
	c.Assert(port.EqualTo(port), qt.IsTrue)
	c.Assert(port.EqualTo(port2), qt.IsFalse)
	c.Assert(port.EqualTo(values.NewInteger(1)), qt.IsFalse)

	// Close
	err = port.Close()
	c.Assert(err, qt.IsNil)
	c.Assert(port.IsClosed(), qt.IsTrue)
}

// CharacterInputPort

func TestCharacterInputPort_Basic(t *testing.T) {
	c := qt.New(t)
	port := values.NewCharacterInputPortFromReader(strings.NewReader("abc"))
	c.Assert(port.IsVoid(), qt.IsFalse)
	c.Assert(port.SchemeString(), qt.Matches, ".*port.*")
	c.Assert(port.IsClosed(), qt.IsFalse)

	// ReadRune
	r, _, err := port.ReadRune()
	c.Assert(err, qt.IsNil)
	c.Assert(r, qt.Equals, 'a')

	// UnreadRune
	err = port.UnreadRune()
	c.Assert(err, qt.IsNil)

	// Read
	buf := make([]byte, 3)
	n, err := port.Read(buf)
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 3)

	// EqualTo
	port2 := values.NewCharacterInputPortFromReader(strings.NewReader("def"))
	c.Assert(port.EqualTo(port), qt.IsTrue)
	c.Assert(port.EqualTo(port2), qt.IsFalse)
	c.Assert(port.EqualTo(values.NewInteger(1)), qt.IsFalse)

	// Close
	err = port.Close()
	c.Assert(err, qt.IsNil)
	c.Assert(port.IsClosed(), qt.IsTrue)
}

// CharacterOutputPort

func TestCharacterOutputPort_Basic(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	port := values.NewCharacterOutputPortFromWriter(&buf)
	c.Assert(port.IsVoid(), qt.IsFalse)
	c.Assert(port.SchemeString(), qt.Matches, ".*port.*")
	c.Assert(port.IsClosed(), qt.IsFalse)

	// WriteString
	n, err := port.WriteString("hi")
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 2)

	// WriteString
	n, err = port.WriteString(" world")
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 6)

	// WriteRune
	n, err = port.WriteRune('!')
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 1)

	// Flush
	err = port.Flush()
	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "hi world!")

	// EqualTo
	var buf2 bytes.Buffer
	port2 := values.NewCharacterOutputPortFromWriter(&buf2)
	c.Assert(port.EqualTo(port), qt.IsTrue)
	c.Assert(port.EqualTo(port2), qt.IsFalse)
	c.Assert(port.EqualTo(values.NewInteger(1)), qt.IsFalse)

	// Close
	err = port.Close()
	c.Assert(err, qt.IsNil)
	c.Assert(port.IsClosed(), qt.IsTrue)
}

// ByteVectorInputPort

func TestByteVectorInputPort_Basic(t *testing.T) {
	c := qt.New(t)
	port := values.NewByteVectorInputPortFromReader(bytes.NewReader([]byte{10, 20, 30}))
	c.Assert(port.IsVoid(), qt.IsFalse)
	c.Assert(port.SchemeString(), qt.Matches, ".*port.*")
	c.Assert(port.IsClosed(), qt.IsFalse)

	// ReadByte
	b, err := port.ReadByte()
	c.Assert(err, qt.IsNil)
	c.Assert(b, qt.Equals, byte(10))

	// UnreadByte
	err = port.UnreadByte()
	c.Assert(err, qt.IsNil)

	// Read
	buf := make([]byte, 3)
	n, err := port.Read(buf)
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 3)

	// EqualTo
	port2 := values.NewByteVectorInputPortFromReader(bytes.NewReader([]byte{1}))
	c.Assert(port.EqualTo(port), qt.IsTrue)
	c.Assert(port.EqualTo(port2), qt.IsFalse)
	c.Assert(port.EqualTo(values.NewInteger(1)), qt.IsFalse)

	// Close
	err = port.Close()
	c.Assert(err, qt.IsNil)
	c.Assert(port.IsClosed(), qt.IsTrue)
}

// ByteVectorOutputPort

func TestByteVectorOutputPort_Basic(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	port := values.NewByteVectorOutputPortFromWriter(&buf)
	c.Assert(port.IsVoid(), qt.IsFalse)
	c.Assert(port.SchemeString(), qt.Matches, ".*port.*")
	c.Assert(port.IsClosed(), qt.IsFalse)

	// Write
	n, err := port.Write([]byte{1, 2, 3})
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 3)

	// WriteByte
	err = port.WriteByte(4)
	c.Assert(err, qt.IsNil)

	// Flush
	err = port.Flush()
	c.Assert(err, qt.IsNil)

	// EqualTo
	var buf2 bytes.Buffer
	port2 := values.NewByteVectorOutputPortFromWriter(&buf2)
	c.Assert(port.EqualTo(port), qt.IsTrue)
	c.Assert(port.EqualTo(port2), qt.IsFalse)
	c.Assert(port.EqualTo(values.NewInteger(1)), qt.IsFalse)

	// Close
	err = port.Close()
	c.Assert(err, qt.IsNil)
	c.Assert(port.IsClosed(), qt.IsTrue)
}

// BinaryInputPort

func TestBinaryInputPort_Basic(t *testing.T) {
	c := qt.New(t)
	port := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{5, 6, 7}))
	c.Assert(port.IsVoid(), qt.IsFalse)
	c.Assert(port.SchemeString(), qt.Matches, ".*port.*")
	c.Assert(port.IsClosed(), qt.IsFalse)

	// ReadByte
	b, err := port.ReadByte()
	c.Assert(err, qt.IsNil)
	c.Assert(b, qt.Equals, byte(5))

	// UnreadByte
	err = port.UnreadByte()
	c.Assert(err, qt.IsNil)

	// Read
	buf := make([]byte, 3)
	n, err := port.Read(buf)
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 3)

	// EqualTo
	port2 := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1}))
	c.Assert(port.EqualTo(port), qt.IsTrue)
	c.Assert(port.EqualTo(port2), qt.IsFalse)
	c.Assert(port.EqualTo(values.NewInteger(1)), qt.IsFalse)

	// Close
	err = port.Close()
	c.Assert(err, qt.IsNil)
	c.Assert(port.IsClosed(), qt.IsTrue)
}

// BinaryOutputPort

func TestBinaryOutputPort_Basic(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	port := values.NewBinaryOutputPortFromWriter(&buf)
	c.Assert(port.IsVoid(), qt.IsFalse)
	c.Assert(port.SchemeString(), qt.Matches, ".*port.*")
	c.Assert(port.IsClosed(), qt.IsFalse)

	// Write
	n, err := port.Write([]byte{1, 2, 3})
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 3)

	// WriteByte
	err = port.WriteByte(4)
	c.Assert(err, qt.IsNil)

	// Flush
	err = port.Flush()
	c.Assert(err, qt.IsNil)

	// EqualTo
	var buf2 bytes.Buffer
	port2 := values.NewBinaryOutputPortFromWriter(&buf2)
	c.Assert(port.EqualTo(port), qt.IsTrue)
	c.Assert(port.EqualTo(port2), qt.IsFalse)
	c.Assert(port.EqualTo(values.NewInteger(1)), qt.IsFalse)

	// Close
	err = port.Close()
	c.Assert(err, qt.IsNil)
	c.Assert(port.IsClosed(), qt.IsTrue)
}
