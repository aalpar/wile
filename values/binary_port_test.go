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
	"errors"
	"io"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// --- BinaryInputPort ---

func TestBinaryInputPort_ReadByte(t *testing.T) {
	port := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{42, 99}))

	b1, err := port.ReadByte()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, b1, qt.Equals, byte(42))

	b2, err := port.ReadByte()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, b2, qt.Equals, byte(99))

	_, err = port.ReadByte()
	qt.Assert(t, err, qt.Equals, io.EOF)
}

func TestBinaryInputPort_Read(t *testing.T) {
	port := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1, 2, 3, 4, 5}))

	buf := make([]byte, 3)
	n, err := port.Read(buf)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, n, qt.Equals, 3)
	qt.Assert(t, buf, qt.DeepEquals, []byte{1, 2, 3})
}

func TestBinaryInputPort_UnreadByte(t *testing.T) {
	port := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1, 2}))

	b, _ := port.ReadByte()
	qt.Assert(t, b, qt.Equals, byte(1))

	err := port.UnreadByte()
	qt.Assert(t, err, qt.IsNil)

	b, _ = port.ReadByte()
	qt.Assert(t, b, qt.Equals, byte(1))
}

func TestBinaryInputPort_Close(t *testing.T) {
	port := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1}))
	err := port.Close()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, port.IsClosed(), qt.IsTrue)

	// Operations after close return ErrPortClosed
	_, err = port.ReadByte()
	qt.Assert(t, errors.Is(err, werr.ErrPortClosed), qt.IsTrue)

	err = port.UnreadByte()
	qt.Assert(t, errors.Is(err, werr.ErrPortClosed), qt.IsTrue)

	_, err = port.Read(make([]byte, 1))
	qt.Assert(t, errors.Is(err, werr.ErrPortClosed), qt.IsTrue)
}

func TestBinaryInputPort_CloseWithCloser(t *testing.T) {
	// io.ReadCloser integrates Closer
	rc := io.NopCloser(bytes.NewReader([]byte{1}))
	port := values.NewBinaryInputPortFromReader(rc)
	err := port.Close()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, port.IsClosed(), qt.IsTrue)
}

func TestBinaryInputPort_IsVoid(t *testing.T) {
	port := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1}))
	qt.Assert(t, port.IsVoid(), qt.IsFalse)

	var nilPort *values.BinaryInputPort
	qt.Assert(t, nilPort.IsVoid(), qt.IsTrue)
}

func TestBinaryInputPort_EqualTo(t *testing.T) {
	port1 := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1}))
	port2 := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1}))
	qt.Assert(t, port1.EqualTo(port2), qt.IsFalse)
	qt.Assert(t, port1.EqualTo(port1), qt.IsTrue)
	qt.Assert(t, port1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestBinaryInputPort_SchemeString(t *testing.T) {
	port := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1}))
	s := port.SchemeString()
	qt.Assert(t, strings.Contains(s, "binary-input-port"), qt.IsTrue)
}

func TestBinaryInputPort_Datum(t *testing.T) {
	port := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1}))
	qt.Assert(t, port.Datum(), qt.Not(qt.IsNil))
}

// --- BinaryOutputPort ---

func TestBinaryOutputPort_WriteByte(t *testing.T) {
	buf := &bytes.Buffer{}
	port := values.NewBinaryOutputPortFromWriter(buf)

	err := port.WriteByte(42)
	qt.Assert(t, err, qt.IsNil)

	err = port.Flush()
	qt.Assert(t, err, qt.IsNil)

	qt.Assert(t, buf.Bytes(), qt.DeepEquals, []byte{42})
}

func TestBinaryOutputPort_Write(t *testing.T) {
	buf := &bytes.Buffer{}
	port := values.NewBinaryOutputPortFromWriter(buf)

	n, err := port.Write([]byte{1, 2, 3})
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, n, qt.Equals, 3)

	err = port.Flush()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.Bytes(), qt.DeepEquals, []byte{1, 2, 3})
}

func TestBinaryOutputPort_Close(t *testing.T) {
	buf := &bytes.Buffer{}
	port := values.NewBinaryOutputPortFromWriter(buf)
	err := port.Close()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, port.IsClosed(), qt.IsTrue)

	// Operations after close return ErrPortClosed
	err = port.WriteByte(1)
	qt.Assert(t, errors.Is(err, werr.ErrPortClosed), qt.IsTrue)

	_, err = port.Write([]byte{1})
	qt.Assert(t, errors.Is(err, werr.ErrPortClosed), qt.IsTrue)

	err = port.Flush()
	qt.Assert(t, errors.Is(err, werr.ErrPortClosed), qt.IsTrue)
}

func TestBinaryOutputPort_IsVoid(t *testing.T) {
	buf := &bytes.Buffer{}
	port := values.NewBinaryOutputPortFromWriter(buf)
	qt.Assert(t, port.IsVoid(), qt.IsFalse)

	var nilPort *values.BinaryOutputPort
	qt.Assert(t, nilPort.IsVoid(), qt.IsTrue)
}

func TestBinaryOutputPort_EqualTo(t *testing.T) {
	port1 := values.NewBinaryOutputPortFromWriter(&bytes.Buffer{})
	port2 := values.NewBinaryOutputPortFromWriter(&bytes.Buffer{})
	qt.Assert(t, port1.EqualTo(port2), qt.IsFalse)
	qt.Assert(t, port1.EqualTo(port1), qt.IsTrue)
	qt.Assert(t, port1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestBinaryOutputPort_SchemeString(t *testing.T) {
	port := values.NewBinaryOutputPortFromWriter(&bytes.Buffer{})
	s := port.SchemeString()
	qt.Assert(t, strings.Contains(s, "binary-output-port"), qt.IsTrue)
}

func TestBinaryOutputPort_Datum(t *testing.T) {
	port := values.NewBinaryOutputPortFromWriter(&bytes.Buffer{})
	qt.Assert(t, port.Datum(), qt.Not(qt.IsNil))
}
