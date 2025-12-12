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
	"bytes"
	"strings"
	"testing"
)

func TestCharacterInputPort_NewCharacterInputPort(t *testing.T) {
	reader := strings.NewReader("test")
	port := NewCharacterInputPort(reader)
	qt.Assert(t, port, qt.Not(qt.IsNil))
}

func TestCharacterInputPort_NewCharacterInputPortFromReader(t *testing.T) {
	reader := strings.NewReader("test")
	port := NewCharacterInputPortFromReader(reader)
	qt.Assert(t, port, qt.Not(qt.IsNil))
}

func TestCharacterInputPort_IsVoid(t *testing.T) {
	reader := strings.NewReader("test")
	port := NewCharacterInputPort(reader)
	qt.Assert(t, port.IsVoid(), qt.IsFalse)

	var nilPort *CharacterInputPort
	qt.Assert(t, nilPort.IsVoid(), qt.IsTrue)
}

func TestCharacterInputPort_Datum(t *testing.T) {
	reader := strings.NewReader("test")
	port := NewCharacterInputPort(reader)
	datum := port.Datum()
	qt.Assert(t, datum, qt.Not(qt.IsNil))
}

func TestCharacterInputPort_EqualTo(t *testing.T) {
	reader1 := strings.NewReader("test")
	port1 := NewCharacterInputPort(reader1)
	reader2 := strings.NewReader("test")
	port2 := NewCharacterInputPort(reader2)
	qt.Assert(t, port1.EqualTo(port2), qt.IsFalse)

	qt.Assert(t, port1.EqualTo(port1), qt.IsTrue)
}

func TestCharacterInputPort_SchemeString(t *testing.T) {
	reader := strings.NewReader("test")
	port := NewCharacterInputPort(reader)
	s := port.SchemeString()
	qt.Assert(t, strings.Contains(s, "character-input-port"), qt.IsTrue)
}

func TestCharacterOutputPort_NewCharacterOutputPort(t *testing.T) {
	var buf bytes.Buffer
	port := NewCharacterOutputPort(&buf)
	qt.Assert(t, port, qt.Not(qt.IsNil))
}

func TestCharacterOutputPort_IsVoid(t *testing.T) {
	var buf bytes.Buffer
	port := NewCharacterOutputPort(&buf)
	qt.Assert(t, port.IsVoid(), qt.IsFalse)

	var nilPort *CharacterOutputPort
	qt.Assert(t, nilPort.IsVoid(), qt.IsTrue)
}

func TestCharacterOutputPort_Datum(t *testing.T) {
	var buf bytes.Buffer
	port := NewCharacterOutputPort(&buf)
	datum := port.Datum()
	qt.Assert(t, datum, qt.Not(qt.IsNil))
}

func TestCharacterOutputPort_EqualTo(t *testing.T) {
	var buf1 bytes.Buffer
	port1 := NewCharacterOutputPort(&buf1)
	var buf2 bytes.Buffer
	port2 := NewCharacterOutputPort(&buf2)
	qt.Assert(t, port1.EqualTo(port2), qt.IsFalse)

	qt.Assert(t, port1.EqualTo(port1), qt.IsTrue)
}

func TestCharacterOutputPort_SchemeString(t *testing.T) {
	var buf bytes.Buffer
	port := NewCharacterOutputPort(&buf)
	s := port.SchemeString()
	qt.Assert(t, strings.Contains(s, "character-output-port"), qt.IsTrue)
}
