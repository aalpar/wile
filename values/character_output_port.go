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

package values

import (
	"bufio"
	"fmt"
	"io"
)

var _ Value = (*CharacterOutputPort)(nil)
var _ Port = (*CharacterOutputPort)(nil)
var _ OutputPort = (*CharacterOutputPort)(nil)
var _ TextualWriter = (*CharacterOutputPort)(nil)
var _ io.WriteCloser = (*CharacterOutputPort)(nil)
var _ io.StringWriter = (*CharacterOutputPort)(nil)

// CharacterOutputPort represents a Scheme textual output port.
type CharacterOutputPort struct {
	portBase
	wrt *bufio.Writer
}

// NewCharacterOutputPort creates a new character input port from an io.Reader.
func NewCharacterOutputPort(wrt *bufio.Writer) *CharacterOutputPort {
	q := &CharacterOutputPort{wrt: wrt}
	return q
}

// NewCharacterOutputPortFromWriter creates a new character output port wrapping the given buf.
func NewCharacterOutputPortFromWriter(wrt io.Writer) *CharacterOutputPort {
	q := NewCharacterOutputPort(bufio.NewWriter(wrt))
	closer, ok := wrt.(io.Closer)
	if ok {
		q.clsr = closer
	}
	return q
}

// Close flushes buffered data and closes the underlying stream.
func (p *CharacterOutputPort) Close() error {
	return flushThenClose(p.wrt, &p.portBase)
}

func (p *CharacterOutputPort) Flush() error {
	return guardedFlush(&p.portBase, p.wrt)
}

func (p *CharacterOutputPort) Write(bs []byte) (int, error) {
	return guardedWrite(&p.portBase, p.wrt, bs)
}

func (p *CharacterOutputPort) WriteString(s string) (int, error) {
	return guardedWriteString(&p.portBase, p.wrt, s)
}

// WriteRune writes a single rune to the port's buf.
func (p *CharacterOutputPort) WriteRune(rn rune) (int, error) {
	return guardedWriteRune(&p.portBase, p.wrt, rn)
}

// Datum returns the underlying data of the CharacterOutputPort as an io.Writer.
// there is no RunWriter interface in the standard library, so we just use io.Writer here.
func (p *CharacterOutputPort) Datum() io.Writer {
	return p.wrt
}

// IsVoid returns true if the port is nil.
func (p *CharacterOutputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports wrap the same buf.
func (p *CharacterOutputPort) EqualTo(v Value) bool {
	other, ok := v.(*CharacterOutputPort)
	if ok {
		return p.wrt == other.wrt
	}
	return false
}

// SchemeString returns the Scheme representation of the port.
func (p *CharacterOutputPort) SchemeString() string {
	return fmt.Sprintf("<character-output-port %p>", p.wrt)
}
