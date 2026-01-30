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
	wrt    *bufio.Writer
	clsr   io.Closer
	closed bool
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

func (p *CharacterOutputPort) Close() error {
	defer func() { p.closed = true }()
	p.Flush()
	if p.clsr != nil {
		return p.clsr.Close()
	}
	return nil
}

func (p *CharacterOutputPort) IsClosed() bool {
	return p.closed
}

func (p *CharacterOutputPort) Flush() error {
	if p.closed {
		return ErrPortClosed
	}
	return p.wrt.Flush()
}

func (p *CharacterOutputPort) Write(bs []byte) (int, error) {
	if p.closed {
		return 0, ErrPortClosed
	}
	return p.wrt.Write(bs)
}

func (p *CharacterOutputPort) WriteString(s string) (int, error) {
	if p.closed {
		return 0, ErrPortClosed
	}
	return p.wrt.WriteString(s)
}

// WriteRune writes a single rune to the port's buf.
func (p *CharacterOutputPort) WriteRune(rn rune) (int, error) {
	if p.closed {
		return 0, ErrPortClosed
	}
	return p.wrt.WriteRune(rn)
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
	if other, ok := v.(*CharacterOutputPort); ok {
		return p.wrt == other.wrt
	}
	return false
}

// SchemeString returns the Scheme representation of the port.
func (p *CharacterOutputPort) SchemeString() string {
	return fmt.Sprintf("<character-output-port %p>", p.wrt)
}
