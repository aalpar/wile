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

var _ Value = (*CharacterInputPort)(nil)
var _ Port = (*CharacterInputPort)(nil)
var _ InputPort = (*CharacterInputPort)(nil)
var _ TextualReader = (*CharacterInputPort)(nil)
var _ io.Reader = (*CharacterInputPort)(nil)
var _ io.Closer = (*CharacterInputPort)(nil)
var _ io.RuneScanner = (*CharacterInputPort)(nil)

// CharacterInputPort represents a Scheme textual input port.
type CharacterInputPort struct {
	portBase
	rdr *bufio.Reader
}

// NewCharacterInputPort creates a new character input port from an io.RuneReader.
func NewCharacterInputPort(rdr *bufio.Reader) *CharacterInputPort {
	q := &CharacterInputPort{rdr: rdr}
	return q
}

// NewCharacterInputPortFromReader creates a new character input port from an io.Reader.
func NewCharacterInputPortFromReader(rdr io.Reader) *CharacterInputPort {
	q := &CharacterInputPort{rdr: bufio.NewReader(rdr)}
	closer, ok := rdr.(io.Closer)
	if ok {
		q.clsr = closer
	}
	return q
}

func (p *CharacterInputPort) ReadRune() (rune, int, error) {
	return guardedReadRune(&p.portBase, p.rdr)
}

func (p *CharacterInputPort) Read(bs []byte) (int, error) {
	return guardedRead(&p.portBase, p.rdr, bs)
}

func (p *CharacterInputPort) UnreadRune() error {
	return guardedUnreadRune(&p.portBase, p.rdr)
}

// Datum returns the underlying io.RuneReader.
func (p *CharacterInputPort) Datum() io.RuneReader {
	return p.rdr
}

// IsVoid returns true if the port is nil.
func (p *CharacterInputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports wrap the same rdr.
func (p *CharacterInputPort) EqualTo(v Value) bool {
	other, ok := v.(*CharacterInputPort)
	if ok {
		return p.rdr == other.rdr
	}
	return false
}

// SchemeString returns the Scheme representation of the port.
func (p *CharacterInputPort) SchemeString() string {
	return fmt.Sprintf("<character-input-port %p>", p.rdr)
}
