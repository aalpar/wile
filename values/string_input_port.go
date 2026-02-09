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
	"fmt"
	"io"
)

var _ Value = (*StringInputPort)(nil)
var _ Port = (*StringInputPort)(nil)
var _ InputPort = (*StringInputPort)(nil)
var _ TextualReader = (*StringInputPort)(nil)
var _ io.ReadCloser = (*StringInputPort)(nil)
var _ io.RuneScanner = (*StringInputPort)(nil)

// StringInputPort represents a Scheme string output port backed by a buffer.
type StringInputPort struct {
	buf    *bytes.Buffer
	closed bool
}

// NewStringInputPort creates a new string output port.
func NewStringInputPort() *StringInputPort {
	return &StringInputPort{
		buf: &bytes.Buffer{},
	}
}

// NewStringInputPortWithBuffer creates a new string output port.
func NewStringInputPortWithBuffer(buffer *bytes.Buffer) *StringInputPort {
	q := &StringInputPort{
		buf: buffer,
	}
	return q
}

// Read reads data from the port into bs.
func (p *StringInputPort) Read(bs []byte) (n int, err error) {
	if p.closed {
		return 0, ErrPortClosed
	}
	return p.buf.Read(bs)
}

// ReadRune reads a rune from the port.
func (p *StringInputPort) ReadRune() (r rune, size int, err error) {
	if p.closed {
		return 0, 0, ErrPortClosed
	}
	return p.buf.ReadRune()
}

// UnreadRune unreads the last rune read from the port.
func (p *StringInputPort) UnreadRune() error {
	if p.closed {
		return ErrPortClosed
	}
	return p.buf.UnreadRune()
}

func (p *StringInputPort) Flush() error {
	if p.closed {
		return ErrPortClosed
	}
	return nil
}

func (p *StringInputPort) Close() error {
	defer func() { p.closed = true }()
	return nil
}

func (p *StringInputPort) IsClosed() bool {
	return p.closed
}

// Datum returns the underlying buffer.
func (p *StringInputPort) Datum() *bytes.Buffer {
	return p.buf
}

// IsVoid returns true if the port is nil.
func (p *StringInputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports use the same buffer.
func (p *StringInputPort) EqualTo(v Value) bool {
	other, ok := v.(*StringInputPort)
	if ok {
		return p.buf == other.buf
	}
	return false
}

func (p *StringInputPort) String() string {
	return p.buf.String()
}

// SchemeString returns the Scheme representation of the port.
func (p *StringInputPort) SchemeString() string {
	return fmt.Sprintf("<string-input-output-port %p>", p.buf)
}
