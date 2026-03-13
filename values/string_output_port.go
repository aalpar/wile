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
	"bytes"
	"io"
)

var _ Value = (*StringOutputPort)(nil)
var _ Port = (*StringOutputPort)(nil)
var _ OutputPort = (*StringOutputPort)(nil)
var _ TextualWriter = (*StringOutputPort)(nil)
var _ io.WriteCloser = (*StringOutputPort)(nil)
var _ io.StringWriter = (*StringOutputPort)(nil)

// StringOutputPort represents a Scheme string output port backed by a buffer.
type StringOutputPort struct {
	portBase
	buf *bytes.Buffer
}

// NewStringOutputPort creates a new string output port.
func NewStringOutputPort() *StringOutputPort {
	q := &StringOutputPort{
		buf: &bytes.Buffer{},
	}
	q.kind = portKindStringOutput
	q.datum = q.buf
	return q
}

// NewStringOutputPortWithBuffer creates a new string output port.
func NewStringOutputPortWithBuffer(buffer *bytes.Buffer) *StringOutputPort {
	q := &StringOutputPort{
		buf: buffer,
	}
	q.kind = portKindStringOutput
	q.datum = q.buf
	return q
}

// WriteString writes a string to the port.
func (p *StringOutputPort) WriteString(s string) (int, error) {
	return guardedWriteString(&p.portBase, p.buf, s)
}

// Write writes data to the port.
func (p *StringOutputPort) Write(bs []byte) (int, error) {
	return guardedWrite(&p.portBase, p.buf, bs)
}

// WriteRune writes a single rune to the port's buffer.
func (p *StringOutputPort) WriteRune(rn rune) (int, error) {
	return guardedWriteRune(&p.portBase, p.buf, rn)
}

// Flush is a no-op for StringOutputPort.
func (p *StringOutputPort) Flush() error {
	return p.guardClosed()
}

// Datum returns the underlying buffer.
func (p *StringOutputPort) Datum() *bytes.Buffer {
	return p.buf
}

func (p *StringOutputPort) String() string {
	return p.buf.String()
}
