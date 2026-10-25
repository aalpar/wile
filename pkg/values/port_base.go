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
	"fmt"
	"io"

	"github.com/aalpar/wile/pkg/werr"
)

// Port kind constants for SchemeString display. Two factories
// (NewByteVectorOutputPortFromWriter and NewByteVectorBufferedOutputPort)
// share portKindBytevectorOutput because they represent the same
// R7RS port type — the distinction is in the slot configuration (the
// buffered variant carries `ext` for accumulated-bytes retrieval; the
// writer-backed variant carries `flsh` for flush-on-close).
const (
	portKindStringInput           = "string-input-port"
	portKindStringOutput          = "string-output-port"
	portKindCharacterInput        = "character-input-port"
	portKindCharacterOutput       = "character-output-port"
	portKindBinaryInput           = "binary-input-port"
	portKindBinaryOutput          = "binary-output-port"
	portKindBytevectorInput       = "bytevector-input-port"
	portKindBytevectorOutput      = "bytevector-output-port"
	portKindBytevectorInputOutput = "bytevector-input-output-port"
)

// portBase tracks the closed state of a port, optionally holds an io.Closer for
// the underlying stream, and counts the position of the port's rune reader.
// *PortObject is its sole embedder; portBase supplies guardClosed, a base Close
// and the position arithmetic (consumed, unconsumed), and *PortObject shadows
// IsClosed/Close with nil-safe wrappers.
//
// Close semantics:
//   - Idempotent: calling Close on an already-closed port returns nil.
//   - If clsr is non-nil, delegates to clsr.Close().
//   - Flush-before-close is handled by (*PortObject).Close via the flsh
//     slot (see flushThenClose), not by overriding this method.
type portBase struct {
	closed bool
	clsr   io.Closer
	kind   string
	datum  any
	// srcName labels where the port's data comes from. For a file port it is
	// the engine-resolved absolute path, because the reader stamps it into
	// SourceContext.File and, the port being marked (srcIsPath), include
	// resolution steers by it (lexicalIncludeContext). For a host stream or a
	// process pipe it is a display label with no path separator ("<stdin>",
	// "echo stdout"); it is never a path to open. "" means the port has no
	// name to give (string and bytevector ports, in-memory buffers).
	srcName string
	// srcIsPath is set (MarkNameAsPath) when srcName is the path of the file
	// the port reads, so the reader may mark SourceContext.FileIsPath.
	srcIsPath bool
	// pos is where the port's next rune stands, counted by the port's guarded
	// rune reader (guardedRuneReader) as it yields runes, so it already includes
	// what the tokenizer itself consumed and excludes the lookahead it handed
	// back. Only that reader counts: a consumer that reads the underlying
	// io.Reader through AsReader on a textual port bypasses the counter, and
	// guardedRead and the byte path do not count. No textual consumer in the
	// tree reads that way. last is the rune consumed just before pos: AdvanceIndexes
	// needs it to tell the '\n' of a CRLF pair from a bare one. posBefore and
	// lastBefore are the same pair one read earlier, which is exactly what
	// UnreadRune restores; the io.RuneScanner contract allows one rune of
	// unread, so one step of history is the whole of it.
	//
	// Not synchronised: read-char takes no per-port lock, so two threads
	// reading one port race here as they race on the stream itself (see
	// ReadPosition).
	pos, posBefore   SourceIndexes
	last, lastBefore rune
}

// portOrigin is where a port's first rune stands: index 0, column 0, line 1
// (SourceIndexes documents line as 1-based, so the struct zero value is not it).
func portOrigin() SourceIndexes {
	return NewSourceIndexes(0, 0, 1)
}

// position returns where the port's next rune stands. pos counts lines from
// zero, so a port that has read nothing is at the struct zero value and needs no
// initialisation; SourceIndexes documents line as 1-based, so the shift happens
// here and nowhere else.
func (p *portBase) position() SourceIndexes {
	return NewSourceIndexes(p.pos.Index(), p.pos.Column(), p.pos.Line()+1)
}

// consumed records that one rune of n bytes has been read from the port.
func (p *portBase) consumed(r rune, n int) {
	p.posBefore, p.lastBefore = p.pos, p.last
	p.pos = AdvanceIndexes(p.pos, p.last, r, n)
	p.last = r
}

// unconsumed undoes the most recent consumed. The caller has already seen the
// underlying reader accept the unread, so the history it restores is current.
func (p *portBase) unconsumed() {
	p.pos = p.posBefore
	p.last = p.lastBefore
}

// IsClosed returns true if the port has been closed.
func (p *portBase) IsClosed() bool {
	return p.closed
}

// Close marks the port as closed and, if a closer was provided,
// closes the underlying stream. Close is idempotent.
func (p *portBase) Close() error {
	if p.closed {
		return nil
	}
	p.closed = true
	if p.clsr != nil {
		return p.clsr.Close()
	}
	return nil
}

// setCloser checks if v implements io.Closer and, if so, stores it
// for use when the port is closed. Used by port constructors to detect
// closeable underlying streams.
func (p *portBase) setCloser(v any) {
	closer, ok := v.(io.Closer)
	if ok {
		p.clsr = closer
	}
}

// guardClosed returns werr.ErrPortClosed if the port is closed.
// I/O methods call this at the top to reject operations on closed ports.
func (p *portBase) guardClosed() error {
	if p.closed {
		return werr.ErrPortClosed
	}
	return nil
}

// portDatumProvider is implemented by all port types via portBase embedding.
// It allows EqualTo to extract the portBase from any port Value.
type portDatumProvider interface {
	getPortBase() *portBase
}

func (p *portBase) getPortBase() *portBase {
	return p
}

// EqualTo returns true if v is a port with the same kind and datum identity.
// This works because Go's any equality checks both dynamic type and value,
// so two ports sharing a kind but using different backing types (e.g.,
// *bufio.Writer vs *bytes.Buffer) will never compare equal.
func (p *portBase) EqualTo(v Value) bool {
	other, ok := v.(portDatumProvider)
	if !ok {
		return false
	}
	ob := other.getPortBase()
	return p.kind == ob.kind && p.datum == ob.datum
}

// SchemeString returns the Scheme external representation of the port.
func (p *portBase) SchemeString() string {
	return fmt.Sprintf("<%s %p>", p.kind, p.datum)
}
