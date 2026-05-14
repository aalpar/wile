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

	"github.com/aalpar/wile/werr"
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

// portBase tracks the closed state of a port and optionally holds
// an io.Closer for the underlying stream. Concrete port types embed
// portBase to inherit IsClosed, Close, and guardClosed.
//
// Close semantics:
//   - Idempotent: calling Close on an already-closed port returns nil.
//   - If clsr is non-nil, delegates to clsr.Close().
//   - Output ports that buffer writes (variant B) must override Close
//     to flush before calling portBase.Close().
type portBase struct {
	closed bool
	clsr   io.Closer
	kind   string
	datum  any
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
