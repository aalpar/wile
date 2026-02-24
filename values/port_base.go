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

import "io"

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
}

// IsClosed returns true if the port has been closed.
func (b *portBase) IsClosed() bool {
	return b.closed
}

// Close marks the port as closed and, if a closer was provided,
// closes the underlying stream. Close is idempotent.
func (b *portBase) Close() error {
	if b.closed {
		return nil
	}
	b.closed = true
	if b.clsr != nil {
		return b.clsr.Close()
	}
	return nil
}

// setCloser checks if v implements io.Closer and, if so, stores it
// for use when the port is closed. Used by port constructors to detect
// closeable underlying streams.
func (b *portBase) setCloser(v any) {
	closer, ok := v.(io.Closer)
	if ok {
		b.clsr = closer
	}
}

// guardClosed returns ErrPortClosed if the port is closed.
// I/O methods call this at the top to reject operations on closed ports.
func (b *portBase) guardClosed() error {
	if b.closed {
		return ErrPortClosed
	}
	return nil
}
