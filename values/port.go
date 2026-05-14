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
	"io"

	"github.com/aalpar/wile/werr"
)

// StringExtractor is implemented by buffers that can yield their accumulated
// bytes as a string. *bytes.Buffer satisfies it. Symmetric with
// ByteVectorExtractor.
type StringExtractor interface {
	String() string
}

// PortObject is the single concrete representation of an R7RS port.
// Capability presence is encoded as nil-checks on the slot fields:
// a non-nil rdr means the port is readable, a non-nil wb means it
// supports byte writes, etc. Construction always goes through one of
// the New*Port factories, which call Validate before returning.
//
// Field-set invariants are documented on Validate. The kind tag and
// slot configuration are paired by construction (every factory writes
// both in the same struct literal); Validate enforces only the
// cross-slot invariants I1–I7.
type PortObject struct {
	portBase

	// Read side.
	rdr io.Reader     // present iff readable
	rb  io.ByteReader // present iff byte-readable
	rr  io.RuneReader // present iff rune-readable
	urb ByteUnreader  // present iff byte-unreadable
	urr RuneUnreader  // present iff rune-unreadable

	// Write side.
	wrt  io.Writer       // present iff writable
	wb   io.ByteWriter   // present iff byte-writable
	wr   RuneWriter      // present iff rune-writable
	ws   io.StringWriter // present iff string-writable
	flsh Flusher         // present iff a real flush is needed at close

	// Output extractors. ext and sext are mutually exclusive (I7).
	ext  ByteVectorExtractor // present iff bytevector-extractable
	sext StringExtractor     // present iff string-extractable
}

// Compile-time interface satisfaction. Port (interface) is declared in
// values.go and survives D1; *PortObject is the sole implementation.
var (
	_ Value = (*PortObject)(nil)
	_ Port  = (*PortObject)(nil)
)

// IsVoid returns true if the receiver is nil. Mirrors the void-receiver
// convention used by other value types.
func (p *PortObject) IsVoid() bool {
	return p == nil
}

// IsClosed returns true if the port has been closed. Nil-safe: a nil
// receiver is treated as closed.
func (p *PortObject) IsClosed() bool {
	if p == nil {
		return true
	}
	return p.portBase.IsClosed()
}

// SchemeString returns the Scheme external representation. Preserves
// the existing portBase format `<{kind} 0xADDR>` verbatim. A nil
// receiver returns `<port nil>`.
func (p *PortObject) SchemeString() string {
	if p == nil {
		return "<port nil>"
	}
	return p.portBase.SchemeString()
}

// EqualTo returns true iff v is a port with the same kind and datum
// identity. Nil-safe: a nil receiver compares equal only to nil.
func (p *PortObject) EqualTo(v Value) bool {
	if p == nil {
		return v == nil
	}
	return p.portBase.EqualTo(v)
}

// PortKind returns the Scheme-visible port kind tag (e.g.,
// "binary-input-port"). Returns "" for a nil receiver — there is no
// "unknown" port kind in the codebase; the empty string is the
// nil-safe sentinel.
func (p *PortObject) PortKind() string {
	if p == nil {
		return ""
	}
	return p.kind
}

// Close flushes (if a flusher is present) and closes the port. Close
// is idempotent. Nil-safe: a nil receiver is a no-op returning nil.
func (p *PortObject) Close() error {
	if p == nil {
		return nil
	}
	if p.flsh != nil {
		return flushThenClose(p.flsh, &p.portBase)
	}
	return p.portBase.Close()
}

// AsReader returns the port's io.Reader and true if readable; nil and
// false otherwise. Nil-safe.
func (p *PortObject) AsReader() (io.Reader, bool) {
	if p == nil {
		return nil, false
	}
	return p.rdr, p.rdr != nil
}

// AsByteReader returns the port's io.ByteReader and true if
// byte-readable; nil and false otherwise. Nil-safe.
func (p *PortObject) AsByteReader() (io.ByteReader, bool) {
	if p == nil {
		return nil, false
	}
	return p.rb, p.rb != nil
}

// AsRuneReader returns the port's io.RuneReader and true if
// rune-readable; nil and false otherwise. Nil-safe.
func (p *PortObject) AsRuneReader() (io.RuneReader, bool) {
	if p == nil {
		return nil, false
	}
	return p.rr, p.rr != nil
}

// AsByteUnreader returns the port's ByteUnreader and true if
// byte-unreadable; nil and false otherwise. Nil-safe.
func (p *PortObject) AsByteUnreader() (ByteUnreader, bool) {
	if p == nil {
		return nil, false
	}
	return p.urb, p.urb != nil
}

// AsRuneUnreader returns the port's RuneUnreader and true if
// rune-unreadable; nil and false otherwise. Nil-safe.
func (p *PortObject) AsRuneUnreader() (RuneUnreader, bool) {
	if p == nil {
		return nil, false
	}
	return p.urr, p.urr != nil
}

// AsWriter returns the port's io.Writer and true if writable; nil and
// false otherwise. Nil-safe.
func (p *PortObject) AsWriter() (io.Writer, bool) {
	if p == nil {
		return nil, false
	}
	return p.wrt, p.wrt != nil
}

// AsByteWriter returns the port's io.ByteWriter and true if
// byte-writable; nil and false otherwise. Nil-safe.
func (p *PortObject) AsByteWriter() (io.ByteWriter, bool) {
	if p == nil {
		return nil, false
	}
	return p.wb, p.wb != nil
}

// AsRuneWriter returns the port's RuneWriter and true if
// rune-writable; nil and false otherwise. Nil-safe.
func (p *PortObject) AsRuneWriter() (RuneWriter, bool) {
	if p == nil {
		return nil, false
	}
	return p.wr, p.wr != nil
}

// AsStringWriter returns the port's io.StringWriter and true if
// string-writable; nil and false otherwise. Nil-safe.
func (p *PortObject) AsStringWriter() (io.StringWriter, bool) {
	if p == nil {
		return nil, false
	}
	return p.ws, p.ws != nil
}

// AsFlusher returns the port's Flusher and true if a real flush is
// needed before close; nil and false otherwise. Nil-safe.
func (p *PortObject) AsFlusher() (Flusher, bool) {
	if p == nil {
		return nil, false
	}
	return p.flsh, p.flsh != nil
}

// AsByteVectorExtractor returns the port's ByteVectorExtractor and
// true if extractable as bytevector; nil and false otherwise.
// Nil-safe.
func (p *PortObject) AsByteVectorExtractor() (ByteVectorExtractor, bool) {
	if p == nil {
		return nil, false
	}
	return p.ext, p.ext != nil
}

// StringContent returns the accumulated string for string-output
// ports. Returns ("", false) if the port is not string-extractable.
// This is the symmetric accessor to AsByteVectorExtractor — it serves
// the get-output-string primitive. Nil-safe.
func (p *PortObject) StringContent() (string, bool) {
	if p == nil || p.sext == nil {
		return "", false
	}
	return p.sext.String(), true
}

// Validate checks the cross-slot capability invariants I1–I7. Every
// New*Port factory calls Validate and panics on failure; embedders
// constructing PortObject literally may call this themselves.
//
// Invariants:
//   - I1: rb != nil requires rdr != nil
//   - I2: rr != nil requires rdr != nil
//   - I3: urb requires rb; urr requires rr
//   - I4: wb, wr, ws non-nil require wrt != nil
//   - I5: ext != nil requires wrt != nil
//   - I6: sext != nil requires wrt != nil
//   - I7: ext and sext are mutually exclusive
//
// I8 (kind matches capability profile) is enforced by construction —
// every factory writes both kind and the slot set in the same struct
// literal — and is asserted at the per-factory test level.
func (p *PortObject) Validate() error {
	if p == nil {
		return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
			"PortObject.Validate: nil receiver")
	}
	if p.rb != nil && p.rdr == nil {
		return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
			"PortObject.Validate: rb without rdr")
	}
	if p.rr != nil && p.rdr == nil {
		return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
			"PortObject.Validate: rr without rdr")
	}
	if p.urb != nil && p.rb == nil {
		return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
			"PortObject.Validate: urb without rb")
	}
	if p.urr != nil && p.rr == nil {
		return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
			"PortObject.Validate: urr without rr")
	}
	if (p.wb != nil || p.wr != nil || p.ws != nil) && p.wrt == nil {
		return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
			"PortObject.Validate: write capability without wrt")
	}
	if p.ext != nil && p.wrt == nil {
		return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
			"PortObject.Validate: ext without wrt")
	}
	if p.sext != nil && p.wrt == nil {
		return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
			"PortObject.Validate: sext without wrt")
	}
	if p.sext != nil && p.ext != nil {
		return werr.WrapForeignErrorf(werr.ErrInvariantViolation,
			"PortObject.Validate: sext and ext both set")
	}
	return nil
}
