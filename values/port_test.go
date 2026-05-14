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
	"bytes"
	"errors"
	"io"
	"regexp"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/werr"
)

// schemeStringPattern enforces the SchemeString R-5 contract:
//
//	<{kind} 0xADDR>
//
// where ADDR is any hex string. No `#` prefix, single angle brackets.
var schemeStringPattern = regexp.MustCompile(`^<[a-z-]+ 0x[0-9a-f]+>$`)

// makePopulatedPort builds a *PortObject with every slot populated. Used
// by the accessor positive-case tests so they all share one fixture.
func makePopulatedPort(t *testing.T) *PortObject {
	t.Helper()
	buf := &bytes.Buffer{}
	rb := bufio.NewReader(buf)
	wb := bufio.NewWriter(buf)
	q := &PortObject{
		rdr:  rb,
		rb:   rb,
		rr:   rb,
		urb:  rb,
		urr:  rb,
		wrt:  wb,
		wb:   wb,
		wr:   wb,
		ws:   wb,
		flsh: wb,
	}
	q.kind = portKindCharacterInput
	q.datum = rb
	return q
}

// TestPortObject_Accessors_PopulatedReturnTrue verifies that every
// accessor returns (non-nil, true) when its slot is populated.
func TestPortObject_Accessors_PopulatedReturnTrue(t *testing.T) {
	c := qt.New(t)
	p := makePopulatedPort(t)

	r, ok := p.AsReader()
	c.Assert(ok, qt.IsTrue)
	c.Assert(r, qt.IsNotNil)

	br, ok := p.AsByteReader()
	c.Assert(ok, qt.IsTrue)
	c.Assert(br, qt.IsNotNil)

	rr, ok := p.AsRuneReader()
	c.Assert(ok, qt.IsTrue)
	c.Assert(rr, qt.IsNotNil)

	urb, ok := p.AsByteUnreader()
	c.Assert(ok, qt.IsTrue)
	c.Assert(urb, qt.IsNotNil)

	urr, ok := p.AsRuneUnreader()
	c.Assert(ok, qt.IsTrue)
	c.Assert(urr, qt.IsNotNil)

	w, ok := p.AsWriter()
	c.Assert(ok, qt.IsTrue)
	c.Assert(w, qt.IsNotNil)

	bw, ok := p.AsByteWriter()
	c.Assert(ok, qt.IsTrue)
	c.Assert(bw, qt.IsNotNil)

	rw, ok := p.AsRuneWriter()
	c.Assert(ok, qt.IsTrue)
	c.Assert(rw, qt.IsNotNil)

	sw, ok := p.AsStringWriter()
	c.Assert(ok, qt.IsTrue)
	c.Assert(sw, qt.IsNotNil)

	flsh, ok := p.AsFlusher()
	c.Assert(ok, qt.IsTrue)
	c.Assert(flsh, qt.IsNotNil)
}

// TestPortObject_Accessors_ZeroValueReturnFalse verifies that a
// freshly-zeroed *PortObject returns (zero, false) from every accessor.
func TestPortObject_Accessors_ZeroValueReturnFalse(t *testing.T) {
	c := qt.New(t)
	p := &PortObject{}

	_, ok := p.AsReader()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsByteReader()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsRuneReader()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsByteUnreader()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsRuneUnreader()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsWriter()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsByteWriter()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsRuneWriter()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsStringWriter()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsFlusher()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsByteVectorExtractor()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.StringContent()
	c.Assert(ok, qt.IsFalse)
}

// TestPortObject_Accessors_NilReceiverReturnFalse verifies the D4
// nil-safety contract: every accessor must tolerate a nil receiver
// and return (zero, false).
func TestPortObject_Accessors_NilReceiverReturnFalse(t *testing.T) {
	c := qt.New(t)
	var p *PortObject

	_, ok := p.AsReader()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsByteReader()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsRuneReader()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsByteUnreader()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsRuneUnreader()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsWriter()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsByteWriter()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsRuneWriter()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsStringWriter()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsFlusher()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.AsByteVectorExtractor()
	c.Assert(ok, qt.IsFalse)
	_, ok = p.StringContent()
	c.Assert(ok, qt.IsFalse)
}

// TestPortObject_Close_Idempotent verifies that Close called twice
// returns nil both times (R7RS port-close idempotence).
func TestPortObject_Close_Idempotent(t *testing.T) {
	c := qt.New(t)
	p := makePopulatedPort(t)

	c.Assert(p.Close(), qt.IsNil)
	c.Assert(p.Close(), qt.IsNil)
	c.Assert(p.IsClosed(), qt.IsTrue)
}

// TestPortObject_Close_FlushOnlyWhenFlsh verifies the D5 encoding:
// flsh != nil triggers flushThenClose, flsh == nil does not.
func TestPortObject_Close_FlushOnlyWhenFlsh(t *testing.T) {
	c := qt.New(t)

	// Port without flsh — Close just marks closed.
	p1 := &PortObject{}
	p1.kind = portKindStringOutput
	c.Assert(p1.Close(), qt.IsNil)
	c.Assert(p1.IsClosed(), qt.IsTrue)

	// Port with flsh — Close calls flushThenClose. Use a recording
	// flusher to verify the call.
	rec := &recordingFlusher{}
	p2 := &PortObject{flsh: rec}
	p2.kind = portKindCharacterOutput
	c.Assert(p2.Close(), qt.IsNil)
	c.Assert(rec.flushed, qt.IsTrue)
}

// recordingFlusher records whether Flush was called. Used to verify
// Close's flsh-conditional flush behavior.
type recordingFlusher struct {
	flushed bool
}

func (r *recordingFlusher) Flush() error {
	r.flushed = true
	return nil
}

// TestPortObject_Close_NilReceiver verifies that Close on a nil
// receiver returns nil (no panic).
func TestPortObject_Close_NilReceiver(t *testing.T) {
	c := qt.New(t)
	var p *PortObject
	c.Assert(p.Close(), qt.IsNil)
}

// TestPortObject_IsVoid verifies that IsVoid returns true for a nil
// receiver and false otherwise.
func TestPortObject_IsVoid(t *testing.T) {
	c := qt.New(t)

	var p *PortObject
	c.Assert(p.IsVoid(), qt.IsTrue)

	q := &PortObject{}
	c.Assert(q.IsVoid(), qt.IsFalse)
}

// TestPortObject_SchemeString_Format enumerates all 9 port kinds and
// asserts the SchemeString format matches the R-5 contract:
//
//	<{kind} 0xADDR>
//
// (no `#` prefix, single angle brackets, lowercase hex address.)
func TestPortObject_SchemeString_Format(t *testing.T) {
	c := qt.New(t)
	allKinds := []string{
		portKindBinaryInput,
		portKindBinaryOutput,
		portKindCharacterInput,
		portKindCharacterOutput,
		portKindStringInput,
		portKindStringOutput,
		portKindBytevectorInput,
		portKindBytevectorOutput,
		portKindBytevectorInputOutput,
	}
	for _, k := range allKinds {
		p := &PortObject{}
		p.kind = k
		p.datum = p
		s := p.SchemeString()
		c.Assert(schemeStringPattern.MatchString(s), qt.IsTrue,
			qt.Commentf("kind=%s SchemeString=%q does not match %s",
				k, s, schemeStringPattern.String()))
	}
}

// TestPortObject_SchemeString_NilReceiver verifies the documented
// sentinel form for nil.
func TestPortObject_SchemeString_NilReceiver(t *testing.T) {
	c := qt.New(t)
	var p *PortObject
	c.Assert(p.SchemeString(), qt.Equals, "<port nil>")
}

// TestPortObject_EqualTo verifies that EqualTo returns true iff same
// kind + same datum.
func TestPortObject_EqualTo(t *testing.T) {
	c := qt.New(t)
	buf := &bytes.Buffer{}

	p1 := &PortObject{}
	p1.kind = portKindStringOutput
	p1.datum = buf

	p2 := &PortObject{}
	p2.kind = portKindStringOutput
	p2.datum = buf

	p3 := &PortObject{}
	p3.kind = portKindStringInput // different kind
	p3.datum = buf

	p4 := &PortObject{}
	p4.kind = portKindStringOutput
	p4.datum = &bytes.Buffer{} // different datum

	c.Assert(p1.EqualTo(p2), qt.IsTrue)
	c.Assert(p1.EqualTo(p3), qt.IsFalse)
	c.Assert(p1.EqualTo(p4), qt.IsFalse)

	// Nil receiver compares equal only to nil Value.
	var pNil *PortObject
	c.Assert(pNil.EqualTo(nil), qt.IsTrue)
	c.Assert(pNil.EqualTo(p1), qt.IsFalse)
}

// TestPortObject_PortKind enumerates all 9 kinds; PortKind returns the
// expected tag. Nil receiver returns "".
func TestPortObject_PortKind(t *testing.T) {
	c := qt.New(t)
	allKinds := []string{
		portKindBinaryInput,
		portKindBinaryOutput,
		portKindCharacterInput,
		portKindCharacterOutput,
		portKindStringInput,
		portKindStringOutput,
		portKindBytevectorInput,
		portKindBytevectorOutput,
		portKindBytevectorInputOutput,
	}
	for _, k := range allKinds {
		p := &PortObject{}
		p.kind = k
		c.Assert(p.PortKind(), qt.Equals, k)
	}

	var pNil *PortObject
	c.Assert(pNil.PortKind(), qt.Equals, "")
}

// TestPortObject_Validate_AcceptsValidConfigs asserts Validate accepts
// each of the 9 documented kind/slot pairings (the "I8 form" required
// by Phase 1: 9 manually-constructed *PortObject literals).
//
// Since Phase 1 does not yet have factories, this test demonstrates
// that the 9 intended slot configurations all pass Validate. Phase 2
// extends this to the actual factory outputs.
func TestPortObject_Validate_AcceptsValidConfigs(t *testing.T) {
	c := qt.New(t)
	buf := &bytes.Buffer{}
	rd := bufio.NewReader(buf)
	wr := bufio.NewWriter(buf)

	cases := []struct {
		kind string
		port *PortObject
	}{
		{portKindBinaryInput, &PortObject{rdr: rd, rb: rd, urb: rd}},
		{portKindBinaryOutput, &PortObject{wrt: wr, wb: wr, flsh: wr}},
		{portKindCharacterInput, &PortObject{rdr: rd, rr: rd, urr: rd}},
		{portKindCharacterOutput, &PortObject{wrt: wr, wr: wr, ws: wr, flsh: wr}},
		{portKindStringInput, &PortObject{rdr: buf, rr: buf, urr: buf}},
		{portKindStringOutput, &PortObject{wrt: buf, wr: buf, ws: buf, sext: buf}},
		{portKindBytevectorInput, &PortObject{rdr: rd, rb: rd, urb: rd}},
		{portKindBytevectorOutput, &PortObject{wrt: wr, wb: wr, flsh: wr}},
		{portKindBytevectorInputOutput, &PortObject{rdr: buf, rb: buf, urb: buf, wrt: buf, wb: buf}},
	}
	for _, tc := range cases {
		tc.port.kind = tc.kind
		c.Assert(tc.port.Validate(), qt.IsNil,
			qt.Commentf("kind=%s should validate", tc.kind))
		c.Assert(tc.port.PortKind(), qt.Equals, tc.kind)
	}
}

// TestPortObject_Validate_RejectsInvariantViolations covers the 7
// negative cases for I1–I7 documented in Validate.
func TestPortObject_Validate_RejectsInvariantViolations(t *testing.T) {
	c := qt.New(t)
	buf := &bytes.Buffer{}
	rd := bufio.NewReader(buf)
	wr := bufio.NewWriter(buf)

	cases := []struct {
		name string
		port *PortObject
	}{
		// I1: rb without rdr
		{"I1: rb without rdr", &PortObject{rb: rd}},
		// I2: rr without rdr
		{"I2: rr without rdr", &PortObject{rr: rd}},
		// I3a: urb without rb. urb requires rb (and transitively rdr).
		{"I3a: urb without rb", &PortObject{rdr: rd, urb: rd}},
		// I4: write capability without wrt
		{"I4: wb without wrt", &PortObject{wb: wr}},
		// I5: ext without wrt
		{"I5: ext without wrt", &PortObject{ext: &fakeBVE{}}},
		// I6: sext without wrt
		{"I6: sext without wrt", &PortObject{sext: buf}},
		// I7: ext and sext both set
		{"I7: ext and sext both set", &PortObject{wrt: buf, ext: &fakeBVE{}, sext: buf}},
	}
	for _, tc := range cases {
		err := tc.port.Validate()
		c.Assert(err, qt.IsNotNil, qt.Commentf("case=%s", tc.name))
		c.Assert(errors.Is(err, werr.ErrInvariantViolation), qt.IsTrue,
			qt.Commentf("case=%s err=%v", tc.name, err))
	}
}

// fakeBVE is a minimal ByteVectorExtractor used in negative-validation
// tests. It is never actually called.
type fakeBVE struct{}

func (*fakeBVE) ReadByteVector() (*ByteVector, error) {
	return nil, io.EOF
}

// TestPortObject_Validate_NilReceiver verifies that Validate on a nil
// receiver returns ErrInvariantViolation rather than panicking.
func TestPortObject_Validate_NilReceiver(t *testing.T) {
	c := qt.New(t)
	var p *PortObject
	err := p.Validate()
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrInvariantViolation), qt.IsTrue)
}

// TestPortObject_StringContent verifies StringContent returns
// (string, true) for sext-populated ports and ("", false) otherwise.
func TestPortObject_StringContent(t *testing.T) {
	c := qt.New(t)
	buf := bytes.NewBufferString("hello")

	p := &PortObject{wrt: buf, sext: buf}
	p.kind = portKindStringOutput
	s, ok := p.StringContent()
	c.Assert(ok, qt.IsTrue)
	c.Assert(s, qt.Equals, "hello")

	// Port without sext.
	p2 := &PortObject{wrt: &bytes.Buffer{}}
	p2.kind = portKindBinaryOutput
	_, ok = p2.StringContent()
	c.Assert(ok, qt.IsFalse)
}
