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

// Package iotest provides fault-injecting I/O ports for testing the
// classification of read errors per R7RS §6.11. It is intended to be
// composed with the io extension via WithExtension in tests only.
//
// The shipped ports (StringInputPort, CharacterInputPort, ...) cannot be
// driven into ReadRune/UnreadRune failure modes from Scheme — their
// underlying bufio.Reader / bytes.Buffer implementations succeed
// deterministically on the call sequences our primitives produce.
// Without a fault-injection layer, the (read-error? e) classification
// path through goErrorToSchemeException is unobservable from a test.
package iotest

import (
	"bytes"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// ErrIOTestFault is the synthetic error returned by FailingTextualInputPort
// when fault injection triggers. Test-only; not part of the production
// werr sentinel inventory.
var ErrIOTestFault = werr.NewStaticError("iotest: injected I/O fault")

var _ values.TextualReader = (*FailingTextualInputPort)(nil)

// FailingTextualInputPort wraps a *values.StringInputPort with configurable
// ReadRune/UnreadRune failure modes for fault-injection testing.
//
// Modes are orthogonal:
//   - failUnread: every UnreadRune call returns ErrIOTestFault.
//   - failReadAfter >= 0: the (failReadAfter+1)th ReadRune returns
//     ErrIOTestFault instead of consulting the wrapped port. Use -1 to
//     disable.
//
// All other Value, Port, and TextualReader methods promote through the
// embedded *StringInputPort.
type FailingTextualInputPort struct {
	*values.StringInputPort
	failUnread      bool
	failReadAfter   int
	successfulReads int
}

// NewFailingTextualInputPort wraps inner with fault-injection. failUnread
// makes every UnreadRune call fail; failReadAfter >= 0 makes the (n+1)th
// ReadRune fail (use -1 to disable).
func NewFailingTextualInputPort(inner *values.StringInputPort, failUnread bool, failReadAfter int) *FailingTextualInputPort {
	q := &FailingTextualInputPort{
		StringInputPort: inner,
		failUnread:      failUnread,
		failReadAfter:   failReadAfter,
	}
	return q
}

// ReadRune returns ErrIOTestFault once the configured threshold is reached;
// otherwise it delegates to the wrapped port and counts successful reads.
func (p *FailingTextualInputPort) ReadRune() (rune, int, error) {
	if p.failReadAfter >= 0 && p.successfulReads >= p.failReadAfter {
		return 0, 0, werr.WrapForeignErrorf(ErrIOTestFault,
			"FailingTextualInputPort: synthetic ReadRune failure after %d reads",
			p.failReadAfter)
	}
	r, n, err := p.StringInputPort.ReadRune()
	if err == nil {
		p.successfulReads++
	}
	return r, n, err
}

// UnreadRune returns ErrIOTestFault when fault injection is enabled;
// otherwise it delegates to the wrapped port.
func (p *FailingTextualInputPort) UnreadRune() error {
	if p.failUnread {
		return werr.WrapForeignErrorf(ErrIOTestFault,
			"FailingTextualInputPort: synthetic UnreadRune failure")
	}
	return p.StringInputPort.UnreadRune()
}

// Extension registers the fault-injection constructor primitives.
// Compose with WithExtension in tests; do NOT load in production.
var Extension = registry.NewDescribedExtension("iotest",
	"TEST ONLY: fault-injecting input ports for read-error? regression tests.",
	addToRegistry)

func addToRegistry(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name: "make-failing-unread-port", ParamCount: 1, Impl: PrimMakeFailingUnreadPort,
			Doc:        "TEST ONLY: returns a textual input port wrapping STRING whose UnreadRune always fails.",
			ParamNames: []string{"string"}, Category: "iotest",
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeTextualInputPort,
		},
		{
			Name: "make-failing-read-after-port", ParamCount: 2, Impl: PrimMakeFailingReadAfterPort,
			Doc:        "TEST ONLY: returns a textual input port wrapping STRING whose ReadRune fails after N successful reads.",
			ParamNames: []string{"string", "n"}, Category: "iotest",
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeInteger},
			ReturnType: values.TypeTextualInputPort,
		},
	}, registry.PhaseRuntime)
	return nil
}

// PrimMakeFailingUnreadPort constructs a FailingTextualInputPort with
// fault-on-UnreadRune.
func PrimMakeFailingUnreadPort(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "make-failing-unread-port")
	if err != nil {
		return err
	}
	q := &FailingTextualInputPort{
		StringInputPort: values.NewStringInputPortWithBuffer(bytes.NewBufferString(s.Value)),
		failUnread:      true,
		failReadAfter:   -1,
	}
	mc.SetValue(q)
	return nil
}

// PrimMakeFailingReadAfterPort constructs a FailingTextualInputPort whose
// ReadRune fails after N successful reads.
func PrimMakeFailingReadAfterPort(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "make-failing-read-after-port")
	if err != nil {
		return err
	}
	n, err := helpers.RequireArg[*values.Integer](mc, 1, werr.ErrNotAnInteger, "make-failing-read-after-port")
	if err != nil {
		return err
	}
	if n.Value < 0 {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"make-failing-read-after-port: n must be non-negative, got %d", n.Value)
	}
	q := &FailingTextualInputPort{
		StringInputPort: values.NewStringInputPortWithBuffer(bytes.NewBufferString(s.Value)),
		failUnread:      false,
		failReadAfter:   int(n.Value),
	}
	mc.SetValue(q)
	return nil
}
