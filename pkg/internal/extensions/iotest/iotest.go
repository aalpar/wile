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
// Post-port-unification (Phase 2) the wrapper composes by feeding
// custom rune-reader and rune-unreader values into the
// values.NewStringInputPortWithReaders factory. The result is a
// *values.PortObject — production code's type assertions accept it
// unchanged. The fault injection lives in the rune-reader/unreader
// implementations.
package iotest

import (
	"bytes"
	"io"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ErrIOTestFault is the synthetic error returned by fault-injecting
// readers/unreaders. Test-only; not part of the production werr
// sentinel inventory.
var ErrIOTestFault = werr.NewStaticError("iotest: injected I/O fault")

// NewFailingTextualInputPort constructs a textual input port whose
// rune reader and rune unreader can be configured to fail.
//
// failUnread: every UnreadRune call returns ErrIOTestFault.
// failReadAfter >= 0: the (failReadAfter+1)th ReadRune returns
// ErrIOTestFault. Use -1 to disable.
//
// Returns a *values.PortObject so that downstream code's type
// assertions on *values.PortObject accept the result unchanged. Fault
// behavior lives in the slot wrappers.
func NewFailingTextualInputPort(s string, failUnread bool, failReadAfter int) *values.PortObject {
	underlying := bytes.NewBufferString(s)
	rr := &countingRuneReader{
		inner:         underlying,
		failReadAfter: failReadAfter,
	}
	var urr values.RuneUnreader
	if failUnread {
		urr = alwaysFailRuneUnreader{}
	} else {
		urr = underlying
	}
	return values.NewStringInputPortWithReaders(underlying, rr, urr)
}

// countingRuneReader wraps an io.RuneReader and counts successful
// reads, returning ErrIOTestFault once failReadAfter is reached.
//
// Counts by `n > 0` rather than `err == nil`: io.RuneReader permits
// (r, size>0, io.EOF) when the last rune is read at end of stream.
type countingRuneReader struct {
	inner           io.RuneReader
	failReadAfter   int
	successfulReads int
}

func (r *countingRuneReader) ReadRune() (rune, int, error) {
	if r.failReadAfter >= 0 && r.successfulReads >= r.failReadAfter {
		return 0, 0, werr.WrapForeignErrorf(ErrIOTestFault,
			"countingRuneReader: synthetic ReadRune failure after %d reads",
			r.failReadAfter)
	}
	rn, n, err := r.inner.ReadRune()
	if n > 0 {
		r.successfulReads++
	}
	return rn, n, err
}

// alwaysFailRuneUnreader returns ErrIOTestFault on every UnreadRune.
type alwaysFailRuneUnreader struct{}

func (alwaysFailRuneUnreader) UnreadRune() error {
	return werr.WrapForeignErrorf(ErrIOTestFault,
		"alwaysFailRuneUnreader: synthetic UnreadRune failure")
}

// Extension registers the fault-injection constructor primitives.
// Compose with WithExtension in tests; do NOT load in production.
var Extension = registry.NewDescribedExtension("iotest",
	"TEST ONLY: fault-injecting input ports for read-error? regression tests.",
	AddToRegistry)

// Builder aggregates iotest registration. Matches the Builder pattern
// used by every other extension package (io, envvars, namespace, ...).
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all iotest primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.PrimitiveRegistry) error {
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
	}, registry.PhaseSetRuntime)
	return nil
}

// PrimMakeFailingUnreadPort constructs a *PortObject with
// fault-on-UnreadRune.
func PrimMakeFailingUnreadPort(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "make-failing-unread-port")
	if err != nil {
		return err
	}
	mc.SetValue(NewFailingTextualInputPort(s.Value, true, -1))
	return nil
}

// PrimMakeFailingReadAfterPort constructs a *PortObject whose
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
	mc.SetValue(NewFailingTextualInputPort(s.Value, false, int(n.Value)))
	return nil
}
