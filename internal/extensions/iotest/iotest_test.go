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

package iotest_test

import (
	"context"
	"errors"
	"testing"

	"github.com/aalpar/wile"
	extio "github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/internal/extensions/iotest"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// readRune reads a rune from a fault-injecting port via the public
// AsRuneReader accessor. The fault wrappers live in the rr slot, so
// the accessor returns the failing reader transparently.
func readRune(p *values.PortObject) (rune, int, error) {
	rr, _ := p.AsRuneReader()
	return rr.ReadRune()
}

// unreadRune unreads via the AsRuneUnreader accessor.
func unreadRune(p *values.PortObject) error {
	urr, _ := p.AsRuneUnreader()
	return urr.UnreadRune()
}

// TestFailingTextualInputPort_UnreadFault verifies that UnreadRune returns
// ErrIOTestFault when fault injection is enabled, while ReadRune still
// delegates to the wrapped port.
func TestFailingTextualInputPort_UnreadFault(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name       string
		input      string
		readsFirst int
	}{
		{name: "unread after one read", input: "abc", readsFirst: 1},
		{name: "unread after two reads", input: "abc", readsFirst: 2},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			p := iotest.NewFailingTextualInputPort(tc.input, true, -1)
			for i := 0; i < tc.readsFirst; i++ {
				_, _, err := readRune(p)
				c.Assert(err, qt.IsNil)
			}
			err := unreadRune(p)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, iotest.ErrIOTestFault), qt.IsTrue)
		})
	}
}

// TestFailingTextualInputPort_ReadAfterFault verifies that the (N+1)th
// ReadRune call returns ErrIOTestFault, while the first N succeed normally.
func TestFailingTextualInputPort_ReadAfterFault(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		input string
		n     int
	}{
		{name: "fail on first", input: "abc", n: 0},
		{name: "fail on second", input: "abc", n: 1},
		{name: "fail on third", input: "abc", n: 2},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			p := iotest.NewFailingTextualInputPort(tc.input, false, tc.n)
			for i := 0; i < tc.n; i++ {
				_, _, err := readRune(p)
				c.Assert(err, qt.IsNil)
			}
			_, _, err := readRune(p)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, iotest.ErrIOTestFault), qt.IsTrue)
		})
	}
}

// TestFailingTextualInputPort_NoFault verifies that without fault injection
// the port is transparent.
func TestFailingTextualInputPort_NoFault(t *testing.T) {
	c := qt.New(t)

	p := iotest.NewFailingTextualInputPort("ab", false, -1)
	r, _, err := readRune(p)
	c.Assert(err, qt.IsNil)
	c.Assert(r, qt.Equals, 'a')
	err = unreadRune(p)
	c.Assert(err, qt.IsNil)
	r, _, err = readRune(p)
	c.Assert(err, qt.IsNil)
	c.Assert(r, qt.Equals, 'a')
}

// TestExtensionPrimitives_Construct verifies that the Scheme-callable
// constructors register correctly and produce *PortObject values with
// rune-read capability, so getOptionalInputPort (which checks
// AsRuneReader) accepts them.
func TestExtensionPrimitives_Construct(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		code string
	}{
		{
			name: "make-failing-unread-port returns input port",
			code: `(input-port? (make-failing-unread-port "abc"))`,
		},
		{
			name: "make-failing-read-after-port returns input port",
			code: `(input-port? (make-failing-read-after-port "abc" 1))`,
		},
		{
			name: "make-failing-read-after-port accepts zero",
			code: `(input-port? (make-failing-read-after-port "abc" 0))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine := newEngineWithIO(t)
			q, err := engine.EvalMultiple(context.Background(), tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(q.Internal(), qt.Equals, values.TrueValue)
		})
	}
}

// TestExtensionPrimitives_RejectInvalidArgs verifies argument validation
// surfaces as Go-level errors.
func TestExtensionPrimitives_RejectInvalidArgs(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		code string
	}{
		{name: "unread-port: non-string arg", code: `(make-failing-unread-port 42)`},
		{name: "read-after-port: non-string first arg", code: `(make-failing-read-after-port 42 1)`},
		{name: "read-after-port: non-integer second arg", code: `(make-failing-read-after-port "x" "y")`},
		{name: "read-after-port: negative n", code: `(make-failing-read-after-port "x" -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine := newEngineWithIO(t)
			_, err := engine.EvalMultiple(context.Background(), tc.code)
			c.Assert(err, qt.IsNotNil)
		})
	}
}

// newEngineWithIO loads only the io extension (for input-port?) plus the
// iotest extension under test. Matches the minimal-extension setup used
// by io's own tests (see internal/extensions/io/prim_ports_test.go:29).
func newEngineWithIO(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(iotest.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}
