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

package io_test

import (
	"context"
	"testing"

	extio "github.com/aalpar/wile/pkg/extensions/io"
	"github.com/aalpar/wile/pkg/internal/extensions/iotest"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// newEngineWithIOTest creates an engine with both the io and iotest
// extensions loaded. iotest provides fault-injection input ports that let
// us drive ReadRune/UnreadRune failure modes from Scheme — necessary
// because the shipped port types succeed deterministically on the call
// sequences peek-char and read-line produce.
func newEngineWithIOTest(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(iotest.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// TestReadErrorClassification verifies that I/O failures inside peek-char
// and read-line surface as read errors per R7RS §6.11 — i.e.
// (read-error? e) returns #t.
//
// These tests guard against regressions of two prior bugs in
// internal/extensions/io/prim_read_write.go:
//   - peek-char's UnreadRune error was wrapped with WrapForeignErrorf
//     (instead of WrapForeignReadErrorf), classifying it as Generic
//     instead of Read.
//   - read-line silently dropped UnreadRune errors via //nolint:errcheck
//     and silently dropped non-EOF errors from the inner ReadRune.
func TestReadErrorClassification(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithIOTest(t)

	tcs := []struct {
		name string
		code string
	}{
		{
			name: "peek-char surfaces UnreadRune failure as read-error",
			code: `(read-error?
                     (guard (e (#t e))
                       (peek-char (make-failing-unread-port "abc"))))`,
		},
		{
			name: "read-line surfaces UnreadRune failure as read-error",
			code: `(read-error?
                     (guard (e (#t e))
                       (read-line (make-failing-unread-port "ab\rxy"))))`,
		},
		{
			name: "read-line surfaces inner ReadRune failure as read-error",
			code: `(read-error?
                     (guard (e (#t e))
                       (read-line (make-failing-read-after-port "ab\rxy" 3))))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, values.TrueValue)
		})
	}
}

// TestReadLineBareCarriageReturnAtEOF is a positive regression: after the
// read-line fixes, a bare \r at end-of-stream must still produce the
// preceding line, not raise.
func TestReadLineBareCarriageReturnAtEOF(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithIOTest(t)
	// Input "hi\r" — after \r, inner ReadRune sees io.EOF, which must be
	// treated as line-ends-with-bare-\r, not propagated.
	result := eval(t, engine,
		`(equal? (read-line (open-input-string "hi\r")) "hi")`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}
