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

package werr_test

import (
	"context"
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// TestContextEnding pins what an ended ctx's ending matches and how it renders:
// nil while live; the cause alone when it already matches ctx.Err(); otherwise
// the cause and ctx.Err() both, rendered as the cause; and via, when given, kept
// matchable. FailureMessage projects it as its cause, so a condition's message
// does not grow a ctx.Err() or a via the error text never showed.
func TestContextEnding(t *testing.T) {
	c := qt.New(t)
	c.Assert(werr.ContextEnding(t.Context(), nil), qt.IsNil)

	hostCause := werr.NewStaticError("werr test: host cause")
	via := werr.WrapForeignErrorf(context.Canceled, "walker")
	cases := []struct {
		name   string
		cause  error
		via    error
		render string
		match  []error
	}{
		{"plain cancel", nil, nil, "context canceled", []error{context.Canceled}},
		{"host cause", hostCause, nil, "werr test: host cause", []error{hostCause, context.Canceled}},
		{"host cause via a walker", hostCause, via, "werr test: host cause", []error{hostCause, context.Canceled, via}},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			ctx, cancel := context.WithCancelCause(context.Background())
			cancel(tc.cause)
			got := werr.ContextEnding(ctx, tc.via)
			c.Assert(got.Error(), qt.Equals, tc.render)
			for _, m := range tc.match {
				c.Assert(errors.Is(got, m), qt.IsTrue, qt.Commentf("must match %v", m))
			}
			// The projection is the cause's own: a sentinel cause (hostCause) is a
			// category and drops out, as it would wrapped directly.
			want := "site"
			fm := werr.FailureMessage(context.Cause(ctx))
			if fm != "" {
				want += ": " + fm
			}
			wrapped := werr.WrapForeignErrorWithCause(werr.ErrOperationCancelled, got, "site")
			c.Assert(werr.FailureMessage(wrapped), qt.Equals, want)
		})
	}
}
