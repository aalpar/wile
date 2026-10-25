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
// matchable and rendered when its text says more than the cause's (a host
// primitive's own wrap), so the failure's story is not reduced to its class.
// FailureMessage projects it as it renders; a sentinel cause (hostCause) is a
// category there and drops out, as it would wrapped directly.
func TestContextEnding(t *testing.T) {
	c := qt.New(t)
	c.Assert(werr.ContextEnding(t.Context(), nil), qt.IsNil)

	hostCause := werr.NewStaticError("werr test: host cause")
	hostWrap := werr.WrapForeignErrorf(context.Canceled, "host: fetch http://x failed")
	cases := []struct {
		name    string
		cause   error
		via     error
		render  string
		message string // FailureMessage of the ending wrapped under "site"
		match   []error
	}{
		{"plain cancel", nil, nil,
			"context canceled", "site: context canceled", []error{context.Canceled}},
		{"host cause", hostCause, nil,
			"werr test: host cause", "site", []error{hostCause, context.Canceled}},
		{"bare walker via", nil, context.Canceled,
			"context canceled", "site: context canceled", []error{context.Canceled}},
		{"host primitive via", nil, hostWrap,
			"host: fetch http://x failed: context canceled", "site: host: fetch http://x failed: context canceled",
			[]error{context.Canceled, hostWrap}},
		{"host primitive via, host cause", hostCause, hostWrap,
			"host: fetch http://x failed: context canceled: werr test: host cause",
			"site: host: fetch http://x failed: context canceled",
			[]error{hostCause, context.Canceled, hostWrap}},
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
			wrapped := werr.WrapForeignErrorWithCause(werr.ErrOperationCancelled, got, "site")
			c.Assert(werr.FailureMessage(wrapped), qt.Equals, tc.message)
		})
	}
}

// TestWaitCancelled pins the shared cancelled-wait failure: the class, the ctx's
// cause and ctx.Err() all match, and the site leads the message.
func TestWaitCancelled(t *testing.T) {
	c := qt.New(t)
	hostCause := werr.NewStaticError("werr test: cancelled by the host")
	ctx, cancel := context.WithCancelCause(context.Background())
	cancel(hostCause)
	got := werr.WaitCancelled(ctx, "process-wait")
	c.Assert(errors.Is(got, werr.ErrOperationCancelled), qt.IsTrue)
	c.Assert(errors.Is(got, hostCause), qt.IsTrue)
	c.Assert(errors.Is(got, context.Canceled), qt.IsTrue)
	c.Assert(got.Error(), qt.Contains, "process-wait: cancelled before the wait completed")
}
