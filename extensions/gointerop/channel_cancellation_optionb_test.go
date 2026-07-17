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

package gointerop_test

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestEmbedderDeadlineReceiveRaisesCancelled is the Option-B guard for the one
// cancellation source with no other protection. A channel receive parked under an
// embedder-supplied deadline (mc.timer == nil, ctx cause context.DeadlineExceeded)
// raises ErrChannelCancelled rather than the closed-channel Void, so a guard can
// tell a cancelled receive from a drained channel. With-timeout (ErrTimerExpired)
// keeps the Void surface — TestWithTimeoutInterruptsParkedReceive pins that half,
// and thread-terminate!'s write-once outcome makes raising harmless there.
func TestEmbedderDeadlineReceiveRaisesCancelled(t *testing.T) {
	engine := newThreadedEngine(t)

	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()

	// Empty channel, no sender: the receive parks until the deadline cancels ctx.
	expr, err := engine.Parse(ctx, "(begin (define ch (make-channel)) (channel-receive ch) )")
	qt.Assert(t, err, qt.IsNil)

	_, err = engine.Eval(ctx, expr)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrChannelCancelled), qt.IsTrue,
		qt.Commentf("want the distinct cancellation sentinel; got %v", err))
}
