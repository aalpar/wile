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

package values_test

import (
	"context"
	"errors"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
)

// --- Channel primitive tests ---

func TestChannel_NewChannel(t *testing.T) {
	ch := values.NewChannel(0)
	qt.Assert(t, ch, qt.Not(qt.IsNil))
	qt.Assert(t, ch.BufferSize(), qt.Equals, 0)
	qt.Assert(t, ch.Cap(), qt.Equals, 0)
	qt.Assert(t, ch.ID() > 0, qt.IsTrue)

	ch2 := values.NewChannel(5)
	qt.Assert(t, ch2.BufferSize(), qt.Equals, 5)
	qt.Assert(t, ch2.Cap(), qt.Equals, 5)
}

func TestChannel_NewChannel_NegativeBuffer(t *testing.T) {
	ch := values.NewChannel(-1)
	qt.Assert(t, ch.BufferSize(), qt.Equals, 0)
}

func TestChannel_SendReceive_Buffered(t *testing.T) {
	ch := values.NewChannel(2)
	ctx := context.Background()

	qt.Assert(t, ch.Send(ctx, values.NewInteger(1)), qt.Equals, values.SendSent)
	qt.Assert(t, ch.Len(), qt.Equals, 1)

	qt.Assert(t, ch.Send(ctx, values.NewInteger(2)), qt.Equals, values.SendSent)
	qt.Assert(t, ch.Len(), qt.Equals, 2)

	v, out := ch.Receive(ctx)
	qt.Assert(t, out, qt.Equals, values.RecvReceived)
	qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(1))

	v, out = ch.Receive(ctx)
	qt.Assert(t, out, qt.Equals, values.RecvReceived)
	qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(2))
}

func TestChannel_TrySend_FullBuffer(t *testing.T) {
	ch := values.NewChannel(1)

	qt.Assert(t, ch.TrySend(values.NewInteger(1)), qt.Equals, values.SendSent)

	// Buffer is full, should not block
	qt.Assert(t, ch.TrySend(values.NewInteger(2)), qt.Equals, values.SendWouldBlock)
}

func TestChannel_TryReceive_Empty(t *testing.T) {
	ch := values.NewChannel(1)

	v, out := ch.TryReceive()
	qt.Assert(t, out, qt.Equals, values.RecvWouldBlock) // open, nothing buffered
	qt.Assert(t, v == nil, qt.IsTrue)
}

func TestChannel_TryReceive_WithData(t *testing.T) {
	ch := values.NewChannel(1)
	_ = ch.Send(context.Background(), values.NewInteger(42))

	v, out := ch.TryReceive()
	qt.Assert(t, out, qt.Equals, values.RecvReceived)
	qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestChannel_Close(t *testing.T) {
	ch := values.NewChannel(1)
	qt.Assert(t, ch.IsClosed(), qt.IsFalse)

	err := ch.Close()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, ch.IsClosed(), qt.IsTrue)
}

func TestChannel_DoubleClose(t *testing.T) {
	ch := values.NewChannel(0)
	err := ch.Close()
	qt.Assert(t, err, qt.IsNil)

	err = ch.Close()
	qt.Assert(t, errors.Is(err, werr.ErrChannelClosed), qt.IsTrue)
}

func TestChannel_SendAfterClose(t *testing.T) {
	ch := values.NewChannel(1)
	_ = ch.Close()

	qt.Assert(t, ch.Send(context.Background(), values.NewInteger(1)), qt.Equals, values.SendClosed)
}

func TestChannel_TrySendAfterClose(t *testing.T) {
	ch := values.NewChannel(1)
	_ = ch.Close()

	qt.Assert(t, ch.TrySend(values.NewInteger(1)), qt.Equals, values.SendClosed)
}

func TestChannel_TryReceiveAfterClose(t *testing.T) {
	ch := values.NewChannel(1)
	_ = ch.Close()

	_, out := ch.TryReceive()
	qt.Assert(t, out, qt.Equals, values.RecvClosed)
}

func TestChannel_ReceiveAfterClose(t *testing.T) {
	ch := values.NewChannel(1)
	ctx := context.Background()
	_ = ch.Send(ctx, values.NewInteger(42))
	_ = ch.Close()

	// Can still receive buffered values
	v, out := ch.Receive(ctx)
	qt.Assert(t, out, qt.Equals, values.RecvReceived)
	qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(42))

	// Then closed
	_, out = ch.Receive(ctx)
	qt.Assert(t, out, qt.Equals, values.RecvClosed)
}

func TestChannel_Chan(t *testing.T) {
	ch := values.NewChannel(1)
	qt.Assert(t, ch.Chan(), qt.Not(qt.IsNil))
}

func TestChannel_IsVoid(t *testing.T) {
	ch := values.NewChannel(0)
	qt.Assert(t, ch.IsVoid(), qt.IsFalse)

	var nilCh *values.Channel
	qt.Assert(t, nilCh.IsVoid(), qt.IsTrue)
}

func TestChannel_EqualTo(t *testing.T) {
	ch1 := values.NewChannel(0)
	ch2 := values.NewChannel(0)
	qt.Assert(t, ch1.EqualTo(ch1), qt.IsTrue)
	qt.Assert(t, ch1.EqualTo(ch2), qt.IsFalse)
	qt.Assert(t, ch1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestChannel_SchemeString(t *testing.T) {
	ch := values.NewChannel(0)
	s := ch.SchemeString()
	qt.Assert(t, strings.Contains(s, "unbuffered"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "open"), qt.IsTrue)

	ch2 := values.NewChannel(5)
	s2 := ch2.SchemeString()
	qt.Assert(t, strings.Contains(s2, "buffered[5]"), qt.IsTrue)

	_ = ch.Close()
	s3 := ch.SchemeString()
	qt.Assert(t, strings.Contains(s3, "closed"), qt.IsTrue)

	var nilCh *values.Channel
	qt.Assert(t, nilCh.SchemeString(), qt.Equals, "#<channel:void>")
}
