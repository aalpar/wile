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
	"time"

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

// --- ChannelSelect tests ---

func TestChannelSelectReceive(t *testing.T) {
	c := qt.New(t)

	ch1 := values.NewChannel(1)
	ch2 := values.NewChannel(1)

	// Send to ch2 so it's ready
	c.Assert(ch2.Send(context.Background(), values.NewInteger(42)), qt.Equals, values.SendSent)

	cases := []values.SelectCase{
		{Channel: ch1, Kind: values.SelectReceive},
		{Channel: ch2, Kind: values.SelectReceive},
	}

	idx, val, ok := values.ChannelSelect(cases)
	c.Assert(idx, qt.Equals, 1)
	c.Assert(ok, qt.IsTrue)
	c.Assert(val, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestChannelSelectSend(t *testing.T) {
	c := qt.New(t)

	ch := values.NewChannel(1)
	cases := []values.SelectCase{
		{Channel: ch, Kind: values.SelectSend, Value: values.NewString("hello")},
	}

	idx, _, ok := values.ChannelSelect(cases)
	c.Assert(idx, qt.Equals, 0)
	c.Assert(ok, qt.IsTrue)

	// Verify the value was sent
	v, out := ch.Receive(context.Background())
	c.Assert(out, qt.Equals, values.RecvReceived)
	c.Assert(v, valuestest.SchemeEquals, values.NewString("hello"))
}

func TestChannelSelectDefault(t *testing.T) {
	c := qt.New(t)

	ch := values.NewChannel(0) // unbuffered, nothing ready
	cases := []values.SelectCase{
		{Channel: ch, Kind: values.SelectReceive},
		{Kind: values.SelectDefault},
	}

	idx, _, ok := values.ChannelSelect(cases)
	c.Assert(idx, qt.Equals, 1)
	c.Assert(ok, qt.IsTrue)
}

func TestChannelSelectBlocking(t *testing.T) {
	c := qt.New(t)

	ch := values.NewChannel(0) // unbuffered
	cases := []values.SelectCase{
		{Channel: ch, Kind: values.SelectReceive},
	}

	// Send from another goroutine — unbuffered channel semantics
	// synchronize sender and receiver, so no sleep needed.
	go func() {
		_ = ch.Send(context.Background(), values.NewInteger(99))
	}()

	done := make(chan struct{})
	go func() {
		idx, val, ok := values.ChannelSelect(cases)
		c.Assert(idx, qt.Equals, 0)
		c.Assert(ok, qt.IsTrue)
		c.Assert(val, valuestest.SchemeEquals, values.NewInteger(99))
		close(done)
	}()

	select {
	case <-done:
		// Success — ChannelSelect unblocked without busy-spinning
	case <-time.After(2 * time.Second):
		t.Fatal("ChannelSelect blocked indefinitely")
	}
}

func TestChannelSelectClosedChannel(t *testing.T) {
	c := qt.New(t)

	ch := values.NewChannel(0)
	_ = ch.Close()

	cases := []values.SelectCase{
		{Channel: ch, Kind: values.SelectReceive},
	}

	idx, _, ok := values.ChannelSelect(cases)
	c.Assert(idx, qt.Equals, 0)
	c.Assert(ok, qt.IsFalse)
}

// TestChannelSelectClosedButBuffered pins the Len()==0 guard in firstDeadCase:
// a receive case on a CLOSED channel that still holds buffered values is not
// dead — the ready pass must drain the buffered value (ok=true), not report the
// case as a dead closed-receive (ok=false). Dropping the Len()==0 conjunct
// would regress this silently.
func TestChannelSelectClosedButBuffered(t *testing.T) {
	c := qt.New(t)

	ch := values.NewChannel(1)
	_ = ch.Send(context.Background(), values.NewInteger(7))
	_ = ch.Close() // closed, but the buffered 7 must still be receivable

	cases := []values.SelectCase{
		{Channel: ch, Kind: values.SelectReceive},
	}

	idx, val, ok := values.ChannelSelect(cases)
	c.Assert(idx, qt.Equals, 0)
	c.Assert(ok, qt.IsTrue)
	c.Assert(val, valuestest.SchemeEquals, values.NewInteger(7))
}

func TestChannelSelectSendToClosedChannel(t *testing.T) {
	c := qt.New(t)

	ch := values.NewChannel(1)
	// Fill the buffer so TrySend fails (non-blocking pass won't catch it)
	_ = ch.Send(context.Background(), values.NewInteger(1))
	// Close the channel — the send case is now dead and reported deterministically
	_ = ch.Close()

	cases := []values.SelectCase{
		{Channel: ch, Kind: values.SelectSend, Value: values.NewInteger(2)},
	}

	// Must not panic
	idx, _, ok := values.ChannelSelect(cases)
	c.Assert(idx, qt.Equals, 0)
	c.Assert(ok, qt.IsFalse)
}

// TestChannelSelectClosedSendDeterministic pins the deterministic contract for
// closed send cases: a closed send is reported by first-in-slice-order, it is
// reached without panicking, and a ready operation always wins over a dead send
// case regardless of position.
func TestChannelSelectClosedSendDeterministic(t *testing.T) {
	c := qt.New(t)

	// A ready receive must win over a closed send case, even when the closed
	// send appears first in slice order.
	t.Run("ready operation wins over closed send", func(t *testing.T) {
		closed := values.NewChannel(0)
		_ = closed.Close()
		ready := values.NewChannel(1)
		_ = ready.Send(context.Background(), values.NewInteger(99))

		cases := []values.SelectCase{
			{Channel: closed, Kind: values.SelectSend, Value: values.NewInteger(1)},
			{Channel: ready, Kind: values.SelectReceive},
		}
		idx, val, ok := values.ChannelSelect(cases)
		c.Assert(idx, qt.Equals, 1)
		c.Assert(ok, qt.IsTrue)
		c.Assert(val, valuestest.SchemeEquals, values.NewInteger(99))
	})

	// With multiple closed send cases and nothing else ready, the FIRST in
	// slice order is reported, stably across repetitions (no reflect.Select
	// pseudo-random pick, no panic). A full-open send sits between them to
	// prove the dead case is still preferred over a would-block case.
	t.Run("first closed send reported, stable", func(t *testing.T) {
		for range 200 {
			closed0 := values.NewChannel(0)
			_ = closed0.Close()
			full := values.NewChannel(1)
			_ = full.Send(context.Background(), values.NewInteger(0)) // open but full: would block
			closed2 := values.NewChannel(0)
			_ = closed2.Close()

			cases := []values.SelectCase{
				{Channel: closed0, Kind: values.SelectSend, Value: values.NewInteger(1)},
				{Channel: full, Kind: values.SelectSend, Value: values.NewInteger(2)},
				{Channel: closed2, Kind: values.SelectSend, Value: values.NewInteger(3)},
			}
			idx, _, ok := values.ChannelSelect(cases)
			c.Assert(idx, qt.Equals, 0)
			c.Assert(ok, qt.IsFalse)
		}
	})
}
