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

	err := ch.Send(values.NewInteger(1))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, ch.Len(), qt.Equals, 1)

	err = ch.Send(values.NewInteger(2))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, ch.Len(), qt.Equals, 2)

	v, ok := ch.Receive()
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(1))

	v, ok = ch.Receive()
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(2))
}

func TestChannel_TrySend_FullBuffer(t *testing.T) {
	ch := values.NewChannel(1)

	ok, err := ch.TrySend(values.NewInteger(1))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, ok, qt.IsTrue)

	// Buffer is full, should not block
	ok, err = ch.TrySend(values.NewInteger(2))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, ok, qt.IsFalse)
}

func TestChannel_TryReceive_Empty(t *testing.T) {
	ch := values.NewChannel(1)

	v, received, ok := ch.TryReceive()
	qt.Assert(t, received, qt.IsFalse)
	qt.Assert(t, ok, qt.IsTrue) // channel is open
	qt.Assert(t, v == nil, qt.IsTrue)
}

func TestChannel_TryReceive_WithData(t *testing.T) {
	ch := values.NewChannel(1)
	_ = ch.Send(values.NewInteger(42))

	v, received, ok := ch.TryReceive()
	qt.Assert(t, received, qt.IsTrue)
	qt.Assert(t, ok, qt.IsTrue)
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

	err := ch.Send(values.NewInteger(1))
	qt.Assert(t, errors.Is(err, werr.ErrChannelClosed), qt.IsTrue)
}

func TestChannel_TrySendAfterClose(t *testing.T) {
	ch := values.NewChannel(1)
	_ = ch.Close()

	_, err := ch.TrySend(values.NewInteger(1))
	qt.Assert(t, errors.Is(err, werr.ErrChannelClosed), qt.IsTrue)
}

func TestChannel_TryReceiveAfterClose(t *testing.T) {
	ch := values.NewChannel(1)
	_ = ch.Close()

	_, _, ok := ch.TryReceive()
	qt.Assert(t, ok, qt.IsFalse) // channel closed
}

func TestChannel_ReceiveAfterClose(t *testing.T) {
	ch := values.NewChannel(1)
	_ = ch.Send(values.NewInteger(42))
	_ = ch.Close()

	// Can still receive buffered values
	v, ok := ch.Receive()
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(42))

	// Then closed
	_, ok = ch.Receive()
	qt.Assert(t, ok, qt.IsFalse)
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
	err := ch2.Send(values.NewInteger(42))
	c.Assert(err, qt.IsNil)

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
	v, recvOK := ch.Receive()
	c.Assert(recvOK, qt.IsTrue)
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
		_ = ch.Send(values.NewInteger(99))
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

func TestChannelSelectSendToClosedChannel(t *testing.T) {
	c := qt.New(t)

	ch := values.NewChannel(1)
	// Fill the buffer so TrySend fails (non-blocking pass won't catch it)
	_ = ch.Send(values.NewInteger(1))
	// Close the channel — reflect.Select would panic without the recover guard
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
		_ = ready.Send(values.NewInteger(99))

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
			_ = full.Send(values.NewInteger(0)) // open but full: would block
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
