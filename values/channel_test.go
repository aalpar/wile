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

package values

import (
	"errors"
	"strings"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"
)

// --- Channel primitive tests ---

func TestChannel_NewChannel(t *testing.T) {
	ch := NewChannel(0)
	qt.Assert(t, ch, qt.Not(qt.IsNil))
	qt.Assert(t, ch.BufferSize(), qt.Equals, 0)
	qt.Assert(t, ch.Cap(), qt.Equals, 0)
	qt.Assert(t, ch.ID() > 0, qt.IsTrue)

	ch2 := NewChannel(5)
	qt.Assert(t, ch2.BufferSize(), qt.Equals, 5)
	qt.Assert(t, ch2.Cap(), qt.Equals, 5)
}

func TestChannel_NewChannel_NegativeBuffer(t *testing.T) {
	ch := NewChannel(-1)
	qt.Assert(t, ch.BufferSize(), qt.Equals, 0)
}

func TestChannel_SendReceive_Buffered(t *testing.T) {
	ch := NewChannel(2)

	err := ch.Send(NewInteger(1))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, ch.Len(), qt.Equals, 1)

	err = ch.Send(NewInteger(2))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, ch.Len(), qt.Equals, 2)

	v, ok := ch.Receive()
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, v, SchemeEquals, NewInteger(1))

	v, ok = ch.Receive()
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, v, SchemeEquals, NewInteger(2))
}

func TestChannel_TrySend_FullBuffer(t *testing.T) {
	ch := NewChannel(1)

	ok, err := ch.TrySend(NewInteger(1))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, ok, qt.IsTrue)

	// Buffer is full, should not block
	ok, err = ch.TrySend(NewInteger(2))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, ok, qt.IsFalse)
}

func TestChannel_TryReceive_Empty(t *testing.T) {
	ch := NewChannel(1)

	v, received, ok := ch.TryReceive()
	qt.Assert(t, received, qt.IsFalse)
	qt.Assert(t, ok, qt.IsTrue) // channel is open
	qt.Assert(t, v == nil, qt.IsTrue)
}

func TestChannel_TryReceive_WithData(t *testing.T) {
	ch := NewChannel(1)
	_ = ch.Send(NewInteger(42))

	v, received, ok := ch.TryReceive()
	qt.Assert(t, received, qt.IsTrue)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, v, SchemeEquals, NewInteger(42))
}

func TestChannel_Close(t *testing.T) {
	ch := NewChannel(1)
	qt.Assert(t, ch.IsClosed(), qt.IsFalse)

	err := ch.Close()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, ch.IsClosed(), qt.IsTrue)
}

func TestChannel_DoubleClose(t *testing.T) {
	ch := NewChannel(0)
	err := ch.Close()
	qt.Assert(t, err, qt.IsNil)

	err = ch.Close()
	qt.Assert(t, errors.Is(err, ErrChannelClosed), qt.IsTrue)
}

func TestChannel_SendAfterClose(t *testing.T) {
	ch := NewChannel(1)
	_ = ch.Close()

	err := ch.Send(NewInteger(1))
	qt.Assert(t, errors.Is(err, ErrChannelClosed), qt.IsTrue)
}

func TestChannel_TrySendAfterClose(t *testing.T) {
	ch := NewChannel(1)
	_ = ch.Close()

	_, err := ch.TrySend(NewInteger(1))
	qt.Assert(t, errors.Is(err, ErrChannelClosed), qt.IsTrue)
}

func TestChannel_TryReceiveAfterClose(t *testing.T) {
	ch := NewChannel(1)
	_ = ch.Close()

	_, _, ok := ch.TryReceive()
	qt.Assert(t, ok, qt.IsFalse) // channel closed
}

func TestChannel_ReceiveAfterClose(t *testing.T) {
	ch := NewChannel(1)
	_ = ch.Send(NewInteger(42))
	_ = ch.Close()

	// Can still receive buffered values
	v, ok := ch.Receive()
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, v, SchemeEquals, NewInteger(42))

	// Then closed
	_, ok = ch.Receive()
	qt.Assert(t, ok, qt.IsFalse)
}

func TestChannel_Chan(t *testing.T) {
	ch := NewChannel(1)
	qt.Assert(t, ch.Chan(), qt.Not(qt.IsNil))
}

func TestChannel_IsVoid(t *testing.T) {
	ch := NewChannel(0)
	qt.Assert(t, ch.IsVoid(), qt.IsFalse)

	var nilCh *Channel
	qt.Assert(t, nilCh.IsVoid(), qt.IsTrue)
}

func TestChannel_EqualTo(t *testing.T) {
	ch1 := NewChannel(0)
	ch2 := NewChannel(0)
	qt.Assert(t, ch1.EqualTo(ch1), qt.IsTrue)
	qt.Assert(t, ch1.EqualTo(ch2), qt.IsFalse)
	qt.Assert(t, ch1.EqualTo(NewInteger(1)), qt.IsFalse)
}

func TestChannel_SchemeString(t *testing.T) {
	ch := NewChannel(0)
	s := ch.SchemeString()
	qt.Assert(t, strings.Contains(s, "unbuffered"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "open"), qt.IsTrue)

	ch2 := NewChannel(5)
	s2 := ch2.SchemeString()
	qt.Assert(t, strings.Contains(s2, "buffered[5]"), qt.IsTrue)

	_ = ch.Close()
	s3 := ch.SchemeString()
	qt.Assert(t, strings.Contains(s3, "closed"), qt.IsTrue)

	var nilCh *Channel
	qt.Assert(t, nilCh.SchemeString(), qt.Equals, "#<channel:void>")
}

// --- ChannelSelect tests ---

func TestChannelSelectReceive(t *testing.T) {
	c := qt.New(t)

	ch1 := NewChannel(1)
	ch2 := NewChannel(1)

	// Send to ch2 so it's ready
	err := ch2.Send(NewInteger(42))
	c.Assert(err, qt.IsNil)

	cases := []SelectCase{
		{Channel: ch1, IsSend: false},
		{Channel: ch2, IsSend: false},
	}

	idx, val, ok := ChannelSelect(cases)
	c.Assert(idx, qt.Equals, 1)
	c.Assert(ok, qt.IsTrue)
	c.Assert(val, SchemeEquals, NewInteger(42))
}

func TestChannelSelectSend(t *testing.T) {
	c := qt.New(t)

	ch := NewChannel(1)
	cases := []SelectCase{
		{Channel: ch, IsSend: true, Value: NewString("hello")},
	}

	idx, _, ok := ChannelSelect(cases)
	c.Assert(idx, qt.Equals, 0)
	c.Assert(ok, qt.IsTrue)

	// Verify the value was sent
	v, recvOK := ch.Receive()
	c.Assert(recvOK, qt.IsTrue)
	c.Assert(v, SchemeEquals, NewString("hello"))
}

func TestChannelSelectDefault(t *testing.T) {
	c := qt.New(t)

	ch := NewChannel(0) // unbuffered, nothing ready
	cases := []SelectCase{
		{Channel: ch, IsSend: false},
		{IsDefault: true},
	}

	idx, _, ok := ChannelSelect(cases)
	c.Assert(idx, qt.Equals, 1)
	c.Assert(ok, qt.IsTrue)
}

func TestChannelSelectBlocking(t *testing.T) {
	c := qt.New(t)

	ch := NewChannel(0) // unbuffered
	cases := []SelectCase{
		{Channel: ch, IsSend: false},
	}

	// Send from another goroutine after a short delay
	go func() {
		time.Sleep(20 * time.Millisecond)
		_ = ch.Send(NewInteger(99))
	}()

	done := make(chan struct{})
	go func() {
		idx, val, ok := ChannelSelect(cases)
		c.Assert(idx, qt.Equals, 0)
		c.Assert(ok, qt.IsTrue)
		c.Assert(val, SchemeEquals, NewInteger(99))
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

	ch := NewChannel(0)
	_ = ch.Close()

	cases := []SelectCase{
		{Channel: ch, IsSend: false},
	}

	idx, _, ok := ChannelSelect(cases)
	c.Assert(idx, qt.Equals, 0)
	c.Assert(ok, qt.IsFalse)
}

func TestChannelSelectSendToClosedChannel(t *testing.T) {
	c := qt.New(t)

	ch := NewChannel(1)
	// Fill the buffer so TrySend fails (non-blocking pass won't catch it)
	_ = ch.Send(NewInteger(1))
	// Close the channel — reflect.Select would panic without the recover guard
	_ = ch.Close()

	cases := []SelectCase{
		{Channel: ch, IsSend: true, Value: NewInteger(2)},
	}

	// Must not panic
	idx, _, ok := ChannelSelect(cases)
	c.Assert(idx, qt.Equals, 0)
	c.Assert(ok, qt.IsFalse)
}
