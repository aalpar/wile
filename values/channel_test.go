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

	qt "github.com/frankban/quicktest"
)

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
