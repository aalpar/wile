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
	"testing"
	"time"

	qt "github.com/frankban/quicktest"
)

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
