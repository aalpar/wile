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
	"context"
	"sync"
	"testing"
	"time"
)

// TestChannel_ConcurrentSendClose_NoPanic is the permanent guard for the
// reviews/2026-07-13 §4 CONCURRENCY finding (channel-send! TOCTOU). It replaces
// the gated repro (formerly channel_toctou_repro_test.go).
//
// Before the done-channel lifecycle, Channel.Send checked p.closed, released
// the lock, then did `ch <- v`; a concurrent Close landing in the gap panicked
// "send on closed channel" — a fatal host crash uncatchable by the VM recover
// boundary, reachable from ordinary Scheme via (channel-send!)/(channel-close!)
// on two SRFI-18 threads. The gated repro reproduced it in ~2/20000 trials.
//
// Now the data channel is never closed (closure is signalled via a separate
// done channel), so a concurrent close-during-send CANNOT panic. Reaching the
// end of every trial without a host panic IS the assertion — this runs in CI,
// ungated, and under -race.
func TestChannel_ConcurrentSendClose_NoPanic(t *testing.T) {
	const trials = 20000
	for range trials {
		ch := NewChannel(1)
		var wg sync.WaitGroup
		wg.Add(1)
		go func() {
			defer wg.Done()
			// Fill past the buffer so a send is in flight when Close lands.
			for range 4 {
				if ch.Send(context.Background(), TrueValue) != SendSent {
					return
				}
			}
		}()
		_ = ch.Close()
		wg.Wait()
	}
}

// TestChannel_ReceiveCancelled_Returns is the guard for the ctx-cancellation
// finding (T1.3): a blocking receive must observe ctx cancellation and return,
// so thread-terminate! / a deadline unwinds a parked receiver instead of
// leaking its goroutine. The result-channel handshake (not a NumGoroutine poll)
// directly proves the parked op returns.
func TestChannel_ReceiveCancelled_Returns(t *testing.T) {
	ch := NewChannel(0) // unbuffered, no sender: a receive parks
	ctx, cancel := context.WithCancel(context.Background())

	result := make(chan RecvOutcome, 1)
	go func() {
		_, out := ch.Receive(ctx)
		result <- out
	}()

	cancel()

	select {
	case out := <-result:
		if out != RecvCancelled {
			t.Fatalf("expected RecvCancelled after ctx cancel, got %v", out)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("Receive did not return after ctx cancellation — parked goroutine leaked")
	}
}

// TestChannel_SendCancelled_Returns is the send-side counterpart: a blocking
// send on a channel with no receiver must return SendCancelled on ctx cancel.
func TestChannel_SendCancelled_Returns(t *testing.T) {
	ch := NewChannel(0) // unbuffered, no receiver: a send parks
	ctx, cancel := context.WithCancel(context.Background())

	result := make(chan SendOutcome, 1)
	go func() {
		result <- ch.Send(ctx, TrueValue)
	}()

	cancel()

	select {
	case out := <-result:
		if out != SendCancelled {
			t.Fatalf("expected SendCancelled after ctx cancel, got %v", out)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("Send did not return after ctx cancellation — parked goroutine leaked")
	}
}

// TestChannel_ReceiveClosed_Wakes is the close-side twin of the cancellation
// guard: a receiver parked in the done arm (empty channel, no sender) must be
// woken by a concurrent Close and report RecvClosed. Without the done-channel
// wake this receiver would block forever. The result-channel handshake proves
// the parked op returned (no NumGoroutine poll).
func TestChannel_ReceiveClosed_Wakes(t *testing.T) {
	ch := NewChannel(0) // unbuffered, empty, no sender: a receive parks
	result := make(chan RecvOutcome, 1)
	go func() {
		_, out := ch.Receive(context.Background())
		result <- out
	}()

	_ = ch.Close()

	select {
	case out := <-result:
		if out != RecvClosed {
			t.Fatalf("expected RecvClosed after concurrent Close, got %v", out)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("Receive did not wake on Close — parked goroutine leaked")
	}
}

// TestChannel_SendClosed_Wakes is the deterministic sender counterpart to the
// no-panic guard: TestChannel_ConcurrentSendClose_NoPanic drives the done arm
// but only asserts "no panic" and discards the outcome. Here a sender parked on
// a full channel with no receiver must be woken by Close and report SendClosed.
func TestChannel_SendClosed_Wakes(t *testing.T) {
	ch := NewChannel(1)
	// Fill the buffer so the next send blocks in the select (no receiver).
	fill := ch.Send(context.Background(), TrueValue)
	if fill != SendSent {
		t.Fatalf("buffer fill: expected SendSent, got %v", fill)
	}
	result := make(chan SendOutcome, 1)
	go func() {
		result <- ch.Send(context.Background(), TrueValue)
	}()

	_ = ch.Close()

	select {
	case out := <-result:
		if out != SendClosed {
			t.Fatalf("expected SendClosed after concurrent Close, got %v", out)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("Send did not wake on Close — parked goroutine leaked")
	}
}
