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
	"fmt"
	"sync"
	"sync/atomic"

	"github.com/aalpar/wile/pkg/werr"
)

var (
	_ Value = (*Channel)(nil)

	// channelIDCounter assigns each Channel a unique ID.
	channelIDCounter atomic.Uint64
)

// SendOutcome reports how a send resolved. It is the seam that keeps the
// ctx-cancellation cause visible to the caller: the primitive layer decides
// whether a cancelled send is surfaced to Scheme as an ordinary closed-channel
// error (the current policy) or as a distinct condition, without any change to
// the channel lifecycle below.
type SendOutcome int

const (
	// SendSent means the value was delivered.
	SendSent SendOutcome = iota
	// SendClosed means the channel was closed; the value was not delivered.
	SendClosed
	// SendWouldBlock is returned by TrySend only: the buffer is full or there
	// is no ready receiver.
	SendWouldBlock
	// SendCancelled means a blocking Send observed ctx cancellation (deadline
	// or thread-terminate!) before the value was delivered.
	SendCancelled
)

// String names the outcome, so %v in diagnostics reads "SendCancelled" rather
// than a bare integer.
func (o SendOutcome) String() string {
	switch o {
	case SendSent:
		return "SendSent"
	case SendClosed:
		return "SendClosed"
	case SendWouldBlock:
		return "SendWouldBlock"
	case SendCancelled:
		return "SendCancelled"
	default:
		return fmt.Sprintf("SendOutcome(%d)", int(o))
	}
}

// RecvOutcome reports how a receive resolved. See SendOutcome for why the cause
// is surfaced rather than flattened.
type RecvOutcome int

const (
	// RecvReceived means a value was produced from the channel. The value is
	// whatever was sent, which may itself be nil (a nil Value on the channel);
	// callers that treat nil specially must still check it (PrimChannelReceive
	// maps RecvReceived+nil to Void).
	RecvReceived RecvOutcome = iota
	// RecvClosed means the channel is closed and drained.
	RecvClosed
	// RecvWouldBlock is returned by TryReceive only: nothing is buffered and
	// the channel is still open.
	RecvWouldBlock
	// RecvCancelled means a blocking Receive observed ctx cancellation.
	RecvCancelled
)

// String names the outcome, so %v in diagnostics reads "RecvCancelled" rather
// than a bare integer.
func (o RecvOutcome) String() string {
	switch o {
	case RecvReceived:
		return "RecvReceived"
	case RecvClosed:
		return "RecvClosed"
	case RecvWouldBlock:
		return "RecvWouldBlock"
	case RecvCancelled:
		return "RecvCancelled"
	default:
		return fmt.Sprintf("RecvOutcome(%d)", int(o))
	}
}

// Channel represents a Go channel exposed to Scheme.
//
// Lifecycle: the underlying data channel (ch) is never closed. Closure is
// signalled by closing a separate done channel exactly once, guarded by
// closeOnce. This is deliberate — closing ch while a send may be in flight is a
// data race (and a "send on closed channel" host panic) under Go's memory
// model. Because ch is never closed, every send/receive is a select over
// {data op, done, ctx.Done()}: a concurrent Close can never panic a send, the
// operations are -race-clean, and a blocking op honours the VM deadline /
// thread-terminate! instead of leaking a parked goroutine.
//
// This type carries no transactional guarantee across operations; concurrent
// senders/receivers observe standard Go channel semantics. Status (closed) is a
// lock-free atomic; there is no mutex.
type Channel struct {
	id         uint64
	bufferSize int
	ch         chan Value
	done       chan struct{} // closed exactly once by Close; the closure signal
	closeOnce  sync.Once     // guards the single close(done) + closed.Store
	closed     atomic.Bool   // status for IsClosed / SchemeString; set before done closes
}

// NewChannel creates a new channel with the given buffer size.
// bufferSize of 0 creates an unbuffered channel.
func NewChannel(bufferSize int) *Channel {
	if bufferSize < 0 {
		bufferSize = 0
	}
	id := channelIDCounter.Add(1)
	return &Channel{
		id:         id,
		bufferSize: bufferSize,
		ch:         make(chan Value, bufferSize),
		done:       make(chan struct{}),
	}
}

// ID returns the channel's unique identifier.
func (p *Channel) ID() uint64 {
	return p.id
}

// BufferSize returns the channel's buffer size.
func (p *Channel) BufferSize() int {
	return p.bufferSize
}

// Send sends a value on the channel, blocking until the value is delivered, the
// channel is closed, or ctx is cancelled.
func (p *Channel) Send(ctx context.Context, v Value) SendOutcome {
	// Closed wins over an available buffer slot: never deliver to a closed
	// channel. (A send racing a concurrent close may still land in the buffer
	// and be drained by a receiver — a legitimate send-before-close ordering,
	// never a panic, since ch is never closed.)
	if p.closed.Load() {
		return SendClosed
	}
	select {
	case p.ch <- v:
		return SendSent
	case <-p.done:
		return SendClosed
	case <-ctx.Done():
		return SendCancelled
	}
}

// TrySend attempts to send a value without blocking.
func (p *Channel) TrySend(v Value) SendOutcome {
	if p.closed.Load() {
		return SendClosed
	}
	select {
	case p.ch <- v:
		return SendSent
	case <-p.done:
		return SendClosed
	default:
		return SendWouldBlock
	}
}

// Receive receives a value from the channel, blocking until a value is
// available, the channel is closed and drained, or ctx is cancelled.
//
// This is a plain 3-way select, mirroring Go's own `select { case v := <-ch;
// case <-ctx.Done() }`: when a buffered value and cancellation are both ready
// the choice is pseudo-random, exactly as Go's select is. The one priority Go
// DOES guarantee — a receive on a closed channel yields buffered values before
// the closed signal — is provided by the inner drain on the done arm, not by a
// leading non-blocking receive (which would impose a data-beats-ctx priority Go
// does not have).
func (p *Channel) Receive(ctx context.Context) (Value, RecvOutcome) {
	select {
	case v := <-p.ch:
		return v, RecvReceived
	case <-p.done:
		// Closed: a straggler may still sit in the buffer (the outer select
		// picks randomly between a ready ch and a ready done); drain it before
		// reporting closed, per Go's drain-then-zero close semantics.
		select {
		case v := <-p.ch:
			return v, RecvReceived
		default:
			return nil, RecvClosed
		}
	case <-ctx.Done():
		return nil, RecvCancelled
	}
}

// TryReceive attempts to receive a value without blocking.
func (p *Channel) TryReceive() (Value, RecvOutcome) {
	// Drain a buffered value first, even if the channel is closed.
	select {
	case v := <-p.ch:
		return v, RecvReceived
	default:
	}
	if p.closed.Load() {
		return nil, RecvClosed
	}
	return nil, RecvWouldBlock
}

// Close closes the channel. It is idempotent-safe: a second Close returns
// ErrChannelClosed rather than panicking.
func (p *Channel) Close() error {
	first := false
	p.closeOnce.Do(func() {
		first = true
		p.closed.Store(true)
		close(p.done)
	})
	if !first {
		return werr.WrapForeignErrorf(werr.ErrChannelClosed, "Close: channel already closed")
	}
	return nil
}

// IsClosed returns true if the channel is closed.
func (p *Channel) IsClosed() bool {
	return p.closed.Load()
}

// Len returns the number of elements queued in the channel.
func (p *Channel) Len() int {
	return len(p.ch)
}

// Cap returns the channel's capacity.
func (p *Channel) Cap() int {
	return p.bufferSize
}

// Chan returns the underlying Go channel for use in select statements.
//
// The caller MUST NOT close the returned channel: the never-closed invariant is
// what makes concurrent sends panic-free (see the Channel doc comment), and
// close(ch) from here would reintroduce the "send on closed channel" host panic
// this type exists to prevent. Closure is signalled only through Close/IsClosed.
// A caller ranging or selecting on the returned channel will not observe closure
// by the channel's own lifecycle.
func (p *Channel) Chan() chan Value {
	return p.ch
}

// buf interface implementation

// IsVoid returns true if this channel is nil.
func (p *Channel) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both channels are the same object.
func (p *Channel) EqualTo(v Value) bool {
	other, ok := v.(*Channel)
	if !ok {
		return false
	}
	return p == other // Identity is reference equality
}

// SchemeString returns the Scheme representation of this channel.
func (p *Channel) SchemeString() string {
	if p == nil {
		return "#<channel:void>"
	}
	status := "open"
	if p.closed.Load() {
		status = "closed"
	}
	if p.bufferSize == 0 {
		return fmt.Sprintf("#<channel:unbuffered id=%d %s>", p.id, status)
	}
	return fmt.Sprintf("#<channel:buffered[%d] id=%d %s len=%d>", p.bufferSize, p.id, status, len(p.ch))
}
