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
	"fmt"
	"reflect"
	"sync"
	"sync/atomic"

	"github.com/aalpar/wile/werr"
)

var (
	_ Value = (*Channel)(nil)

	// Channel ID counter
	channelIDCounter atomic.Uint64
)

// Channel represents a Go channel exposed to Scheme
type Channel struct {
	id         uint64
	bufferSize int
	ch         chan Value
	closed     bool
	mu         sync.RWMutex
}

// NewChannel creates a new channel with the given buffer size
// bufferSize of 0 creates an unbuffered channel
func NewChannel(bufferSize int) *Channel {
	if bufferSize < 0 {
		bufferSize = 0
	}
	id := channelIDCounter.Add(1)
	return &Channel{
		id:         id,
		bufferSize: bufferSize,
		ch:         make(chan Value, bufferSize),
	}
}

// ID returns the channel's unique identifier
func (p *Channel) ID() uint64 {
	return p.id
}

// BufferSize returns the channel's buffer size
func (p *Channel) BufferSize() int {
	return p.bufferSize
}

// Send sends a value on the channel (blocking)
func (p *Channel) Send(v Value) error {
	p.mu.RLock()
	if p.closed {
		p.mu.RUnlock()
		return werr.ErrChannelClosed
	}
	ch := p.ch
	p.mu.RUnlock()

	ch <- v
	return nil
}

// TrySend attempts to send a value without blocking
// Returns true if sent, false if would block
func (p *Channel) TrySend(v Value) (bool, error) {
	p.mu.RLock()
	if p.closed {
		p.mu.RUnlock()
		return false, werr.ErrChannelClosed
	}
	ch := p.ch
	p.mu.RUnlock()

	select {
	case ch <- v:
		return true, nil
	default:
		return false, nil
	}
}

// Receive receives a value from the channel (blocking)
// Returns the value and true, or nil and false if channel is closed
func (p *Channel) Receive() (Value, bool) {
	p.mu.RLock()
	ch := p.ch
	p.mu.RUnlock()

	v, ok := <-ch
	return v, ok
}

// TryReceive attempts to receive a value without blocking
// Returns (value, true, true) if received
// Returns (nil, false, true) if would block
// Returns (nil, false, false) if channel is closed
func (p *Channel) TryReceive() (Value, bool, bool) {
	p.mu.RLock()
	ch := p.ch
	closed := p.closed
	p.mu.RUnlock()

	select {
	case v, ok := <-ch:
		if !ok {
			return nil, false, false // channel closed
		}
		return v, true, true
	default:
		if closed {
			return nil, false, false
		}
		return nil, false, true // would block
	}
}

// Close closes the channel
func (p *Channel) Close() error {
	p.mu.Lock()
	defer p.mu.Unlock()

	if p.closed {
		return werr.ErrChannelClosed
	}
	p.closed = true
	close(p.ch)
	return nil
}

// IsClosed returns true if the channel is closed
func (p *Channel) IsClosed() bool {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.closed
}

// Len returns the number of elements queued in the channel
func (p *Channel) Len() int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return len(p.ch)
}

// Cap returns the channel's capacity
func (p *Channel) Cap() int {
	return p.bufferSize
}

// Chan returns the underlying Go channel for use in select statements
func (p *Channel) Chan() chan Value {
	p.mu.RLock()
	defer p.mu.RUnlock()
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
	p.mu.RLock()
	defer p.mu.RUnlock()
	status := "open"
	if p.closed {
		status = "closed"
	}
	if p.bufferSize == 0 {
		return fmt.Sprintf("#<channel:unbuffered id=%d %s>", p.id, status)
	}
	return fmt.Sprintf("#<channel:buffered[%d] id=%d %s len=%d>", p.bufferSize, p.id, status, len(p.ch))
}

// SelectCase represents a case in a channel select operation
// SelectCaseKind distinguishes the three valid select case types.
type SelectCaseKind int

const (
	SelectReceive SelectCaseKind = iota
	SelectSend
	SelectDefault
)

type SelectCase struct {
	Channel *Channel
	Value   Value // for send operations
	Kind    SelectCaseKind
}

// ChannelSelect performs a select operation on multiple channels.
// Returns the index of the selected case and the received value (for receive cases).
// If a send case targets a channel that is closed concurrently, the select returns
// that case's index with ok=false instead of panicking.
func ChannelSelect(cases []SelectCase) (idx int, val Value, ok bool) {
	if len(cases) == 0 {
		return -1, nil, false
	}

	// Build native select cases
	// First pass: try non-blocking operations before falling through to reflect.Select
	for i, c := range cases {
		if c.Kind == SelectDefault {
			continue
		}
		if c.Kind == SelectSend {
			ok, _ := c.Channel.TrySend(c.Value)
			if ok {
				return i, nil, true
			}
		} else {
			v, received, ok := c.Channel.TryReceive()
			if received {
				return i, v, ok
			}
		}
	}

	// Check for default case
	for i, c := range cases {
		if c.Kind == SelectDefault {
			return i, nil, true
		}
	}

	// No default case — block using reflect.Select for true multiplexing
	// Build reflect.SelectCase slice, tracking original indices
	selectCases := make([]reflect.SelectCase, 0, len(cases))
	originalIndices := make([]int, 0, len(cases))
	for i, c := range cases {
		if c.Kind == SelectDefault {
			continue
		}
		var rc reflect.SelectCase
		if c.Kind == SelectSend {
			rc = reflect.SelectCase{
				Dir:  reflect.SelectSend,
				Chan: reflect.ValueOf(c.Channel.ch),
				Send: reflect.ValueOf(c.Value),
			}
		} else {
			rc = reflect.SelectCase{
				Dir:  reflect.SelectRecv,
				Chan: reflect.ValueOf(c.Channel.ch),
			}
		}
		selectCases = append(selectCases, rc)
		originalIndices = append(originalIndices, i)
	}

	// reflect.Select panics with "send on closed channel" if a send case
	// targets a channel closed between our TrySend check and here (TOCTOU).
	// Recover and report the closed-channel case instead of crashing.
	defer func() {
		r := recover()
		if r != nil {
			// A send on a concurrently-closed channel panicked.
			// Find the first closed send case and report it.
			for i, c := range cases {
				if c.Kind == SelectSend && c.Channel.IsClosed() {
					idx = i
					val = nil
					ok = false
					return
				}
			}
			// Not a closed-channel panic — re-panic.
			panic(r)
		}
	}()

	chosen, recv, recvOK := reflect.Select(selectCases)
	idx = originalIndices[chosen]
	if cases[idx].Kind == SelectSend {
		return idx, nil, true
	}
	if !recvOK {
		return idx, nil, false
	}
	return idx, recv.Interface().(Value), true
}
