// Copyright 2025 Aaron Alpar
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
	"sync"
	"sync/atomic"
)

var (
	_ Value = (*Channel)(nil)

	// Channel ID counter
	channelIDCounter uint64

	// ErrChannelClosed is returned when operating on a closed channel
	ErrChannelClosed = NewStaticError("channel is closed")
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
	id := atomic.AddUint64(&channelIDCounter, 1)
	return &Channel{
		id:         id,
		bufferSize: bufferSize,
		ch:         make(chan Value, bufferSize),
	}
}

// ID returns the channel's unique identifier
func (c *Channel) ID() uint64 {
	return c.id
}

// BufferSize returns the channel's buffer size
func (c *Channel) BufferSize() int {
	return c.bufferSize
}

// Send sends a value on the channel (blocking)
func (c *Channel) Send(v Value) error {
	c.mu.RLock()
	if c.closed {
		c.mu.RUnlock()
		return ErrChannelClosed
	}
	ch := c.ch
	c.mu.RUnlock()

	ch <- v
	return nil
}

// TrySend attempts to send a value without blocking
// Returns true if sent, false if would block
func (c *Channel) TrySend(v Value) (bool, error) {
	c.mu.RLock()
	if c.closed {
		c.mu.RUnlock()
		return false, ErrChannelClosed
	}
	ch := c.ch
	c.mu.RUnlock()

	select {
	case ch <- v:
		return true, nil
	default:
		return false, nil
	}
}

// Receive receives a value from the channel (blocking)
// Returns the value and true, or nil and false if channel is closed
func (c *Channel) Receive() (Value, bool) {
	c.mu.RLock()
	ch := c.ch
	c.mu.RUnlock()

	v, ok := <-ch
	return v, ok
}

// TryReceive attempts to receive a value without blocking
// Returns (value, true, true) if received
// Returns (nil, false, true) if would block
// Returns (nil, false, false) if channel is closed
func (c *Channel) TryReceive() (Value, bool, bool) {
	c.mu.RLock()
	ch := c.ch
	closed := c.closed
	c.mu.RUnlock()

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
func (c *Channel) Close() error {
	c.mu.Lock()
	defer c.mu.Unlock()

	if c.closed {
		return ErrChannelClosed
	}
	c.closed = true
	close(c.ch)
	return nil
}

// IsClosed returns true if the channel is closed
func (c *Channel) IsClosed() bool {
	c.mu.RLock()
	defer c.mu.RUnlock()
	return c.closed
}

// Len returns the number of elements queued in the channel
func (c *Channel) Len() int {
	c.mu.RLock()
	defer c.mu.RUnlock()
	return len(c.ch)
}

// Cap returns the channel's capacity
func (c *Channel) Cap() int {
	return c.bufferSize
}

// Chan returns the underlying Go channel for use in select statements
func (c *Channel) Chan() chan Value {
	c.mu.RLock()
	defer c.mu.RUnlock()
	return c.ch
}

// Value interface implementation

func (c *Channel) IsVoid() bool {
	return c == nil
}

func (c *Channel) EqualTo(v Value) bool {
	other, ok := v.(*Channel)
	if !ok {
		return false
	}
	return c == other // Identity is reference equality
}

func (c *Channel) SchemeString() string {
	if c == nil {
		return "#<channel:void>"
	}
	c.mu.RLock()
	defer c.mu.RUnlock()
	status := "open"
	if c.closed {
		status = "closed"
	}
	if c.bufferSize == 0 {
		return fmt.Sprintf("#<channel:unbuffered id=%d %s>", c.id, status)
	}
	return fmt.Sprintf("#<channel:buffered[%d] id=%d %s len=%d>", c.bufferSize, c.id, status, len(c.ch))
}

// SelectCase represents a case in a channel select operation
type SelectCase struct {
	Channel   *Channel
	Value     Value // for send operations
	IsSend    bool
	IsDefault bool
}

// ChannelSelect performs a select operation on multiple channels
// Returns the index of the selected case and the received value (for receive cases)
func ChannelSelect(cases []SelectCase) (int, Value, bool) {
	if len(cases) == 0 {
		return -1, nil, false
	}

	// Build native select cases
	// This is a simplified implementation that polls
	// For a more efficient implementation, we'd use reflect.Select

	// First pass: try non-blocking operations
	for i, c := range cases {
		if c.IsDefault {
			continue
		}
		if c.IsSend {
			if ok, _ := c.Channel.TrySend(c.Value); ok {
				return i, nil, true
			}
		} else {
			if v, received, ok := c.Channel.TryReceive(); received {
				return i, v, ok
			}
		}
	}

	// Check for default case
	for i, c := range cases {
		if c.IsDefault {
			return i, nil, true
		}
	}

	// No default case - block on first available
	// This is a simplified implementation
	// A real implementation would use reflect.Select for true multiplexing
	for {
		for i, c := range cases {
			if c.IsDefault {
				continue
			}
			if c.IsSend {
				if ok, _ := c.Channel.TrySend(c.Value); ok {
					return i, nil, true
				}
			} else {
				if v, received, ok := c.Channel.TryReceive(); received {
					return i, v, ok
				}
			}
		}
	}
}
