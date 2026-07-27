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
	"sync"
	"sync/atomic"
)

var (
	_ Value = (*Once)(nil)

	// onceIDCounter assigns each Once a unique ID.
	onceIDCounter atomic.Uint64
)

// Once wraps sync.Once for Scheme
type Once struct {
	id   uint64
	once sync.Once
	done atomic.Uint32 // tracks if Do was called
}

// NewOnce creates a new Once
func NewOnce() *Once {
	id := onceIDCounter.Add(1)
	return &Once{id: id}
}

// ID returns the Once's unique identifier
func (p *Once) ID() uint64 {
	return p.id
}

// Do calls the function only once
// Returns true if this call executed the function, false if it was already called
func (p *Once) Do(f func()) bool {
	called := false
	p.once.Do(func() {
		called = true
		f()
		p.done.Store(1)
	})
	return called
}

// Done reports whether Do's function has run to completion. It is false while
// the function is executing, and stays false if the function panicked, even
// though the Once is then consumed and further Do calls return false.
func (p *Once) Done() bool {
	return p.done.Load() == 1
}

// buf interface implementation

// IsVoid returns true if the once is nil.
func (p *Once) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the onces are the same object.
func (p *Once) EqualTo(v Value) bool {
	other, ok := v.(*Once)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString returns the Scheme representation of the once.
func (p *Once) SchemeString() string {
	if p == nil {
		return "#<once:void>"
	}
	status := "pending"
	if p.Done() {
		status = "done"
	}
	return fmt.Sprintf("#<once id=%d %s>", p.id, status)
}
