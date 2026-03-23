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
	_ Value = (*RWMutex)(nil)

	// RWMutex ID counter
	rwMutexIDCounter atomic.Uint64
)

// RWMutex wraps sync.RWMutex for Scheme
type RWMutex struct {
	id   uint64
	name string
	mu   sync.RWMutex
}

// NewRWMutex creates a new RWMutex
func NewRWMutex(name string) *RWMutex {
	id := rwMutexIDCounter.Add(1)
	if name == "" {
		name = fmt.Sprintf("rwmutex-%d", id)
	}
	return &RWMutex{
		id:   id,
		name: name,
	}
}

// ID returns the RWMutex's unique identifier
func (p *RWMutex) ID() uint64 {
	return p.id
}

// Name returns the RWMutex's name
func (p *RWMutex) Name() string {
	return p.name
}

// Lock acquires the write lock
func (p *RWMutex) Lock() {
	p.mu.Lock()
}

// Unlock releases the write lock
func (p *RWMutex) Unlock() {
	p.mu.Unlock()
}

// RLock acquires the read lock
func (p *RWMutex) RLock() {
	p.mu.RLock()
}

// RUnlock releases the read lock
func (p *RWMutex) RUnlock() {
	p.mu.RUnlock()
}

// TryLock tries to acquire the write lock without blocking
func (p *RWMutex) TryLock() bool {
	return p.mu.TryLock()
}

// TryRLock tries to acquire the read lock without blocking
func (p *RWMutex) TryRLock() bool {
	return p.mu.TryRLock()
}

// buf interface implementation

// IsVoid returns true if the RWMutex is nil.
func (p *RWMutex) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the RWMutexes are the same object.
func (p *RWMutex) EqualTo(v Value) bool {
	other, ok := v.(*RWMutex)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString returns the Scheme representation of the RWMutex.
func (p *RWMutex) SchemeString() string {
	if p == nil {
		return "#<rw-mutex:void>"
	}
	return fmt.Sprintf("#<rw-mutex:%s id=%d>", p.name, p.id)
}
