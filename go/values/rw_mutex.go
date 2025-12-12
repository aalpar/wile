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
	_ Value = (*RWMutex)(nil)

	// RWMutex ID counter
	rwMutexIDCounter uint64
)

// RWMutex wraps sync.RWMutex for Scheme
type RWMutex struct {
	id   uint64
	name string
	mu   sync.RWMutex
}

// NewRWMutex creates a new RWMutex
func NewRWMutex(name string) *RWMutex {
	id := atomic.AddUint64(&rwMutexIDCounter, 1)
	if name == "" {
		name = fmt.Sprintf("rwmutex-%d", id)
	}
	return &RWMutex{
		id:   id,
		name: name,
	}
}

// ID returns the RWMutex's unique identifier
func (m *RWMutex) ID() uint64 {
	return m.id
}

// Name returns the RWMutex's name
func (m *RWMutex) Name() string {
	return m.name
}

// Lock acquires the write lock
func (m *RWMutex) Lock() {
	m.mu.Lock()
}

// Unlock releases the write lock
func (m *RWMutex) Unlock() {
	m.mu.Unlock()
}

// RLock acquires the read lock
func (m *RWMutex) RLock() {
	m.mu.RLock()
}

// RUnlock releases the read lock
func (m *RWMutex) RUnlock() {
	m.mu.RUnlock()
}

// TryLock tries to acquire the write lock without blocking
func (m *RWMutex) TryLock() bool {
	return m.mu.TryLock()
}

// TryRLock tries to acquire the read lock without blocking
func (m *RWMutex) TryRLock() bool {
	return m.mu.TryRLock()
}

// Value interface implementation

func (m *RWMutex) IsVoid() bool {
	return m == nil
}

func (m *RWMutex) EqualTo(v Value) bool {
	other, ok := v.(*RWMutex)
	if !ok {
		return false
	}
	return m == other
}

func (m *RWMutex) SchemeString() string {
	if m == nil {
		return "#<rw-mutex:void>"
	}
	return fmt.Sprintf("#<rw-mutex:%s id=%d>", m.name, m.id)
}
