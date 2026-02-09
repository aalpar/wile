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
	_ Value = (*WaitGroup)(nil)

	// WaitGroup ID counter
	waitGroupIDCounter uint64
)

// WaitGroup wraps sync.WaitGroup for Scheme
type WaitGroup struct {
	id uint64
	wg sync.WaitGroup
}

// NewWaitGroup creates a new WaitGroup
func NewWaitGroup() *WaitGroup {
	id := atomic.AddUint64(&waitGroupIDCounter, 1)
	return &WaitGroup{id: id}
}

// ID returns the WaitGroup's unique identifier
func (p *WaitGroup) ID() uint64 {
	return p.id
}

// Add adds delta to the counter
func (p *WaitGroup) Add(delta int) {
	p.wg.Add(delta)
}

// Done decrements the counter by one
func (p *WaitGroup) Done() {
	p.wg.Done()
}

// Wait blocks until the counter is zero
func (p *WaitGroup) Wait() {
	p.wg.Wait()
}

// buf interface implementation

// IsVoid returns true if the wait group is nil.
func (p *WaitGroup) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the wait groups are the same object.
func (p *WaitGroup) EqualTo(v Value) bool {
	other, ok := v.(*WaitGroup)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString returns the Scheme representation of the wait group.
func (p *WaitGroup) SchemeString() string {
	if p == nil {
		return "#<wait-group:void>"
	}
	return fmt.Sprintf("#<wait-group id=%d>", p.id)
}
