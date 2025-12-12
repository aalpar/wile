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
func (wg *WaitGroup) ID() uint64 {
	return wg.id
}

// Add adds delta to the counter
func (wg *WaitGroup) Add(delta int) {
	wg.wg.Add(delta)
}

// Done decrements the counter by one
func (wg *WaitGroup) Done() {
	wg.wg.Done()
}

// Wait blocks until the counter is zero
func (wg *WaitGroup) Wait() {
	wg.wg.Wait()
}

// Value interface implementation

func (wg *WaitGroup) IsVoid() bool {
	return wg == nil
}

func (wg *WaitGroup) EqualTo(v Value) bool {
	other, ok := v.(*WaitGroup)
	if !ok {
		return false
	}
	return wg == other
}

func (wg *WaitGroup) SchemeString() string {
	if wg == nil {
		return "#<wait-group:void>"
	}
	return fmt.Sprintf("#<wait-group id=%d>", wg.id)
}
