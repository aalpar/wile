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
	_ Value = (*Once)(nil)

	// Once ID counter
	onceIDCounter uint64
)

// Once wraps sync.Once for Scheme
type Once struct {
	id   uint64
	once sync.Once
	done uint32 // atomic flag to track if Do was called
}

// NewOnce creates a new Once
func NewOnce() *Once {
	id := atomic.AddUint64(&onceIDCounter, 1)
	return &Once{id: id}
}

// ID returns the Once's unique identifier
func (o *Once) ID() uint64 {
	return o.id
}

// Do calls the function only once
// Returns true if this call executed the function, false if it was already called
func (o *Once) Do(f func()) bool {
	called := false
	o.once.Do(func() {
		atomic.StoreUint32(&o.done, 1)
		called = true
		f()
	})
	return called
}

// Done returns true if Do has been called
func (o *Once) Done() bool {
	return atomic.LoadUint32(&o.done) == 1
}

// Value interface implementation

func (o *Once) IsVoid() bool {
	return o == nil
}

func (o *Once) EqualTo(v Value) bool {
	other, ok := v.(*Once)
	if !ok {
		return false
	}
	return o == other
}

func (o *Once) SchemeString() string {
	if o == nil {
		return "#<once:void>"
	}
	status := "pending"
	if o.Done() {
		status = "done"
	}
	return fmt.Sprintf("#<once id=%d %s>", o.id, status)
}
