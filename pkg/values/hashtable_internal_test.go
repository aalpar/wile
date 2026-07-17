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
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestHashtableSizeHintClampsNegativeDrift pins the crash fix for the lock-free
// size counter. size is best-effort and can drift below zero: two concurrent
// Deletes of the same single-entry key both find it and both size.Add(-1),
// leaving size == -1. A negative value must never reach make's capacity argument
// (make([]T, 0, -1) panics "makeslice: cap out of range" and crashes the host,
// the exact failure the lock-free rewrite exists to avoid) nor surface as a
// negative count to Scheme. sizeHint clamps it at every read site.
func TestHashtableSizeHintClampsNegativeDrift(t *testing.T) {
	c := qt.New(t)

	p := NewEmptyHashtable()
	p.size.Store(-1)

	c.Assert(p.sizeHint(), qt.Equals, 0)
	c.Assert(p.Size(), qt.Equals, 0)

	// snapshot() feeds size to make() as a capacity hint. Before the clamp this
	// panicked on the negative value; it must now return cleanly.
	c.Assert(p.snapshot(), qt.HasLen, 0)
}
