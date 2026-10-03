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

// TestHashtableCopyResumesTheOrdinalCounter pins nextSeq's carry in Copy, which
// had no test at all: deleting the Store failed nothing across pkg/values,
// pkg/wile and pkg/registry/core.
//
// Asserted on the counter directly, in-package, rather than through a rendered
// table. seq is only the TIE-BREAKER in sortHashtableEntries, applied after the
// keys' SchemeString comparison, so a collision does not produce a wrong value
// — it makes the comparator non-total and hands an unstable sort a tie, which
// is a determinism loss too delicate to assert from outside. The invariant
// itself is one comparison: a copy shares the source's buckets by reference, so
// its counter must resume ABOVE every ordinal already in them or a later insert
// ties with a live entry.
func TestHashtableCopyResumesTheOrdinalCounter(t *testing.T) {
	c := qt.New(t)

	p := NewEmptyHashtable()
	for _, k := range []string{"a", "b", "c"} {
		p.Set(NewSymbol(k), NewInteger(1))
	}
	issued := p.nextSeq.Load()
	c.Assert(issued, qt.Equals, uint64(3), qt.Commentf("three inserts, three ordinals"))

	q := p.Copy(true)
	c.Assert(q.nextSeq.Load(), qt.Equals, issued,
		qt.Commentf("the copy shares the source's buckets, so its counter must resume above their ordinals"))

	// And the next insert into the copy genuinely lands past them, rather than
	// the counter merely holding the right number.
	q.Set(NewSymbol("d"), NewInteger(2))
	c.Assert(q.nextSeq.Load() > issued, qt.IsTrue)
}
