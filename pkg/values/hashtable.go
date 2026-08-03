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
	"cmp"
	"slices"
	"strings"
	"sync"
	"sync/atomic"

	"github.com/aalpar/wile/pkg/werr"
)

var _ Value = (*Hashtable)(nil)

// hashtableEntry stores a key-value pair in the hash table.
type hashtableEntry struct {
	key   Hashable
	value Value
}

// Hashtable represents a Scheme hash table mapping hashable values to values.
//
// Separate chaining (Cormen et al., CLRS Ch. 11): collisions are resolved
// by storing all entries with the same hash in a linked list (here, a Go
// slice). O(1) amortized with a good hash function.
// See BIBLIOGRAPHY.md "Separate Chaining Hash Table".
//
// Keys must implement the Hashable interface (Value + HashCode()).
// Uses bucket chaining with FNV-1a hashing for O(1) amortized operations
// and EqualTo() for key comparison within buckets.
//
// Concurrency: LOCK-FREE, by design. A Hashtable is user-owned data that
// SRFI-18 threads may share; per the concurrency ownership line, the USER
// synchronizes it for atomic multi-step sequences or a consistent iteration
// snapshot. The type itself carries no mutex — but it must never CRASH the
// host on concurrent access, so the backing store is a sync.Map (key: uint64
// hash → an IMMUTABLE []hashtableEntry bucket) with copy-on-write writes:
//   - Reads Load an immutable bucket and scan it — no lock, no data race.
//   - Writes (Set/Delete) Load the bucket, COPY it, mutate the copy, Store the
//     new slice. Buckets are never mutated in place, so the inner-slice race is
//     gone too. Both halves (sync.Map AND copy) are required: sync.Map alone
//     would leave an in-place slice append/overwrite racing.
//
// The lock-free store removes Go's fatal "concurrent map read and map write";
// what it does NOT provide is transactional atomicity. Under UNSYNCHRONIZED
// concurrent writers to one bucket a Set may be lost (last-Store-wins) and the
// atomic size may drift — that is the accepted consequence of the user not
// synchronizing their own shared data. Single-threaded use is exact.
type Hashtable struct {
	// buckets maps a uint64 hash to an immutable []hashtableEntry snapshot.
	// Never mutate a stored bucket in place; replace it via Store (copy-on-write).
	buckets sync.Map
	// size is the entry count, maintained lock-free. Exact single-threaded;
	// best-effort under unsynchronized concurrent mutation.
	size atomic.Int64
}

// NewEmptyHashtable creates a new empty hash table. The zero sync.Map and
// atomic.Int64 are ready to use, so no field initialization is needed.
func NewEmptyHashtable() *Hashtable {
	return &Hashtable{}
}

// IsVoid returns true if this hash table is nil.
func (p *Hashtable) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both hash tables have equal contents.
//
// Keys and values take different routes, and the asymmetry is deliberate: keys
// are matched inside EqualComponents (a key is always a leaf — see below), while
// values are pushed onto Equal's iterative worklist, since a value may be a
// container and may be cyclic.
func (p *Hashtable) EqualTo(o Value) bool {
	return Equal(p, o)
}

// EqualComponents pairs this table's entries against the other's by key, then
// pushes the matched VALUES for Equal to compare. Keys are matched here rather
// than pushed because a key is always a leaf: no container type implements
// Hashable, so a key cannot carry a cycle. TestNoContainerIsHashable pins that
// invariant — adding HashCode() to *Pair or *Vector (what R6RS
// make-equal-hashtable wants) would put recursion back on the Go stack here.
//
// Both tables are read through lock-free snapshots, so no lock is held during
// the comparison and two tables never contend.
func (p *Hashtable) EqualComponents(o Value, push func(a, b Value)) bool {
	v, ok := o.(*Hashtable)
	if !ok {
		return false
	}
	if p == nil || v == nil {
		return p == v
	}
	entries := p.snapshot()
	if len(entries) != v.Size() {
		return false
	}
	for _, entry := range entries {
		vval, found := v.get(entry.key)
		if !found {
			return false
		}
		push(entry.value, vval)
	}
	return true
}

// SchemeString returns the Scheme representation of this hash table.
func (p *Hashtable) SchemeString() string {
	return p.schemeStringWithVisited(make(map[Value]bool), 1)
}

// schemeStringWithVisited renders the hash table using PATH-SCOPED cycle
// detection threaded through nested Pair/Vector/Hashtable values, so a cycle
// that passes through a hashtable (e.g. a pair whose cdr is a hashtable whose
// value is that pair) is bounded with "..." instead of overflowing the Go
// stack. The hashtable marks itself on entry and unmarks on exit; keys and
// values recurse via schemeStringChild, which applies the same discipline.
//
// depth is this hashtable's nesting level (root = 1); keys and values sit one
// level deeper (depth+1), where schemeStringChild enforces the host-safety
// bound. Entries are siblings at the same level, so table size does not consume
// depth.
func (p *Hashtable) schemeStringWithVisited(visited map[Value]bool, depth int) string {
	if visited[p] {
		return "..."
	}
	visited[p] = true
	defer func() {
		delete(visited, p)
	}()

	q := &strings.Builder{}
	q.WriteString("#hash(")
	entries := p.snapshot()
	slices.SortFunc(entries, func(a, b hashtableEntry) int {
		return cmp.Compare(a.key.SchemeString(), b.key.SchemeString())
	})
	for i, e := range entries {
		if i > 0 {
			q.WriteString(" ")
		}
		q.WriteString("(")
		q.WriteString(schemeStringChild(e.key, visited, depth+1))
		q.WriteString(" . ")
		q.WriteString(schemeStringChild(e.value, visited, depth+1))
		q.WriteString(")")
	}
	q.WriteString(")")
	return q.String()
}

// loadBucket returns the immutable bucket stored under h, or nil if absent.
// The returned slice must NOT be mutated — writers copy before changing.
func (p *Hashtable) loadBucket(h uint64) []hashtableEntry {
	v, ok := p.buckets.Load(h)
	if !ok {
		return nil
	}
	return v.([]hashtableEntry)
}

// get is the internal lookup used by EqualComponents and other methods.
func (p *Hashtable) get(key Hashable) (Value, bool) {
	for _, e := range p.loadBucket(key.HashCode()) {
		if e.key.EqualTo(key) {
			return e.value, true
		}
	}
	return nil, false
}

// sizeHint returns the entry count clamped to be non-negative. size is a
// best-effort atomic that can drift negative under unsynchronized concurrent
// deletes of the same key (two deletes of one key both decrement), and a
// negative value must never reach make's capacity argument, which panics and
// crashes the host — the exact failure the lock-free rewrite exists to avoid.
func (p *Hashtable) sizeHint() int {
	n := p.size.Load()
	if n < 0 {
		return 0
	}
	return int(n)
}

// snapshot copies every entry out of the sync.Map. Callers that must run user
// code, render nested values, or touch a second table do so against the
// snapshot. Buckets are immutable, so this never races a concurrent writer.
func (p *Hashtable) snapshot() []hashtableEntry {
	q := make([]hashtableEntry, 0, p.sizeHint())
	p.buckets.Range(func(_, v any) bool {
		q = append(q, v.([]hashtableEntry)...)
		return true
	})
	return q
}

// Get retrieves the value associated with key.
// Returns the value and whether the key was found.
// Returns werr.ErrInvalidArgument if the key does not implement Hashable.
func (p *Hashtable) Get(key Value) (Value, bool, error) {
	hk, ok := key.(Hashable)
	if !ok {
		return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "hashtable: key is not hashable: %s", key.SchemeString())
	}
	val, found := p.get(hk)
	return val, found, nil
}

// Set associates key with val in the hash table.
// Returns werr.ErrInvalidArgument if the key does not implement Hashable.
//
// Copy-on-write: the target bucket is copied before it is changed, so a
// concurrent reader scanning the old bucket is never disturbed. See the type
// comment for the (non-transactional) concurrency contract.
func (p *Hashtable) Set(key Value, val Value) error {
	hk, ok := key.(Hashable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "hashtable: key is not hashable: %s", key.SchemeString())
	}
	h := hk.HashCode()
	old := p.loadBucket(h)
	for i, e := range old {
		if e.key.EqualTo(hk) {
			nb := make([]hashtableEntry, len(old))
			copy(nb, old)
			nb[i] = hashtableEntry{key: hk, value: val}
			p.buckets.Store(h, nb)
			return nil
		}
	}
	nb := make([]hashtableEntry, len(old), len(old)+1)
	copy(nb, old)
	nb = append(nb, hashtableEntry{key: hk, value: val})
	p.buckets.Store(h, nb)
	p.size.Add(1)
	return nil
}

// HasKey returns whether the key exists in the hash table.
// Returns werr.ErrInvalidArgument if the key does not implement Hashable.
func (p *Hashtable) HasKey(key Value) (bool, error) {
	hk, ok := key.(Hashable)
	if !ok {
		return false, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "hashtable: key is not hashable: %s", key.SchemeString())
	}
	_, found := p.get(hk)
	return found, nil
}

// Delete removes the entry for key from the hash table.
// Returns werr.ErrInvalidArgument if the key does not implement Hashable.
//
// Copy-on-write: a shrunk bucket is a fresh slice; the last entry's removal
// drops the bucket key entirely.
func (p *Hashtable) Delete(key Value) error {
	hk, ok := key.(Hashable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "hashtable: key is not hashable: %s", key.SchemeString())
	}
	h := hk.HashCode()
	old := p.loadBucket(h)
	for i, e := range old {
		if e.key.EqualTo(hk) {
			if len(old) == 1 {
				p.buckets.Delete(h)
			} else {
				nb := make([]hashtableEntry, 0, len(old)-1)
				nb = append(nb, old[:i]...)
				nb = append(nb, old[i+1:]...)
				p.buckets.Store(h, nb)
			}
			p.size.Add(-1)
			return nil
		}
	}
	return nil
}

// collectEntries walks every bucket and projects each entry to a Value,
// returning the projections as a proper list. Keys and Values differ only
// in which entry field they read.
func (p *Hashtable) collectEntries(project func(e hashtableEntry) Value) Tuple {
	entries := p.snapshot()
	if len(entries) == 0 {
		return EmptyList
	}
	out := make([]Value, 0, len(entries))
	for _, e := range entries {
		out = append(out, project(e))
	}
	return List(out...)
}

// Keys returns a list of all keys in the hash table.
func (p *Hashtable) Keys() Tuple {
	return p.collectEntries(func(e hashtableEntry) Value {
		return e.key
	})
}

// Values returns a list of all values in the hash table.
func (p *Hashtable) Values() Tuple {
	return p.collectEntries(func(e hashtableEntry) Value {
		return e.value
	})
}

// Size returns the number of entries in the hash table. Exact single-threaded;
// best-effort under unsynchronized concurrent mutation, and never negative even
// when the counter has drifted below zero.
func (p *Hashtable) Size() int {
	return p.sizeHint()
}

// Copy returns a shallow copy of the hash table. Buckets are immutable, so each
// stored slice can be shared directly with the copy without re-copying.
func (p *Hashtable) Copy() *Hashtable {
	q := NewEmptyHashtable()
	p.buckets.Range(func(k, v any) bool {
		q.buckets.Store(k, v.([]hashtableEntry))
		return true
	})
	q.size.Store(p.size.Load())
	return q
}

// Clear removes all entries from the hash table.
func (p *Hashtable) Clear() {
	p.buckets.Clear()
	p.size.Store(0)
}

// Entries iterates over all entries in the hash table, calling fn for each
// key-value pair. Iteration stops early if fn returns a non-nil error.
// This is more efficient than Keys()+Get() as it avoids intermediate allocations.
//
// fn runs against a snapshot: it may be Scheme code that reads or mutates this
// same table (hashtable-walk). The snapshot is the iteration's view; entries
// added concurrently are not visited.
func (p *Hashtable) Entries(fn func(key Hashable, value Value) error) error {
	for _, e := range p.snapshot() {
		err := fn(e.key, e.value)
		if err != nil {
			return err
		}
	}
	return nil
}
