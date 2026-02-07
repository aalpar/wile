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
	"sort"
	"strings"
)

var _ Value = (*Hashtable)(nil)

// hashtableEntry stores a key-value pair in the hash table.
type hashtableEntry struct {
	key   Hashable
	value Value
}

// Hashtable represents a Scheme hash table mapping hashable values to values.
//
// Keys must implement the Hashable interface (Value + HashCode()).
// Uses bucket chaining with FNV-1a hashing for O(1) amortized operations
// and EqualTo() for key comparison within buckets.
type Hashtable struct {
	buckets map[uint64][]hashtableEntry
	size    int
}

// NewEmptyHashtable creates a new empty hash table.
func NewEmptyHashtable() *Hashtable {
	q := &Hashtable{
		buckets: make(map[uint64][]hashtableEntry),
	}
	return q
}

// IsVoid returns true if this hash table is nil.
func (p *Hashtable) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both hash tables have equal contents.
// Uses structural equality (EqualTo) for both keys and values.
func (p *Hashtable) EqualTo(o Value) bool {
	v, ok := o.(*Hashtable)
	if !ok {
		return false
	}
	if p.size != v.size {
		return false
	}
	for _, bucket := range p.buckets {
		for _, entry := range bucket {
			vval, found := v.get(entry.key)
			if !found {
				return false
			}
			if !entry.value.EqualTo(vval) {
				return false
			}
		}
	}
	return true
}

// SchemeString returns the Scheme representation of this hash table.
func (p *Hashtable) SchemeString() string {
	q := &strings.Builder{}
	q.WriteString("#hash(")
	// Collect all entries and sort for deterministic output.
	entries := make([]hashtableEntry, 0, p.size)
	for _, bucket := range p.buckets {
		entries = append(entries, bucket...)
	}
	sort.Slice(entries, func(i, j int) bool {
		return entries[i].key.SchemeString() < entries[j].key.SchemeString()
	})
	for i, e := range entries {
		if i > 0 {
			q.WriteString(" ")
		}
		q.WriteString("(")
		q.WriteString(e.key.SchemeString())
		q.WriteString(" . ")
		q.WriteString(e.value.SchemeString())
		q.WriteString(")")
	}
	q.WriteString(")")
	return q.String()
}

// get is the internal lookup used by EqualTo and other methods.
func (p *Hashtable) get(key Hashable) (Value, bool) {
	h := key.HashCode()
	bucket := p.buckets[h]
	for _, e := range bucket {
		if e.key.EqualTo(key) {
			return e.value, true
		}
	}
	return nil, false
}

// Get retrieves the value associated with key.
// Returns the value and whether the key was found.
// Returns ErrInvalidArgument if the key does not implement Hashable.
func (p *Hashtable) Get(key Value) (Value, bool, error) {
	hk, ok := key.(Hashable)
	if !ok {
		return nil, false, WrapForeignErrorf(ErrInvalidArgument, "hashtable: key is not hashable: %s", key.SchemeString())
	}
	val, found := p.get(hk)
	return val, found, nil
}

// Set associates key with val in the hash table.
// Returns ErrInvalidArgument if the key does not implement Hashable.
func (p *Hashtable) Set(key Value, val Value) error {
	hk, ok := key.(Hashable)
	if !ok {
		return WrapForeignErrorf(ErrInvalidArgument, "hashtable: key is not hashable: %s", key.SchemeString())
	}
	h := hk.HashCode()
	bucket := p.buckets[h]
	for i, e := range bucket {
		if e.key.EqualTo(hk) {
			p.buckets[h][i].value = val
			return nil
		}
	}
	p.buckets[h] = append(bucket, hashtableEntry{key: hk, value: val})
	p.size++
	return nil
}

// HasKey returns whether the key exists in the hash table.
// Returns ErrInvalidArgument if the key does not implement Hashable.
func (p *Hashtable) HasKey(key Value) (bool, error) {
	hk, ok := key.(Hashable)
	if !ok {
		return false, WrapForeignErrorf(ErrInvalidArgument, "hashtable: key is not hashable: %s", key.SchemeString())
	}
	_, found := p.get(hk)
	return found, nil
}

// Delete removes the entry for key from the hash table.
// Returns ErrInvalidArgument if the key does not implement Hashable.
func (p *Hashtable) Delete(key Value) error {
	hk, ok := key.(Hashable)
	if !ok {
		return WrapForeignErrorf(ErrInvalidArgument, "hashtable: key is not hashable: %s", key.SchemeString())
	}
	h := hk.HashCode()
	bucket := p.buckets[h]
	for i, e := range bucket {
		if e.key.EqualTo(hk) {
			p.buckets[h] = append(bucket[:i], bucket[i+1:]...)
			p.size--
			if len(p.buckets[h]) == 0 {
				delete(p.buckets, h)
			}
			return nil
		}
	}
	return nil
}

// Keys returns a list of all keys in the hash table.
func (p *Hashtable) Keys() Tuple {
	if p.size == 0 {
		return EmptyList
	}
	keys := make([]Value, 0, p.size)
	for _, bucket := range p.buckets {
		for _, e := range bucket {
			keys = append(keys, e.key)
		}
	}
	return List(keys...)
}

// Values returns a list of all values in the hash table.
func (p *Hashtable) Values() Tuple {
	if p.size == 0 {
		return EmptyList
	}
	vals := make([]Value, 0, p.size)
	for _, bucket := range p.buckets {
		for _, e := range bucket {
			vals = append(vals, e.value)
		}
	}
	return List(vals...)
}

// Size returns the number of entries in the hash table.
func (p *Hashtable) Size() int {
	return p.size
}

// Copy returns a shallow copy of the hash table.
func (p *Hashtable) Copy() *Hashtable {
	q := &Hashtable{
		buckets: make(map[uint64][]hashtableEntry, len(p.buckets)),
		size:    p.size,
	}
	for h, bucket := range p.buckets {
		cp := make([]hashtableEntry, len(bucket))
		copy(cp, bucket)
		q.buckets[h] = cp
	}
	return q
}

// Clear removes all entries from the hash table.
func (p *Hashtable) Clear() {
	p.buckets = make(map[uint64][]hashtableEntry)
	p.size = 0
}
