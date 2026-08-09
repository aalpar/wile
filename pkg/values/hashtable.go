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
	"strings"
	"sync"
	"sync/atomic"

	"github.com/aalpar/wile/pkg/werr"
)

var _ Value = (*Hashtable)(nil)

// hashtableEntry stores a key-value pair in the hash table. key is a bare Value,
// not a Hashable: since HashtableKind moved the hash from the key to the table,
// any object can be any table's key.
type hashtableEntry struct {
	key   Value
	value Value
	// seq is the entry's insertion ordinal, drawn from the owning table's
	// counter when the key was FIRST added and preserved when its value is
	// overwritten. It exists because rendering needs a total order and the
	// store cannot supply one: sync.Map.Range is a per-process seeded walk, and
	// the bucket hash is the key's POINTER for an eq table (identityHash), so
	// neither is stable across processes. Insertion order is the only ordering
	// information a table holds that is a function of the program rather than
	// of the run. See SchemeWriter.writeHashtable.
	seq uint64
}

// HashtableKind selects which (hash, key-equality) pair a table uses. It is the
// R6RS inversion: the hash belongs to the TABLE, not to the key, so any object can
// be a key of any table.
//
// HashtableEqual is deliberately the ZERO VALUE, so that a table whose kind was
// never set keeps the equal?-keyed semantics every table in the tree had before
// kinds existed. Reordering this iota silently reinterprets them. Pinned by
// TestHashtableZeroValueIsEqualKind.
//
// Do NOT read that as "a bare &Hashtable{} is a usable table". It is not, and an
// earlier version of this comment said it was: the mutable field's zero value is
// false, so a zero Hashtable is an IMMUTABLE equal table that rejects every
// write. Construct through NewHashtable or NewEmptyHashtable, which is what
// every site in the tree does.
type HashtableKind uint8

const (
	// HashtableEqual hashes with EqualHash and compares with Equal (equal?).
	HashtableEqual HashtableKind = iota
	// HashtableEq hashes by object identity and compares with EqIdentity (eq?).
	HashtableEq
	// HashtableEqv hashes leaves by content and compares with Eqv (eqv?).
	HashtableEqv
)

// String renders the kind as its R6RS-facing name, for error messages and
// debugging. Nothing in the primitive surface returns it: hashtable-hash-function
// and hashtable-equivalence-function hand back procedure objects, not kind names.
// HashtableKindCount is the number of defined kinds. It exists as a ratchet:
// TestHashtableKindsAreExhaustive fails when a kind is added without giving it a
// row in hashKey, keyEqual, String and equivIdentity. Those four all used to reach
// an unlisted kind through a `default:` arm that answered EQUAL — so a new kind
// would silently have inherited equal? key identity, which is the wrong-key-
// collapse form of data corruption rather than a loud failure.
const HashtableKindCount = 3

// String renders the kind as its R6RS-facing name, for error messages and
// debugging. Nothing in the primitive surface returns it: hashtable-hash-function
// and hashtable-equivalence-function hand back procedure objects, not kind names.
func (p HashtableKind) String() string {
	switch p {
	case HashtableEqual:
		return "equal"
	case HashtableEq:
		return "eq"
	case HashtableEqv:
		return "eqv"
	default:
		return "unknown"
	}
}

// Hashtable represents a Scheme hash table mapping hashable values to values.
//
// Separate chaining (Cormen et al., CLRS Ch. 11): collisions are resolved
// by storing all entries with the same hash in a linked list (here, a Go
// slice). O(1) amortized with a good hash function.
// See BIBLIOGRAPHY.md "Separate Chaining Hash Table".
//
// ANY object can be a key. Which objects count as ONE key is the table's choice,
// carried by its HashtableKind: an equal table hashes with EqualHash and compares
// with Equal, an eq table by identity, an eqv table by Eqv. Uses bucket chaining
// with FNV-1a hashing for O(1) amortized operations.
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
	// nextSeq issues hashtableEntry.seq values. It only ever increases, so a
	// delete-then-reinsert files the key at the END of the insertion order
	// rather than reclaiming its old place — which is what makes the ordinal a
	// function of the write SEQUENCE and not of the live set. Copy carries it
	// forward so a copy's later inserts cannot collide with the shared buckets'
	// existing ordinals.
	nextSeq atomic.Uint64
	// kind is WRITE-ONCE at construction and never mutates, so it does not
	// participate in the copy-on-write dance and the lock-free contract above is
	// unaffected.
	kind HashtableKind
	// mutable is likewise WRITE-ONCE at construction. Only hashtable-copy
	// without a true second argument produces a false one.
	mutable bool
}

// NewEmptyHashtable creates a new empty MUTABLE equal?-keyed hash table. The
// zero sync.Map and atomic.Int64 are ready to use, and HashtableEqual is the
// zero HashtableKind by deliberate choice.
func NewEmptyHashtable() *Hashtable {
	return NewHashtable(HashtableEqual)
}

// NewHashtable creates an empty MUTABLE hash table using kind's hash and key
// equality. Immutable tables come only from hashtable-copy without a true
// second argument, so mutable is set explicitly here rather than relying on a
// zero value — unlike kind, whose zero value is deliberately the common case.
func NewHashtable(kind HashtableKind) *Hashtable {
	return &Hashtable{kind: kind, mutable: true}
}

// Mutable reports whether this table accepts Set, Delete, and Clear.
func (p *Hashtable) Mutable() bool {
	return p.mutable
}

// checkMutable is the guard every destructive method opens with.
func (p *Hashtable) checkMutable(op string) error {
	if p.mutable {
		return nil
	}
	return werr.WrapForeignErrorf(werr.ErrImmutableHashtable,
		"%s: table was created by hashtable-copy without a true mutable argument", op)
}

// Kind reports which (hash, key-equality) pair this table uses.
func (p *Hashtable) Kind() HashtableKind {
	return p.kind
}

// hashKey computes the bucket hash for key under this table's kind.
func (p *Hashtable) hashKey(key Value) uint64 {
	switch p.kind {
	case HashtableEq: //nolint:exhaustive // default is HashtableEqual; see HashtableKindCount
		// Symbols FIRST, and this is load-bearing: EqIdentity compares symbols by
		// Key (Wile de-interns them), so two eq? symbols are distinct pointers and
		// an identity hash would file them in different buckets — a lookup that
		// silently misses. Everything else eq? compares by pointer.
		sym, ok := key.(*Symbol)
		if ok {
			return sym.HashCode()
		}
		return identityHash(key)
	case HashtableEqv:
		// Any Hashable leaf's HashCode is content-canonical, which is FINER than
		// eqv? needs for strings (two distinct equal-content strings are not eqv?
		// but hash alike). That over-collides into one bucket where keyEqual then
		// separates them — correctness-preserving, since the hash contract is
		// one-directional. Numbers REQUIRE it: EqvNumber compares exact values
		// across Integer/BigInteger/Rational, so an identity hash would break them.
		hk, ok := key.(Hashable)
		if ok {
			return hk.HashCode()
		}
		return identityHash(key)
	default:
		// LEAF FAST PATH, and it is not just an optimization — it is what keeps
		// this change free for every table that existed before container keys
		// were legal. EqualHash on a Hashable leaf is exactly
		// mixHash(fnvOffset, HashCode()), so routing leaves through it bought a
		// slice header, a type switch and a multiply per lookup for no new
		// information: measured +76% on Get/symbol/n=10 and +40% on
		// Get/string/n=1000 against the pre-inversion baseline, at identical
		// allocations.
		//
		// Splitting the hash space by leaf-ness is sound because no Hashable
		// value is ever equal? to a non-Hashable one. equal? relates values
		// within a type family: no container implements Hashable
		// (TestNoContainerIsHashable), a *Record is equal? only to a *Record of
		// the same RecordType, and everything else falls to identity. So two
		// equal? keys are either both leaves — where the Hashable contract
		// already guarantees equal HashCodes — or both non-leaves, where
		// EqualHash does. They can never land on opposite sides of this branch.
		hk, ok := key.(Hashable)
		if ok {
			return hk.HashCode()
		}
		return EqualHash(key)
	}
}

// keyEqual reports whether two keys are the same key under this table's kind.
//
// ARGUMENT ORDER: every call site passes the STORED key first. The three
// underlying predicates are all symmetric, so it does not matter today; keeping
// one order means a future asymmetric kind cannot silently invert at one site.
func (p *Hashtable) keyEqual(a, b Value) bool {
	switch p.kind {
	case HashtableEq: //nolint:exhaustive // default is HashtableEqual; see HashtableKindCount
		return EqIdentity(a, b)
	case HashtableEqv:
		return Eqv(a, b)
	default:
		// Equal is the authority on equal?; this is Equal's own leaf path with
		// its DeepEqualer assertion skipped, not a second definition of the
		// predicate. A container key still reaches the worklist, because every
		// container's EqualTo is `return Equal(p, o)`.
		//
		// The spelling is the pre-inversion one, and it is worth ~4ns per
		// in-bucket comparison: routing every lookup through Equal cost a
		// further +15% on Get/symbol/n=10, where a 10-entry table has nothing
		// else to amortize two IsVoid calls and a type assertion against.
		//
		// The void guard is NOT redundant with Equal's. A Void key was
		// unreachable before — Void is not Hashable, so admission rejected it —
		// and is reachable now, so EqualTo would be called on it without this.
		if IsVoid(a) || IsVoid(b) {
			return IsVoid(a) == IsVoid(b)
		}
		return a.EqualTo(b)
	}
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
// than pushed because pairing requires LOOKING A KEY UP, which needs an equality
// answer now — the worklist is what is still deciding it.
//
// That eager lookup is bounded for pair and vector keys, whose EqualTo delegates
// to the iterative Equal. The unbounded case is a HASHTABLE reachable as a key:
// Equal -> EqualComponents -> keyEqual -> Equal recurses one Go frame per nesting
// level, and a cycle of tables-as-keys has no bottom. The structural answer will
// therefore be gated on every key being a Hashable leaf.
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
	if !p.equalGate(v, entries) {
		// Identity, decided here. Returning false is correct rather than merely
		// conservative: equalWorklist.step's `a == b` shortcut (equal.go) already
		// answered the reflexive case before any EqualComponents ran, so the only
		// pairs that arrive here are distinct objects.
		return false
	}
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

// equalGate reports whether these two tables may be compared entry-by-entry.
//
// Two conditions, and both are load-bearing:
//
//   - SAME KIND. Two tables whose key-identity rules differ are not comparable
//     entry-by-entry at all: EqualComponents looks each of p's keys up in v, and
//     "is this the same key" has two different answers.
//
//   - EVERY KEY IS A Hashable LEAF. The lookup is eager — it cannot be deferred to
//     Equal's worklist, because pairing entries across two tables requires an
//     equality answer and the worklist is what would be deciding it. Eager
//     recursion is unbounded whenever a hashtable is TRANSITIVELY REACHABLE from
//     a key, which is the requirement — not merely "the key is not itself a
//     hashtable". A pair key (list ht) reaches it too: Equal -> step ->
//     Hashtable.EqualComponents -> keyEqual -> Equal, one Go frame per level,
//     with no bottom on a cycle. So do NOT relax this to a *Hashtable type test;
//     it would be unsound for exactly the case that motivated the gate.
//     Hashable-ness is the cheap conservative stand-in for unreachability: no
//     container implements Hashable — TestNoContainerIsHashable pins that — so
//     one interface assertion per key admits exactly the tables that were
//     constructible before container keys became legal.
//
// The cost is that (equal? ht1 ht2) now answers by key TYPE: two equal-content
// tables keyed on lists are #f where the same tables keyed on symbols are #t. That
// is the price of keeping the deliberate structural-equal? deviation
// (docs/reference/r7rs-differences.md item 9) rather than dropping it as part of an
// unrelated migration. Chez and Racket answer identity for all four cases.
// entries is p's OWN snapshot, passed in rather than re-read. The gate and the
// comparison that follows it must see one view: two independent lock-free reads
// let the gate pass on a leaf-only view while the comparison then runs against
// an entry holding a hashtable key, which is the recursion the gate exists to
// prevent. Pinned by TestHashtableEqualGateUsesOneSnapshot.
func (p *Hashtable) equalGate(other *Hashtable, entries []hashtableEntry) bool {
	if p.kind != other.kind {
		return false
	}
	return keysAreLeaves(entries) && keysAreLeaves(other.snapshot())
}

// keysAreLeaves reports whether every key in the snapshot is Hashable, i.e.
// cannot carry a hashtable and so cannot recurse the eager key lookup.
func keysAreLeaves(entries []hashtableEntry) bool {
	for _, e := range entries {
		_, ok := e.key.(Hashable)
		if !ok {
			return false
		}
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
	// The same order the SchemeWriter uses, for the same reason: rendered key
	// alone leaves ties to sync.Map.Range's per-process walk. The two renderers
	// must not disagree — this one is what the writer's default branch falls
	// through to.
	sortHashtableEntries(entries)
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
func (p *Hashtable) get(key Value) (Value, bool) {
	for _, e := range p.loadBucket(p.hashKey(key)) {
		if p.keyEqual(e.key, key) {
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

// Get retrieves the value associated with key, and whether it was found.
//
// There is no error return. It used to encode "key does not implement Hashable",
// which became unreachable when HashtableKind moved the hash to the table: every
// kind now admits every key.
func (p *Hashtable) Get(key Value) (Value, bool) {
	return p.get(key)
}

// Set associates key with val in the hash table.
//
// Copy-on-write: the target bucket is copied before it is changed, so a
// concurrent reader scanning the old bucket is never disturbed. See the type
// comment for the (non-transactional) concurrency contract.
func (p *Hashtable) Set(key Value, val Value) error {
	err := p.checkMutable("hashtable-set!")
	if err != nil {
		return err
	}
	h := p.hashKey(key)
	old := p.loadBucket(h)
	for i, e := range old {
		if p.keyEqual(e.key, key) {
			nb := make([]hashtableEntry, len(old))
			copy(nb, old)
			// Overwriting a value keeps the key's original ordinal: the key was
			// added when it was added, and re-Setting it must not move it in the
			// rendered order.
			nb[i] = hashtableEntry{key: key, value: val, seq: e.seq}
			p.buckets.Store(h, nb)
			return nil
		}
	}
	nb := make([]hashtableEntry, len(old), len(old)+1)
	copy(nb, old)
	nb = append(nb, hashtableEntry{key: key, value: val, seq: p.nextSeq.Add(1)})
	p.buckets.Store(h, nb)
	p.size.Add(1)
	return nil
}

// HasKey returns whether the key exists in the hash table.
func (p *Hashtable) HasKey(key Value) bool {
	_, found := p.get(key)
	return found
}

// Delete removes the entry for key from the hash table. Absent keys are a no-op.
//
// Copy-on-write: a shrunk bucket is a fresh slice; the last entry's removal
// drops the bucket key entirely.
func (p *Hashtable) Delete(key Value) error {
	err := p.checkMutable("hashtable-delete!")
	if err != nil {
		return err
	}
	h := p.hashKey(key)
	old := p.loadBucket(h)
	for i, e := range old {
		if p.keyEqual(e.key, key) {
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

// KeysVector returns a vector of all keys in the hash table, for R6RS
// hashtable-keys. The order is unspecified.
//
// This replaced a list-returning Keys() and a sibling Values(); the value half
// is served by EntriesVectors, which is the only way to get keys and values
// paired reliably, since two independent snapshots need not agree.
func (p *Hashtable) KeysVector() *Vector {
	entries := p.snapshot()
	ks := make([]Value, len(entries))
	for i, e := range entries {
		ks[i] = e.key
	}
	return NewVector(ks...)
}

// EntriesVectors returns the keys and values as two vectors, index-aligned, for
// R6RS hashtable-entries.
//
// ONE snapshot feeds both. Two calls to snapshot() would be two independent
// lock-free reads of a table another thread may be writing, and the alignment
// R6RS promises ("the ith element of keys is the key of the ith element of vals")
// would be a coincidence rather than a guarantee.
func (p *Hashtable) EntriesVectors() (*Vector, *Vector) {
	entries := p.snapshot()
	ks := make([]Value, len(entries))
	vs := make([]Value, len(entries))
	for i, e := range entries {
		ks[i] = e.key
		vs[i] = e.value
	}
	return NewVector(ks...), NewVector(vs...)
}

// Size returns the number of entries in the hash table. Exact single-threaded;
// best-effort under unsynchronized concurrent mutation, and never negative even
// when the counter has drifted below zero.
func (p *Hashtable) Size() int {
	return p.sizeHint()
}

// Copy returns a shallow copy of the hash table. Buckets are immutable, so each
// stored slice can be shared directly with the copy without re-copying.
func (p *Hashtable) Copy(mutable bool) *Hashtable {
	q := &Hashtable{kind: p.kind, mutable: mutable}
	p.buckets.Range(func(k, v any) bool {
		q.buckets.Store(k, v.([]hashtableEntry))
		return true
	})
	q.size.Store(p.size.Load())
	// The shared buckets carry the source's ordinals, so the copy's counter has
	// to resume above them or a later insert would tie with an existing entry.
	q.nextSeq.Store(p.nextSeq.Load())
	return q
}

// Clear removes all entries from the hash table.
func (p *Hashtable) Clear() error {
	err := p.checkMutable("hashtable-clear!")
	if err != nil {
		return err
	}
	p.buckets.Clear()
	p.size.Store(0)
	return nil
}

// Entries iterates over all entries in the hash table, calling fn for each
// key-value pair. Iteration stops early if fn returns a non-nil error.
// This is more efficient than Keys()+Get() as it avoids intermediate allocations.
//
// fn runs against a snapshot: it may be Scheme code that reads or mutates this
// same table (hashtable-walk). The snapshot is the iteration's view; entries
// added concurrently are not visited.
func (p *Hashtable) Entries(fn func(key, value Value) error) error {
	for _, e := range p.snapshot() {
		err := fn(e.key, e.value)
		if err != nil {
			return err
		}
	}
	return nil
}
