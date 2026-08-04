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

import "reflect"

// equalHashNodeBudget bounds the unfolding walk. It is the sole termination
// guarantee, and R6RS requires one in writing: "Like equal?, the equal-hash
// procedure must always terminate, even if its arguments contain cycles."
//
// 256 is chosen to cover the structures actually used as keys in this tree
// (e-graph e-node vectors, adjacency-list pairs, tuples of symbols) without a
// per-lookup cost proportional to a large value's whole spine.
const equalHashNodeBudget = 256

// Type seeds for the structural walk. Distinct from the leaf seeds in hash.go
// (0x1 symbol, 0x2 exact numeric, 0x3 string, 0x5 inexact) so a container never
// collides with a leaf by construction.
const (
	seedVoid      byte = 0x10
	seedPair      byte = 0x11
	seedVector    byte = 0x12
	seedBox       byte = 0x13
	seedHashtable byte = 0x14
	seedOpaque    byte = 0x15
	seedRecord    byte = 0x16
)

// EqualHash returns a hash consistent with Equal: Equal(a, b) implies
// EqualHash(a) == EqualHash(b). The converse does NOT hold and no caller may
// assume it — this is the R6RS equal-hash contract, which is one-directional.
//
// The traversal hashes a BOUNDED PREFIX OF THE UNFOLDING: a pre-order walk that
// follows shared and cyclic references as if they were freshly expanded, stopping
// after equalHashNodeBudget nodes. It keeps no visited set, and that is a
// correctness requirement rather than a simplification.
//
// equal? is bisimulation (see equalWorklist.visited), and two bisimilar values can
// have different NUMBERS of distinct nodes: (1 2 . itself) and (1 2 1 2 . itself)
// are equal?, so any fold over distinct nodes — including a visited map keyed on
// first-visit ordinal — gives them different hashes and breaks the contract.
// Their unfoldings are identical, and the budget cuts both at the same point
// because the walk order is fixed by structure, not by allocation.
// Pinned by TestEqualHash_BisimilarCyclesAgree.
func EqualHash(v Value) uint64 {
	h := fnvOffset
	stack := []Value{v}
	budget := equalHashNodeBudget
	for budget > 0 && len(stack) > 0 {
		budget--
		n := len(stack) - 1
		cur := stack[n]
		stack = stack[:n]
		h, stack = equalHashStep(cur, h, stack)
	}
	return h
}

// equalHashStep mixes one node into h and pushes its children so they pop in
// structural order (a stack pops last-in-first, so children go on in reverse).
func equalHashStep(v Value, h uint64, stack []Value) (uint64, []Value) {
	if IsVoid(v) {
		return mixHash(h, hashUint64(seedVoid, 0)), stack
	}
	switch t := v.(type) {
	case *Pair:
		return mixHash(h, hashUint64(seedPair, 0)), append(stack, t.Cdr(), t.Car())
	case *Vector:
		h = mixHash(h, hashUint64(seedVector, uint64(len(*t))))
		for i := len(*t) - 1; i >= 0; i-- {
			stack = append(stack, (*t)[i])
		}
		return h, stack
	case *Box:
		return mixHash(h, hashUint64(seedBox, 0)), append(stack, t.Unbox())
	case *Record:
		// Mirrors Record.EqualComponents, which admits equality only when the
		// RecordType POINTERS match and then compares fields pairwise.
		//
		// The pointer itself must NOT be hashed. equal-hash is Scheme-visible, and an
		// address varies run to run under ASLR and allocation order, so hashing it
		// would make any program that stores a hash irreproducible. The stand-in is
		// the type's NAME plus the field count, both deterministic. Two distinct
		// record types sharing a name and arity collide into one bucket, which the
		// one-directional contract permits.
		//
		// A record field is mutable, so a record can contain itself; the node budget
		// terminates that exactly as it does a cyclic pair.
		h = mixHash(h, hashUint64(seedRecord, uint64(len(t.fields))))
		rt := t.recordType
		if rt != nil && rt.Name() != nil {
			h = mixHash(h, rt.Name().HashCode())
		}
		for i := len(t.fields) - 1; i >= 0; i-- {
			stack = append(stack, t.fields[i])
		}
		return h, stack
	case *Hashtable:
		// Size and kind only, no descent. sync.Map.Range has no defined order, so
		// walking the entries would make the hash depend on iteration order and
		// break determinism outright. Size is order-free and equal tables agree on
		// it. Coarse by construction, which the one-directional contract permits.
		return mixHash(h, hashUint64(seedHashtable, uint64(t.Size()))), stack
	case Hashable:
		// Every leaf whose equal? is content-based already carries a
		// content-canonical HashCode: hashNaN canonicalizes NaN payloads,
		// hashExactNumeric canonicalizes across Integer/BigInteger/Rational. Reusing
		// it is what keeps EqualHash and the existing hash_consistency_test.go
		// contract table in agreement rather than merely concurring.
		return mixHash(h, t.HashCode()), stack
	default:
		// Native errors, ports, procedures, compile-time values: no HashCode and no
		// order-free decomposition worth the walk. Two equal? values share a dynamic
		// type, so the type name is a sound coarse bucket. None of these is a
		// plausible hashtable key, which is why *Record got its own arm above and
		// these did not.
		return mixHash(h, hashString(seedOpaque, reflect.TypeOf(v).String())), stack
	}
}

// mixHash folds an already-hashed component into the FNV-1a accumulator.
func mixHash(h, x uint64) uint64 {
	h ^= x
	h *= fnvPrime
	return h
}

// identityHashFallback is the hash of a Value whose dynamic type is not
// pointer-shaped. Its value does not matter; that it is CONSTANT does.
const identityHashFallback uint64 = 0x9e3779b97f4a7c15

// identityHash returns a stable per-object hash for eq? and eqv? tables.
//
// Value is contractually pointer-shaped — see DeepEqualer's doc in equal.go and
// EqIdentity in utils.go, which compares interfaces with == and would fault on
// anything else — so reflect.Pointer covers every type in this tree. A
// non-pointer dynamic kind, which only an embedder can introduce, falls back to a
// constant. That is a correctness-preserving collision, not a wrong answer: the
// contract is one-directional (eq? a b implies equal hashes), so a shared bucket
// costs a scan. Pinned by TestIdentityHash_NonPointerFallsBackNotFails.
func identityHash(v Value) uint64 {
	rv := reflect.ValueOf(v)
	if rv.Kind() != reflect.Pointer {
		return identityHashFallback
	}
	return uint64(rv.Pointer())
}
