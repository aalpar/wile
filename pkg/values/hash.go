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

import "math/big"

// FNV-1a (Fowler-Noll-Vo): a non-cryptographic hash function chosen for
// simplicity and good distribution. The type seed byte ensures that values
// with identical content but different types (e.g., symbol "x" vs string "x")
// produce different hashes.
// See BIBLIOGRAPHY.md "FNV-1a Hash Function".
const (
	fnvOffset uint64 = 14695981039346656037
	fnvPrime  uint64 = 1099511628211
)

// hashString computes an FNV-1a hash of a string with a type seed.
// The seed differentiates types that share the same underlying data
// (e.g., symbol "foo" vs string "foo").
func hashString(seed byte, s string) uint64 {
	h := fnvOffset
	h ^= uint64(seed)
	h *= fnvPrime
	for i := range len(s) {
		h ^= uint64(s[i])
		h *= fnvPrime
	}
	return h
}

// hashUint64 computes an FNV-1a hash of a uint64 with a type seed.
func hashUint64(seed byte, v uint64) uint64 {
	h := fnvOffset
	h ^= uint64(seed)
	h *= fnvPrime
	for i := range 8 {
		h ^= (v >> (i * 8)) & 0xff
		h *= fnvPrime
	}
	return h
}

// hashExactNumeric computes a canonical hash for any exact numeric value.
// All exact types that compare EqualTo must produce the same hash.
// Canonical form: big.Rat.RatString() (e.g. "5", "3/7").
func hashExactNumeric(r *big.Rat) uint64 {
	return hashString(0x2, r.RatString())
}

// hashInexactNumeric computes a canonical hash for any inexact numeric value.
// All inexact types that compare EqualTo must produce the same hash.
// Canonical form: precision-normalized big.Float.Text('g', -1).
// MinPrec normalization ensures identical output regardless of the
// original precision (53-bit Float vs 256-bit BigFloat).
func hashInexactNumeric(f *big.Float) uint64 {
	prec := f.MinPrec()
	if prec == 0 {
		prec = 1
	}
	normalized := new(big.Float).SetPrec(prec).Set(f)
	return hashString(0x5, normalized.Text('g', -1))
}

// hashNaN is the hash EVERY NaN gets, whatever its payload bits.
//
// The Hashable contract is one-directional but binding: if a.EqualTo(b) then
// a.HashCode() == b.HashCode(). Since eqv? (and so equal?) identifies any two
// NaNs, every NaN must hash alike — and NaN is NOT one bit pattern. Three
// already occur in practice:
//
//	+nan.0 literal   0x7ff8000000000001   (math.NaN())
//	(/ 0.0 0.0)      0x7ff8000000000000
//	negated NaN      0xfff8000000000001   (sign bit flipped)
//
// Hashing the raw bits, as this code used to, gives those three different hashes.
// A hashtable keyed on (/ 0.0 0.0) would then fail to find an entry stored under
// +nan.0 even though the two are equal? — the classic equal-but-different-hash
// corruption. Canonicalizing here is what makes NaN-as-a-key work at all.
func hashNaN() uint64 {
	return hashUint64(0x5, nanCanonicalBits)
}

// nanCanonicalBits is an arbitrary fixed stand-in for "some NaN". Its value does
// not matter; that it is CONSTANT does.
const nanCanonicalBits uint64 = 0x7ff8000000000000
