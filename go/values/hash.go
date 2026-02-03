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

// FNV-1a hash constants.
// See Fowler-Noll-Vo hash function.
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
