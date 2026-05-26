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

// In-place arithmetic on *BigInteger.
//
// These helpers are package-internal. They mutate the destination's underlying
// *big.Int, reusing its []Word storage when capacity allows. The public methods
// (*BigInteger).Add etc. remain immutable per R7RS Number semantics.
//
// Aliasing: dest may alias p or v (or both). math/big.Int handles receiver
// aliasing in practice; this is pinned by the test suite, not promised by the
// stdlib doc.

// addBigIntInPlace stores p.value + v.value into dest.value, reusing dest's
// existing []Word storage when capacity allows. Returns dest for chaining.
func addBigIntInPlace(dest, p, v *BigInteger) *BigInteger {
	dest.value.Add(p.value, v.value)
	return dest
}
