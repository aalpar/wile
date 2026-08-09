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

// Package values implements all Scheme runtime value types.
//
// The package provides the complete R7RS value system:
//
// # Numeric Tower (R7RS 6.2.1)
//
//   - [Integer], [BigInteger]: exact integers (int64 and arbitrary precision)
//   - [Rational]: exact rationals via math/big.Rat
//   - [Float], [BigFloat]: inexact reals (float64 and arbitrary precision)
//   - [Complex], [BigComplex]: complex numbers
//
// All numeric types implement the [Number] interface for uniform arithmetic.
//
// # Core Types
//
//   - [Boolean]: #t and #f singletons ([TrueValue], [FalseValue])
//   - [Character]: Unicode rune wrapper
//   - [String]: immutable and mutable string values
//   - [Symbol]: identifiers compared by string key
//   - [Pair]: cons cells as [2]Value arrays
//   - [Vector]: fixed-size mutable value arrays
//   - [ByteVector]: byte arrays for binary data
//   - [Hashtable]: bucket-chaining hash maps with [Hashable] keys
//
// # I/O Ports (R7RS 6.13)
//
// A single concrete type, [PortObject], implements the [Port] marker interface and
// covers every port flavor. Capability-conditional surfaces are reached through its
// As*() (T, bool) accessors; the narrow flavors are named by the [TypeInputPort] …
// [TypeBinaryOutputPort] [ValueType] constants.
//
// # Concurrency (SRFI-18+)
//
//   - [Thread], [Mutex], [ConditionVariable]: SRFI-18 threading
//   - [AtomicBox], [AtomicInt64]: atomic operations
//
// # Singletons
//
// Use [Void] for the absence of a value and [EOFObject] for end-of-file.
// [EmptyList] is the empty list sentinel.
package values
