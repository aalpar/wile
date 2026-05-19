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

// Package helpers provides shared utility functions for primitive implementations.
//
// This package centralizes reusable patterns for implementing Scheme primitives:
//
// # Numeric Operations
//
//   - [NumericFoldVariadic]: fold variadic args with binary operation (+, *)
//   - [NumericFoldWithFirst]: fold with required first arg (-, /)
//   - [NumericChainCompare]: chain comparisons (=, <, >, <=, >=)
//   - [NumericChainCompareReal]: ordering comparisons (<, >, <=, >=) with complex rejection
//   - [NumericExtremum]: find min/max with exactness contagion
//   - [IntegerFold]: integer fold for gcd/lcm with big.Int fallback
//
// # Fold-Shape Family
//
// All six numeric helpers above are catamorphisms over Scheme numbers —
// instances of fold (⊕) ε [a, b, c, ...] differing along three axes:
//
//   - Variadic protocol: rest-at-0 (+, *) vs first+rest-at-1 (-, /, <, min, max)
//   - Accumulator: arithmetic accumulator vs comparison-chain state vs
//     extremum-tracking
//   - Per-element side channel: exactness contagion, NaN propagation,
//     int64 → big.Int promotion
//
// They are intentionally kept as named variants — each documents its
// argument protocol and accumulator role at call sites. A unified
// generic Fold[Acc] would lose that readability for marginal LOC
// savings; consolidation is appropriate only when a 7th variant with
// a fundamentally different protocol (e.g., early-termination with a
// return-value-producing sentinel, or an async fold) is added.
//
// # Comparisons
//
//   - [Eqv]: R7RS eqv? semantics for memv/assv
//   - [CharCompare], [CharCompareVariadic]: character comparisons
//   - [StringCompare], [StringCompareVariadic]: string comparisons
//
// # Type Conversion
//
//   - [ToComplex128], [ToFloat64]: convert Scheme numbers to Go types
//   - [ComplexOrFloat]: return Float if imaginary part is zero
//   - [MakeTypePredicate]: factory for type predicate primitives
//   - [MakeNumericPredicate]: factory for numeric predicates (exact?, zero?, etc.)
//
// # Sequence Accessors
//
//   - [SequenceLength]: generic length for Vector/ByteVector
//   - [SequenceRef]: generic indexed read with element conversion closure
//   - [SequenceSet]: generic indexed mutation with element setter closure
//
// # List Operations
//
//   - [ListToVector]: convert list to vector
//   - [AssocLookup]: generic alist lookup with custom equality
package helpers
