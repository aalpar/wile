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
//   - [NumericExtremum]: find min/max with exactness contagion
//   - [IntegerFold]: integer fold for gcd/lcm with big.Int fallback
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
