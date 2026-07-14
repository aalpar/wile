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

// This file exposes internals to the EXTERNAL test package (values_test) without
// putting them on the public API. It is compiled only under `go test`.

// PromotionResultKind returns the result kind when operands of kindA and kindB are
// combined in an ARITHMETIC operation: the least upper bound in the promotion lattice.
//
// PromotionResultKind and ComparisonResultKind used to be exported from promotion.go.
// They had zero production callers — every internal site indexes the tables directly —
// and existed solely so the external values_test package could read them. That made them
// public API with two problems and no benefit:
//
//   - A raw index panic. NumericKind is an exported uint8 and numKinds is not, so a
//     caller could not even bounds-check its argument; PromotionResultKind(200, ...) died
//     with "index out of range [200] with length 7", an unwrapped runtime panic in an
//     embedder's process. That violates CLAUDE.md's imperative against raw panics.
//   - A footgun with no type-level guard. Both return NumericKind, nothing distinguishes
//     "kind for arithmetic" from "kind for comparison", and picking the wrong one
//     silently ROUNDS an operand. The answer is merely wrong, not loud.
//
// An export_test.go is where a test-only accessor belongs. The bounds concern evaporates
// with it: the only callers are in-tree tests passing real kinds.
func PromotionResultKind(kindA, kindB NumericKind) NumericKind {
	return promotionTable[kindA][kindB]
}

// ComparisonResultKind returns the kind two operands are promoted to in order to be
// COMPARED, which is not in general the kind their arithmetic would produce. (+ 2 1.5) is
// a Float, but comparing 2 against 1.5 goes through BigFloat so that neither operand is
// rounded. See comparisonTable.
func ComparisonResultKind(kindA, kindB NumericKind) NumericKind {
	return comparisonTable[kindA][kindB]
}
