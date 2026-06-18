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

package environment

import (
	"testing"

	"github.com/aalpar/wile/pkg/values"
)

// sinkSym prevents dead-code elimination of symbol allocations.
var sinkSym *values.Symbol

// sinkBool prevents dead-code elimination of comparison results.
var sinkBool bool

// ---------- Symbol allocation ----------

// BenchmarkNewSymbolOnly measures bare symbol allocation.
func BenchmarkNewSymbolOnly(b *testing.B) {
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sinkSym = values.NewSymbol("lambda")
	}
}

// ---------- eq? comparison cost ----------

// BenchmarkEqPointerSame measures Go interface == on two identical pointers.
func BenchmarkEqPointerSame(b *testing.B) {
	var a values.Value = values.NewSymbol("foo")
	c := a // same pointer (both are values.Value)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sinkBool = a == c
	}
}

// BenchmarkEqPointerDifferent measures Go interface == on two different pointers
// with the same key. Without interning, this is what eq? would see (returns false).
func BenchmarkEqPointerDifferent(b *testing.B) {
	var a values.Value = values.NewSymbol("foo")
	var c values.Value = values.NewSymbol("foo")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sinkBool = a == c
	}
}

// BenchmarkEqStringCompare measures a type-switch + string compare on symbols.
// This is what eq? costs for symbols (string-keyed comparison).
func BenchmarkEqStringCompare(b *testing.B) {
	var a values.Value = values.NewSymbol("foo")
	var c values.Value = values.NewSymbol("foo")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sinkBool = eqByString(a, c)
	}
}

// BenchmarkEqStringCompareLong measures string compare for a longer symbol name.
func BenchmarkEqStringCompareLong(b *testing.B) {
	var a values.Value = values.NewSymbol("call-with-current-continuation")
	var c values.Value = values.NewSymbol("call-with-current-continuation")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sinkBool = eqByString(a, c)
	}
}

// BenchmarkEqStringCompareMiss measures string compare when symbols differ.
func BenchmarkEqStringCompareMiss(b *testing.B) {
	var a values.Value = values.NewSymbol("foo")
	var c values.Value = values.NewSymbol("bar")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sinkBool = eqByString(a, c)
	}
}

// BenchmarkEqNonSymbol measures eq? cost for non-symbol types (Integer).
// Both paths pay this cost identically — it's the baseline.
func BenchmarkEqNonSymbol(b *testing.B) {
	var a values.Value = values.NewInteger(42)
	var c values.Value = values.NewInteger(42)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sinkBool = a == c
	}
}

// ---------- memq-style scan ----------

// BenchmarkMemqStringCompare simulates memq on a 10-element symbol list
// using string comparison.
func BenchmarkMemqStringCompare(b *testing.B) {
	names := []string{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}
	list := make([]values.Value, len(names))
	for i, n := range names {
		list[i] = values.NewSymbol(n)
	}
	var target values.Value = values.NewSymbol("j")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		found := false
		for _, v := range list {
			if eqByString(v, target) {
				found = true
				break
			}
		}
		sinkBool = found
	}
}

// ---------- amortized: creation + comparison ----------

// BenchmarkAllocThenStringEq measures the alternative: create, then string compare.
// Note: escape analysis may stack-allocate the symbol (0 allocs). See
// BenchmarkAllocThenStringEqEscaped for the version that forces heap allocation.
func BenchmarkAllocThenStringEq(b *testing.B) {
	var canonical values.Value = values.NewSymbol("test")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s := values.NewSymbol("test")
		sinkBool = eqByString(s, canonical)
	}
}

// BenchmarkAllocThenStringEqEscaped forces heap allocation by storing the
// symbol into a sink, simulating the real case where symbols escape into
// environments, ASTs, or data structures.
func BenchmarkAllocThenStringEqEscaped(b *testing.B) {
	var canonical values.Value = values.NewSymbol("test")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s := values.NewSymbol("test")
		sinkSym = s // force escape
		sinkBool = eqByString(s, canonical)
	}
}

// eqByString duplicates helpers.EqIdentity for benchmarking purposes.
// Cannot import registry/helpers here due to package layering.
func eqByString(a, b values.Value) bool {
	sa, ok := a.(*values.Symbol)
	if ok {
		sb, ok2 := b.(*values.Symbol)
		if ok2 {
			return sa.Key == sb.Key
		}
		return false
	}
	return a == b
}
