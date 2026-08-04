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

package values_test

import (
	"fmt"
	"reflect"
	"strconv"
	"testing"

	"github.com/aalpar/wile/pkg/values"
)

// BenchmarkHashtableSet and BenchmarkHashtableGet cover the shapes the stdlib's hot
// consumers actually use: cfl.scm's CYK inner loop (33 hashtable ops, symbol and
// pair keys), fca.scm (17), graph.scm (19). Sizes span the bucket-collision regime.
func BenchmarkHashtableSet(b *testing.B) {
	for _, n := range []int{10, 1000, 100000} {
		for _, kind := range benchKeyKinds {
			b.Run(fmt.Sprintf("%s/n=%d", kind.name, n), func(b *testing.B) {
				keys := kind.make(n)
				b.ResetTimer()
				b.ReportAllocs()
				for i := 0; i < b.N; i++ {
					ht := values.NewEmptyHashtable()
					for _, k := range keys {
						ht.Set(k, values.NewInteger(1))
					}
				}
			})
		}
	}
}

func BenchmarkHashtableGet(b *testing.B) {
	for _, n := range []int{10, 1000, 100000} {
		for _, kind := range benchKeyKinds {
			b.Run(fmt.Sprintf("%s/n=%d", kind.name, n), func(b *testing.B) {
				keys := kind.make(n)
				ht := values.NewEmptyHashtable()
				for _, k := range keys {
					ht.Set(k, values.NewInteger(1))
				}
				b.ResetTimer()
				b.ReportAllocs()
				for i := 0; i < b.N; i++ {
					for _, k := range keys {
						_, _ = ht.Get(k)
					}
				}
			})
		}
	}
}

var benchKeyKinds = []struct {
	name string
	make func(n int) []values.Value
}{
	{"symbol", func(n int) []values.Value {
		q := make([]values.Value, n)
		for i := range q {
			q[i] = values.NewSymbol("k" + strconv.Itoa(i))
		}
		return q
	}},
	{"string", func(n int) []values.Value {
		q := make([]values.Value, n)
		for i := range q {
			q[i] = values.NewString("k" + strconv.Itoa(i))
		}
		return q
	}},
	{"fixnum", func(n int) []values.Value {
		q := make([]values.Value, n)
		for i := range q {
			q[i] = values.NewInteger(int64(i))
		}
		return q
	}},
}

// BenchmarkIdentityHashCandidates prices the reflect-based identity hash against a
// leaf HashCode BEFORE it lands on the eq/eqv lookup path. reflect.ValueOf can
// force its argument to escape; if this shows an allocation, the eq/eqv kinds need
// a type-switch fast path over the pointer-shaped types instead. Q5 in the design.
func BenchmarkIdentityHashCandidates(b *testing.B) {
	sym := values.NewSymbol("k")
	b.Run("reflect-pointer", func(b *testing.B) {
		b.ReportAllocs()
		for i := 0; i < b.N; i++ {
			_ = uint64(reflect.ValueOf(values.Value(sym)).Pointer())
		}
	})
	b.Run("hashcode", func(b *testing.B) {
		b.ReportAllocs()
		for i := 0; i < b.N; i++ {
			_ = sym.HashCode()
		}
	})
}
