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

package machine

import (
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// popNOld is HEAD's PopN implementation (general path only), kept here so the
// benchmark can compare it against the current switch-based PopN in one run.
func popNOld(p *Stack, n int) []values.Value {
	l := len(*p)
	if n < 0 {
		panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow, "PopN: negative count %d", n))
	}
	if n > l {
		panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow, "PopN: requested %d elements from stack of length %d", n, l))
	}
	if n == 0 {
		return nil
	}
	result := make([]values.Value, n)
	copy(result, (*p)[l-n:])
	*p = (*p)[:l-n]
	return result
}

// benchPopN drives fn b.N times, resetting the stack to length n before each
// call so only the PopN cost is measured (the backing array is reused).
func benchPopN(b *testing.B, n int, fn func(*Stack, int) []values.Value) {
	backing := make([]values.Value, n)
	for j := range backing {
		backing[j] = values.TrueValue
	}
	s := Stack(backing)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s = s[:n]
		_ = fn(&s, n)
	}
}

func BenchmarkPopN_New_1(b *testing.B) { benchPopN(b, 1, (*Stack).PopN) }
func BenchmarkPopN_Old_1(b *testing.B) { benchPopN(b, 1, popNOld) }

func BenchmarkPopN_New_2(b *testing.B) { benchPopN(b, 2, (*Stack).PopN) }
func BenchmarkPopN_Old_2(b *testing.B) { benchPopN(b, 2, popNOld) }

func BenchmarkPopN_New_3(b *testing.B) { benchPopN(b, 3, (*Stack).PopN) }
func BenchmarkPopN_Old_3(b *testing.B) { benchPopN(b, 3, popNOld) }

func BenchmarkPopN_New_8(b *testing.B) { benchPopN(b, 8, (*Stack).PopN) }
func BenchmarkPopN_Old_8(b *testing.B) { benchPopN(b, 8, popNOld) }
