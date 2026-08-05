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

package wile

import (
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestStrictLevelOptSameShape asserts DIRECTLY what the benchmarks in
// strict_level_optimizer_bench_test.go can only assert statistically: that level
// 2 emits the same bytecode as level 0 for the same source.
//
// The benchmarks read the same property off allocs/op, because a lost Stable
// stamp changes the emitted shape (PullApply in place of a promoted op, a
// non-inlined HOF) and that shows up in allocation counts. But allocs/op is an
// INDIRECT proxy: it is a scalar summary that a shape change is merely likely to
// perturb, it takes ~46 s to collect, it needs a human to read benchstat, and CI
// does not run benchmarks at all. This test reads the shape itself, so a lost
// stamp fails an ordinary `go test` in well under a second.
//
// Byte-identity is the right assertion rather than a substring probe for a
// specific opcode: the stamp feeds several optimizer decisions (promotion,
// self-tail-call, inline-HOF), and naming one opcode would pin only the decision
// that happened to be named. Both arms compile the same text through the same
// compiler; the only variable is where the primitives came from, so anything but
// equality is the regression.
func TestStrictLevelOptSameShape(t *testing.T) {
	tests := []struct {
		name  string
		setup string
		proc  string
		// mustContain guards against the equality passing VACUOUSLY. Two equally
		// DEOPTIMIZED arms compare equal just as happily as two optimized ones, so
		// equality alone would survive a change that switched the optimizer off
		// wholesale. These opcodes are the stamp-dependent shape itself: they are
		// emitted only while the operator's binding is stamped Stable, so requiring
		// them in BOTH arms turns the equality into "equal AND optimized".
		mustContain []string
	}{
		{
			// Arithmetic / self-call arm. This case carries the test's teeth: the
			// promoted ops are exactly what the Stable stamp buys.
			name:        "fib",
			setup:       benchOptFib,
			proc:        "fib",
			mustContain: []string{"NumLe", "Sub", "AddTail"},
		},
		{
			// Second shape, deliberately with no mustContain. This arm does NOT
			// reach the inline-HOF stamp: map arrives by import, and the import
			// seam stamps only the import-gated HOFs (fold, fold-right), so map's
			// loop is un-inlined in both arms and there is no optimized opcode to
			// require. It pins that the generic apply path and the resolution of
			// + and map do not drift between levels, and nothing more. See the
			// header comment in strict_level_optimizer_bench_test.go.
			name:  "map-sum",
			setup: benchOptMapSum,
			proc:  "map-sum",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)

			level0, ctx := newStrictLevelEngine(t, false, tt.setup)
			defer level0.Close()
			level2, _ := newStrictLevelEngine(t, true, tt.setup)
			defer level2.Close()

			val0, err := level0.EvalMultiple(ctx, tt.proc)
			c.Assert(err, qt.IsNil)
			dis0, err := level0.DisassembleValue(val0)
			c.Assert(err, qt.IsNil)

			val2, err := level2.EvalMultiple(ctx, tt.proc)
			c.Assert(err, qt.IsNil)
			dis2, err := level2.DisassembleValue(val2)
			c.Assert(err, qt.IsNil)

			// Guard against the assertion passing on two empty strings, which
			// would make the whole test vacuous if DisassembleValue ever grew a
			// quiet path for a shape it does not recognize.
			c.Assert(dis0, qt.Not(qt.Equals), "")

			for _, op := range tt.mustContain {
				c.Assert(strings.Contains(dis0, op), qt.IsTrue,
					qt.Commentf("level 0 lost %s; the equality below would still"+
						" pass, so it must not be read as evidence:\n%s", op, dis0))
				c.Assert(strings.Contains(dis2, op), qt.IsTrue,
					qt.Commentf("level 2 lost %s; disassembly:\n%s", op, dis2))
			}

			c.Assert(dis2, qt.Equals, dis0,
				qt.Commentf("level 2 must emit the same bytecode as level 0 for %s;"+
					" a difference means a Stable stamp was lost when the primitive's"+
					" evidence moved from ambient registration to import", tt.proc))
		})
	}
}
