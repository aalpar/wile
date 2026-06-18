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

package compilation

import (
	"context"
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// The quasiquote/quasisyntax expander recurses structurally with no bound of its
// own — a hole the parser's depth cap cannot cover for programmatically built
// (macro / datum->syntax) forms. Deep nesting must return a catchable
// ErrExpandDepthExceeded, not crash the host with a fatal Go stack overflow.
// buildNestedCall (expander_depth_test.go) yields nested (list (list ... )) — a
// shape expandQuasi recurses into one level per nesting.

func TestQuasi_DepthLimit_ExpansionTrips(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()
	ccnt.quasiMaxDepth = 50

	_, err := ccnt.expandQuasisyntax(context.Background(), buildNestedCall(200), 1)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrExpandDepthExceeded), qt.IsTrue)
}

func TestQuasi_DepthLimit_WithinLimitOK(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()
	ccnt.quasiMaxDepth = 1000

	// The same depth-200 input that trips at max 50 must expand cleanly when the
	// bound is well above it — proving the bound, not the input, is what trips.
	v, err := ccnt.expandQuasisyntax(context.Background(), buildNestedCall(200), 1)
	c.Assert(err, qt.IsNil)
	c.Assert(v, qt.IsNotNil)
}

func TestQuasi_DepthLimit_Cancellation(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	_, err := ccnt.expandQuasisyntax(ctx, buildNestedCall(10), 1)
	c.Assert(errors.Is(err, context.Canceled), qt.IsTrue)
}

// The needs-runtime predicate runs before the expander and walks the same datum.
// When the input is too deep to analyze safely it must report "needs runtime"
// (true) so the depth-guarded expander runs and surfaces the error — rather than
// overflowing the stack here first. A deep no-unsyntax form would otherwise
// return false (emit as literal) after a full-depth walk.
func TestQuasi_DepthLimit_NeedsRuntimeStopsDeep(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()
	ccnt.quasiMaxDepth = 50

	c.Assert(ccnt.quasisyntaxNeedsRuntime(buildNestedCall(200), 1), qt.IsTrue)
}
