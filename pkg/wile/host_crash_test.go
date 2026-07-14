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

package wile_test

import (
	"context"
	"fmt"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// Regression suite for reviews/2026-07-13 finding #1 (host-process crashes,
// uncatchable by RunResumable's recover). One test per citation, driven through
// the public Engine API because that is the contract an embedder depends on.
//
// Class C citations (recoverable panics) assert in-process, as here. Class A and
// B citations are Go `fatal error`s, which no recover catches; those live in the
// subprocess harness (impl plan Phase 5).

func newTestEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background())
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// manyBindings renders "(x0 0) (x1 0) ... (xN-1 0)".
func manyBindings(n int) string {
	var b strings.Builder
	for i := range n {
		fmt.Fprintf(&b, "(x%d 0)", i)
	}
	return b.String()
}

// manyParams renders "x0 x1 ... xN-1".
func manyParams(n int) string {
	var b strings.Builder
	for i := range n {
		fmt.Fprintf(&b, "x%d ", i)
	}
	return b.String()
}

// TestHostCrash_LetExceedingLocalSlots asserts that a `let` frame needing more
// than 32767 local slots is a compile error, not a process-killing panic.
//
// Citation: native_template.go:305 -> EncodeLocalIndex (instruction.go:83) packs
// the slot into an int16 and panics past the range. There is no recover on the
// compile path, so a *syntactically valid* program crashed the host during
// compile, before Run's boundary existed. Machine-generated Scheme reaches this.
func TestHostCrash_LetExceedingLocalSlots(t *testing.T) {
	eng := newTestEngine(t)

	src := "(let (" + manyBindings(40000) + ") 1)"
	_, err := eng.EvalMultiple(context.Background(), src)

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "32767",
		qt.Commentf("the error must name the limit the program exceeded"))
}

// TestHostCrash_LambdaExceedingLocalSlots is the same defect one form over: the
// parameter list, not the let bindings, is what overflows the frame.
func TestHostCrash_LambdaExceedingLocalSlots(t *testing.T) {
	eng := newTestEngine(t)

	src := "(lambda (" + manyParams(40000) + ") 1)"
	_, err := eng.EvalMultiple(context.Background(), src)

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "32767")
}

// TestHostCrash_InternalDefinesExceedingLocalSlots covers the case the design
// missed. compileClosureBody checks the parameter count, but a body's internal
// defines allocate their slots later, during compileBody, so the parameter check
// cannot see them. Verified to still panic at slot 32768 with only the let and
// parameter guards in place.
func TestHostCrash_InternalDefinesExceedingLocalSlots(t *testing.T) {
	eng := newTestEngine(t)

	var b strings.Builder
	b.WriteString("((lambda () ")
	for i := range 40000 {
		fmt.Fprintf(&b, "(define y%d 0) ", i)
	}
	b.WriteString("1))")

	_, err := eng.EvalMultiple(context.Background(), b.String())

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "32767")
}

// TestHostCrash_LetAtSlotLimitStillCompiles pins the boundary from below, so the
// guard cannot pass by rejecting everything. 32767 slots is the largest legal
// frame; the guard must admit it.
func TestHostCrash_LetAtSlotLimitStillCompiles(t *testing.T) {
	eng := newTestEngine(t)

	src := "(let (" + manyBindings(32767) + ") 1)"
	_, err := eng.EvalMultiple(context.Background(), src)

	qt.Assert(t, err, qt.IsNil,
		qt.Commentf("32767 slots is within the int16 encoding range and must compile"))
}
