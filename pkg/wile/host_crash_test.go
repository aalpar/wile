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
	"errors"
	"fmt"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/werr"
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

// TestHostCrash_AtomicBoxTypeChange asserts that storing a differently-typed value
// into an atomic box is an ordinary operation, not an uncatchable panic.
//
// Citation: atomic.go:61. sync/atomic.Value panics on a concrete-type change, which
// is normal under dynamic typing. The panic escaped RunResumable as a runtime error.
// The atomic primitives live in extensions/gointerop, so this needs a profile that
// loads it (the review said registry/gointerop; the design said pkg/extensions/
// gointerop; both are wrong).
func TestHostCrash_AtomicBoxTypeChange(t *testing.T) {
	eng, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)

	v, err := eng.EvalMultiple(context.Background(),
		`(define a (make-atomic 1)) (atomic-store! a "now a string") (atomic-load a)`)

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, `"now a string"`)
}

// TestHostCrash_VectorCopyDestinationOverflow asserts that a near-MaxInt64
// destination index is rejected rather than slice-panicking.
//
// Citation: PrimVectorCopyTo. The guard `atIdx+(end-start) > to.Length()` overflows
// int64 for a huge atIdx: the sum wraps negative, the guard passes, and the slice
// expression panics uncatchably. Values above MaxInt64 parse as BigInteger and are
// rejected upstream, so MaxInt64-1 is the boundary that matters.
func TestHostCrash_VectorCopyDestinationOverflow(t *testing.T) {
	eng := newTestEngine(t)

	_, err := eng.EvalMultiple(context.Background(),
		`(vector-copy! (vector 1 2 3) 9223372036854775806 (vector 4 5))`)

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "destination index")
}

// TestHostCrash_BytevectorCopyDestinationOverflow is the same guard, same overflow,
// in bytevector-copy! (prim_byte_vectors.go:168).
func TestHostCrash_BytevectorCopyDestinationOverflow(t *testing.T) {
	eng := newTestEngine(t)

	_, err := eng.EvalMultiple(context.Background(),
		`(bytevector-copy! (bytevector 1 2 3) 9223372036854775806 (bytevector 4 5))`)

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "destination index")
}

// TestHostCrash_StringCopyDestinationOverflow is the same guard in string-copy!
// (internal/extensions/all/prim_strings.go:73), which is not in the default profile.
//
// The trigger is at+copyLen exceeding MaxInt64, not "at near MaxInt64" as the design
// puts it. With a one-element source, at must be MaxInt64 exactly: at MaxInt64-1 the
// sum lands on MaxInt64, does not wrap, and the guard correctly rejects. The vector
// cases above copy two elements, so MaxInt64-1 suffices there.
func TestHostCrash_StringCopyDestinationOverflow(t *testing.T) {
	eng, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(context.Background(),
		`(string-copy! (string #\a #\b) 9223372036854775807 (string #\c))`)

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "out of bounds")
}

// TestHostCrash_VectorCopyValidDestinationStillWorks pins the guard from below: a
// legal in-range copy must still be admitted after the rewrite.
func TestHostCrash_VectorCopyValidDestinationStillWorks(t *testing.T) {
	eng := newTestEngine(t)

	v, err := eng.EvalMultiple(context.Background(),
		`(define v (vector 1 2 3)) (vector-copy! v 1 (vector 8 9)) v`)

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "#(1 8 9)")
}

// TestHostCrash_FFINonForeignErrorPanicKeepsSentinel is the discriminating case for
// ffi_wrapper.go:44.
//
// The wrapper returned only *werr.ForeignError and re-panicked everything else as
// panic(fmt.Sprintf("FFI %q: %v", ...)). The stringified re-panic is caught downstream
// by operations_call.go:126, so the panic's *text* survives and a message assertion
// cannot see the bug. What does not survive is the error's identity: any error that is
// not a *ForeignError, such as a bare sentinel, is flattened to a string and
// errors.Is stops matching. That is the lossless-chain rule broken at a boundary
// whose whole job is to carry Go errors into the VM.
//
// The re-raise also implements a policy reversed on 2026-06-23: the VM boundary
// contains every panic, so the wrapper has nothing to re-raise for.
func TestHostCrash_FFINonForeignErrorPanicKeepsSentinel(t *testing.T) {
	eng := newTestEngine(t)

	sentinel := werr.NewStaticError("ffi bare sentinel")
	err := eng.RegisterFunc("boom", func() int {
		panic(sentinel)
	})
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(context.Background(), `(boom)`)

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, sentinel), qt.IsTrue,
		qt.Commentf("a non-ForeignError sentinel must survive the FFI boundary, not be stringified"))
}

// TestHostCrash_FFIForeignErrorPanicKeepsSentinel pins the path that already worked,
// so the fix cannot regress it.
func TestHostCrash_FFIForeignErrorPanicKeepsSentinel(t *testing.T) {
	eng := newTestEngine(t)

	sentinel := werr.NewStaticError("ffi wrapped sentinel")
	err := eng.RegisterFunc("boom2", func() int {
		panic(werr.WrapForeignErrorf(sentinel, "deliberate"))
	})
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(context.Background(), `(boom2)`)

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, sentinel), qt.IsTrue)
}

// TestHostCrash_FFINonErrorPanicNamesTheFunction pins that a panic("...") still
// reports which FFI function blew up. The name is the only locator a Go host gets.
func TestHostCrash_FFINonErrorPanicNamesTheFunction(t *testing.T) {
	eng := newTestEngine(t)

	err := eng.RegisterFunc("boom3", func() int {
		panic("go side exploded")
	})
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(context.Background(), `(boom3)`)

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "go side exploded")
	qt.Assert(t, err.Error(), qt.Contains, "boom3")
}

// TestHostCrash_ApplyCircularList asserts that apply over a circular list returns an
// error instead of consuming memory until the host dies.
//
// Citation: machine_context.go:459. Pair.ForEach accepted a ctx and never read it and
// chased cdr pointers unconditionally, so the argument spread ignored the deadline,
// maxStackSize, and maxCallDepth all at once, growing the eval stack to ~10GB in
// seconds and defeating every sandbox resource limit. R7RS 6.10 requires apply's last
// argument to be a proper list; a circular list is improper, so the correct answer was
// always ErrNotAList, and nothing detected the condition that should raise it.
func TestHostCrash_ApplyCircularList(t *testing.T) {
	eng := newTestEngine(t)

	_, err := eng.EvalMultiple(context.Background(), `
		(define lst (list 1 2 3))
		(set-cdr! (cddr lst) lst)
		(apply + lst)`)

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue,
		qt.Commentf("a circular list is an improper list"))
	qt.Assert(t, errors.Is(err, werr.ErrCircularList), qt.IsTrue,
		qt.Commentf("the cycle sentinel must remain reachable through the chain"))
}

// TestHostCrash_ApplyLongListRespectsMaxStackSize asserts that the eval stack limit
// bounds the argument spread as it happens, not after it completes.
//
// Before the fix, checkStackSize ran after the whole list had been pushed, so the
// stack's peak was the argument list's length no matter what maxStackSize said. The
// error surfaced, but only once the memory had already been committed.
func TestHostCrash_ApplyLongListRespectsMaxStackSize(t *testing.T) {
	eng, err := wile.NewEngine(context.Background(), wile.WithMaxStackSize(1000))
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(context.Background(),
		`(apply + (make-list 500000 1))`)

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrStackOverflow), qt.IsTrue)
}

// TestHostCrash_ApplyProperListStillWorks pins the guard from below: ordinary apply
// over a proper list must be unaffected by the cycle check.
func TestHostCrash_ApplyProperListStillWorks(t *testing.T) {
	eng := newTestEngine(t)

	v, err := eng.EvalMultiple(context.Background(), `(apply + (list 1 2 3 4 5))`)

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "15")
}
