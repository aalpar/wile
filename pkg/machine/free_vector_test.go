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

// Unit-level tests for the flat-closure representation: the free vector, the
// three opcodes that index it, and the packed OpMakeClosure immediate.
//
// These drive hand-built templates rather than compiled Scheme, because the
// codegen that emits these opcodes lands in a later phase. What they pin is the
// half that a wrong emitter would otherwise fail against silently: the codec,
// the bounds and installed-ness checks, and the stack convention MakeClosure
// drains under.

package machine

import (
	"context"
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// runFreeTemplate runs ops in a context whose free vector is free, and returns
// the value register.
func runFreeTemplate(t *testing.T, free []values.Value, ops ...Operation) (values.Value, error) {
	t.Helper()
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false, ops...)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	mc.free = free
	err := mc.Run()
	if err != nil {
		return nil, err
	}
	return mc.GetValue(), nil
}

// TestMakeClosureCodecRoundTrip pins the packed immediate.
//
// The zero row is the compatibility claim: a closure that captures nothing and
// has no self slot encodes to 0, which is the word OpMakeClosure carried when it
// was zero-operand — so the change is invisible to the peephole, to disassembly
// goldens, and to any stored template.
func TestMakeClosureCodecRoundTrip(t *testing.T) {
	tcs := []struct {
		name      string
		freeCount int
		selfSlot  int
		wantArg   int32
	}{
		{name: "captures nothing, no self slot", freeCount: 0, selfSlot: -1, wantArg: 0},
		{name: "captures three, no self slot", freeCount: 3, selfSlot: -1, wantArg: 3},
		{name: "self slot 0", freeCount: 1, selfSlot: 0, wantArg: 1<<16 | 1},
		{name: "self slot 2 of 5", freeCount: 5, selfSlot: 2, wantArg: 3<<16 | 5},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			arg := EncodeMakeClosure(tc.freeCount, tc.selfSlot)
			c.Assert(arg, qt.Equals, tc.wantArg)
			gotFree, gotSelf := DecodeMakeClosure(arg)
			c.Assert(gotFree, qt.Equals, tc.freeCount)
			c.Assert(gotSelf, qt.Equals, tc.selfSlot)
		})
	}
}

// TestMakeClosureCodecRefusesOutOfRange pins the compiler-assertion panics. Both
// operands are bounded by the frame's slot capacity and by the free-variable
// count, so an out-of-range value is a corrupt compiler, not user input.
func TestMakeClosureCodecRefusesOutOfRange(t *testing.T) {
	tcs := []struct {
		name      string
		freeCount int
		selfSlot  int
	}{
		{name: "negative free count", freeCount: -1, selfSlot: -1},
		{name: "free count above int16", freeCount: 1 << 20, selfSlot: -1},
		{name: "self slot below -1", freeCount: 1, selfSlot: -2},
		{name: "self slot above int16", freeCount: 1, selfSlot: 1 << 20},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			defer func() {
				r := recover()
				if r == nil {
					t.Fatalf("EncodeMakeClosure(%d, %d) did not panic", tc.freeCount, tc.selfSlot)
				}
				err, ok := r.(error)
				if !ok || !errors.Is(err, ErrLocalIndexOverflow) {
					t.Fatalf("panic value %v is not a wrapped ErrLocalIndexOverflow", r)
				}
			}()
			EncodeMakeClosure(tc.freeCount, tc.selfSlot)
		})
	}
}

// TestLoadFreeReadsTheVector drives the three free-vector opcodes.
func TestLoadFreeReadsTheVector(t *testing.T) {
	c := qt.New(t)
	free := []values.Value{values.NewInteger(10), values.NewInteger(20)}

	got, err := runFreeTemplate(t, free, NewOperationLoadFree(1))
	c.Assert(err, qt.IsNil)
	c.Assert(got.EqualTo(values.NewInteger(20)), qt.IsTrue)

	// PushFree is the peephole's fusion of LoadFree+Push, so it is asserted
	// through the pair the compiler emits rather than constructed directly.
	got, err = runFreeTemplate(t, free, NewOperationLoadFree(0), NewOperationPush(), NewOperationPull())
	c.Assert(err, qt.IsNil)
	c.Assert(got.EqualTo(values.NewInteger(10)), qt.IsTrue)
}

// TestFreeVectorFaultsAreCompilerDisagreements pins both failure modes. Neither
// is reachable from Scheme: the index is an immediate the emitter chose against
// a layout the same emitter fixed, and the vector is installed by Apply.
func TestFreeVectorFaultsAreCompilerDisagreements(t *testing.T) {
	tcs := []struct {
		name string
		free []values.Value
		idx  int
		want error
	}{
		{
			name: "no vector installed",
			free: nil,
			idx:  0,
			want: ErrNoFreeVector,
		},
		{
			name: "index past the end",
			free: []values.Value{values.NewInteger(1)},
			idx:  1,
			want: ErrFreeIndexOutOfRange,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			_, err := runFreeTemplate(t, tc.free, NewOperationLoadFree(tc.idx))
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.want), qt.IsTrue,
				qt.Commentf("got %v, want a wrapped %v", err, tc.want))
		})
	}
}

// TestPopMakeClosureArgsDrainsInSlotOrder pins MakeClosure's stack convention:
// the emitter pushes free values in slot order and the template LAST, so the
// template pops first and the drain comes back bottom-to-top.
//
// A reversed drain is the defect this catches, and it is invisible to any
// program whose free values happen to be interchangeable.
func TestPopMakeClosureArgsDrainsInSlotOrder(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))

	mc.evals.Push(values.NewInteger(100)) // slot 0
	mc.evals.Push(values.NewInteger(200)) // slot 1
	mc.evals.Push(tpl)                    // pushed last

	gotTpl, free, err := popMakeClosureArgs(mc, 2)
	c.Assert(err, qt.IsNil)
	c.Assert(gotTpl, qt.Equals, tpl)
	c.Assert(len(free), qt.Equals, 2)
	c.Assert(free[0].EqualTo(values.NewInteger(100)), qt.IsTrue,
		qt.Commentf("slot 0 is the value pushed FIRST"))
	c.Assert(free[1].EqualTo(values.NewInteger(200)), qt.IsTrue)
}

// TestPopMakeClosureArgsRefusesANonTemplate pins the type check on the operand
// the emitter is contractually required to have pushed.
func TestPopMakeClosureArgsRefusesANonTemplate(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	mc.evals.Push(values.NewInteger(1))

	_, _, err := popMakeClosureArgs(mc, 0)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotAMachineTemplate), qt.IsTrue)
}

// TestBackPatchSelfSlot pins the letrec T2 carve-out's write, including the two
// no-op cases a closure with no self reference takes.
func TestBackPatchSelfSlot(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.SetShape(env)
	free := []values.Value{values.NewInteger(1), values.NewInteger(2)}
	cls := NewClosureCapturing(tpl, env, free)

	backPatchSelfSlot(cls, free, -1)
	c.Assert(free[0].EqualTo(values.NewInteger(1)), qt.IsTrue,
		qt.Commentf("slot -1 means no self reference and must write nothing"))

	backPatchSelfSlot(cls, free, 7)
	c.Assert(free[1].EqualTo(values.NewInteger(2)), qt.IsTrue,
		qt.Commentf("an out-of-range slot must write nothing rather than panic"))

	backPatchSelfSlot(cls, free, 1)
	c.Assert(free[1], qt.Equals, values.Value(cls))
	c.Assert(cls.Free()[1], qt.Equals, values.Value(cls),
		qt.Commentf("the closure and the vector are the same backing array"))
}

// TestClosureCapturingRefusesANilLink pins that a closure always records the
// environment it resolves globals through. A nil link would leave the apply
// frame with no global, namespace or phases.
func TestClosureCapturingRefusesANilLink(t *testing.T) {
	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("NewClosureCapturing(nil link) did not panic")
		}
		err, ok := r.(error)
		if !ok || !errors.Is(err, werr.ErrNilParentEnvironment) {
			t.Fatalf("panic value %v is not a wrapped ErrNilParentEnvironment", r)
		}
	}()
	NewClosureCapturing(NewNativeTemplate(0, 0, false), nil, nil)
}

// TestFreeVectorOperationIdentity covers the Operation surface the literal pool
// and the disassembler read.
func TestFreeVectorOperationIdentity(t *testing.T) {
	c := qt.New(t)

	c.Assert(NewOperationLoadFree(3).SchemeString(), qt.Equals, "#<machine-operation-load-free 3>")
	c.Assert(NewOperationLoadFree(3).EqualTo(NewOperationLoadFree(3)), qt.IsTrue)
	c.Assert(NewOperationLoadFree(3).EqualTo(NewOperationLoadFree(4)), qt.IsFalse)
	c.Assert(NewOperationLoadFree(3).EqualTo(values.NewInteger(3)), qt.IsFalse)
	c.Assert(NewOperationLoadFree(3).OpKind(), qt.Equals, OpLoadFree)

	// MakeClosure's EqualTo must discriminate BOTH packed fields: the literal
	// pool dedups templates by their code, so two MakeClosures differing only in
	// free count would collapse two templates whose closures capture different
	// things.
	c.Assert(NewOperationMakeClosure(2, -1).EqualTo(NewOperationMakeClosure(2, -1)), qt.IsTrue)
	c.Assert(NewOperationMakeClosure(2, -1).EqualTo(NewOperationMakeClosure(3, -1)), qt.IsFalse)
	c.Assert(NewOperationMakeClosure(2, -1).EqualTo(NewOperationMakeClosure(2, 0)), qt.IsFalse)
}

// TestBoxOperationIdentity covers the boxing operations' Operation surface.
func TestBoxOperationIdentity(t *testing.T) {
	c := qt.New(t)
	li := environment.NewLocalIndex(2, 1)
	other := environment.NewLocalIndex(3, 1)

	c.Assert(NewOperationBoxSlot(li).SchemeString(), qt.Equals, "#<machine-operation-box-slot 2:1>")
	c.Assert(NewOperationBoxSlot(li).EqualTo(NewOperationBoxSlot(li)), qt.IsTrue)
	c.Assert(NewOperationBoxSlot(li).EqualTo(NewOperationBoxSlot(other)), qt.IsFalse)
	c.Assert(NewOperationBoxSlot(li).OpKind(), qt.Equals, OpBoxSlot)

	c.Assert(NewOperationStoreThroughBox(li).SchemeString(), qt.Equals,
		"#<machine-operation-store-through-box 2:1>")
	c.Assert(NewOperationStoreThroughBox(li).EqualTo(NewOperationStoreThroughBox(li)), qt.IsTrue)
	c.Assert(NewOperationStoreThroughBox(li).EqualTo(NewOperationStoreThroughBox(other)), qt.IsFalse)
	c.Assert(NewOperationStoreThroughBox(li).OpKind(), qt.Equals, OpStoreThroughBox)

	c.Assert(NewOperationUnbox().SchemeString(), qt.Equals, "#<machine-operation-unbox>")
	c.Assert(NewOperationUnbox().EqualTo(NewOperationUnbox()), qt.IsTrue)
	c.Assert(NewOperationUnbox().EqualTo(values.NewInteger(1)), qt.IsFalse)
	c.Assert(NewOperationUnbox().OpKind(), qt.Equals, OpUnbox)
}

// TestUnboxRefusesANonBox pins the value-register check. Reaching it means the
// emitter put an OpUnbox after a load of a slot it did not box, which is a
// compiler/VM disagreement rather than user input.
func TestUnboxRefusesANonBox(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false,
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationUnbox(),
	)
	tpl.MaybeAppendLiteral(values.NewInteger(1))
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	err := mc.Run()
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotABox), qt.IsTrue)
}

// TestStoreThroughBoxRefusesANonBox is the write-side twin: a slot the emitter
// did not box cannot be written through.
func TestStoreThroughBoxRefusesANonBox(t *testing.T) {
	c := qt.New(t)
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false,
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationPush(),
		NewOperationStoreThroughBox(environment.NewLocalIndex(0, 0)),
	)
	tpl.MaybeAppendLiteral(values.NewInteger(1))
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	err := mc.Run()
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotABox), qt.IsTrue)
}

// TestBoxSlotInstallsACell drives the binder-side op directly: the slot's value
// is replaced by a box holding it, and a later load of the same slot yields the
// box rather than the value.
func TestBoxSlotInstallsACell(t *testing.T) {
	c := qt.New(t)
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, environment.NewNamespace().Runtime())
	li := environment.NewLocalIndex(0, 0)
	tpl := NewNativeTemplate(0, 0, false,
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationPush(),
		NewOperationStoreLocalByLocalIndexImmediate(li),
		NewOperationBoxSlot(li),
		NewOperationLoadLocalByLocalIndexImmediate(li),
		NewOperationUnbox(),
	)
	tpl.MaybeAppendLiteral(values.NewInteger(7))
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue().EqualTo(values.NewInteger(7)), qt.IsTrue)
}
