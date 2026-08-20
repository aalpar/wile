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
	"reflect"
	"sync"
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestLocalEnvironment(t *testing.T) {
	// Create a new Local environment
	env := NewLocalEnvironment(0)

	// Check if the environment is initialized correctly
	if env == nil {
		t.Fatal("Expected a non-nil Local environment")
	}

	value0 := values.NewInteger(42)
	value1 := values.NewInteger(43)

	// variable has not been added yet, so GetLocalIndex should return nil
	tv0 := values.NewSymbol("testVar0")
	li0 := env.GetLocalIndex(tv0)
	qt.Assert(t, li0, qt.IsNil)

	// Test adding a binding
	li0, ok := env.EnsureLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li0[0], qt.Equals, 0)
	qt.Assert(t, li0[1], qt.Equals, 0)

	// Set the initial value of the new binding
	err := env.SetLocalValue(li0, value0)
	qt.Assert(t, err, qt.IsNil)

	// Re-adding the same binding should not change the index
	li0, ok = env.EnsureLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsFalse)
	qt.Assert(t, li0[0], qt.Equals, 0)
	qt.Assert(t, li0[1], qt.Equals, 0)

	// Adding a new binding should create a new index
	tv1 := values.NewSymbol("testVar1")
	li1, ok := env.EnsureLocalBinding(tv1, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li1[0], qt.Equals, 1)
	qt.Assert(t, li1[1], qt.Equals, 0)

	// Set the initial value of the new binding
	err = env.SetLocalValue(li1, value1)
	qt.Assert(t, err, qt.IsNil)

	v := env.GetLocalBinding(li0)
	qt.Assert(t, v.value, valuestest.SchemeEquals, value0)
	v = env.GetLocalBinding(li1)
	qt.Assert(t, v.value, valuestest.SchemeEquals, value1)
}

func TestLocalEnvironmentFrame_Bindings(t *testing.T) {
	le := NewLocalEnvironment(0)
	qt.Assert(t, le, qt.Not(qt.IsNil))
	le.EnsureLocalBinding(values.NewSymbol("testVar0"), BindingTypeVariable)

	bindings := le.Bindings()
	qt.Assert(t, bindings, qt.HasLen, 1)
}

func TestLocalEnvironmentFrame_Keys(t *testing.T) {
	le := NewLocalEnvironment(0)

	sym1 := values.NewSymbol("var1")
	sym2 := values.NewSymbol("var2")

	le.EnsureLocalBinding(sym1, BindingTypeVariable)
	le.EnsureLocalBinding(sym2, BindingTypeVariable)

	keys := le.Keys()
	qt.Assert(t, keys, qt.HasLen, 2)
}

func TestLocalEnvironmentFrame_Keys_DefensiveCopy(t *testing.T) {
	le := NewLocalEnvironment(0)
	sym1 := values.NewSymbol("var1")
	le.EnsureLocalBinding(sym1, BindingTypeVariable)

	keys := le.Keys()
	// Mutating the returned map must not affect internal state.
	bogus := values.NewSymbol("bogus")
	keys[*bogus] = []int{99}

	qt.Assert(t, le.Keys(), qt.HasLen, 1)
	qt.Assert(t, le.GetLocalIndex(bogus), qt.IsNil)
}

func TestCopyForApplyInto_MarksDestinationShared(t *testing.T) {
	le := NewLocalEnvironment(1)
	li0 := &LocalIndex{0, 0}
	le.SetLocalValue(li0, values.NewInteger(42))

	var dst LocalEnvironmentFrame
	le.copyForApplyInto(&dst)

	// Only the destination is marked keysShared. The source (an apply source =
	// fully-compiled closure env) is intentionally NOT marked: its keys are
	// never mutated at runtime, and marking it raced when the same closure was
	// applied from multiple threads. See copyForApplyInto's comment.
	qt.Assert(t, dst.keysShared, qt.IsTrue)
	qt.Assert(t, le.keysShared, qt.IsFalse)

	// The keys map is aliased (CoW) between source and destination.
	qt.Assert(t, reflect.ValueOf(dst.keys).Pointer(), qt.Equals, reflect.ValueOf(le.keys).Pointer())

	// Bindings are independent: mutating a destination value leaves the source.
	qt.Assert(t, dst.bindings[0].Value(), valuestest.SchemeEquals, values.NewInteger(42))
	dst.bindings[0].SetValue(values.NewInteger(99))
	qt.Assert(t, le.GetLocalBinding(li0).Value(), valuestest.SchemeEquals, values.NewInteger(42))

	// Destination-side CoW still protects the shared keys map: adding a key to
	// the destination clones its keys, leaving the source's keys untouched.
	dst.EnsureLocalBinding(values.NewSymbol("added-to-dst"), BindingTypeVariable)
	qt.Assert(t, dst.GetLocalIndex(values.NewSymbol("added-to-dst")), qt.Not(qt.IsNil))
	qt.Assert(t, le.GetLocalIndex(values.NewSymbol("added-to-dst")), qt.IsNil)
}

// TestCopyForApplyInto_ConcurrentSourceRaceFree exercises the exact data race
// that motivated dropping the source-side keysShared write (B2): applying one
// closure from multiple threads shares a single source frame, and the former
// `p.keysShared = true` was a concurrent write to it. Run under -race; before
// the fix this failed, now it is clean. Each goroutine owns its own dst, so the
// only shared state is the read-only source `le`.
func TestCopyForApplyInto_ConcurrentSourceRaceFree(t *testing.T) {
	le := NewLocalEnvironment(2)
	le.EnsureLocalBinding(values.NewSymbol("x"), BindingTypeVariable)
	le.EnsureLocalBinding(values.NewSymbol("y"), BindingTypeVariable)

	var wg sync.WaitGroup
	for range 32 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for range 2000 {
				var dst LocalEnvironmentFrame
				le.copyForApplyInto(&dst)
			}
		}()
	}
	wg.Wait()

	// The shared source was never mutated: still exactly its two keys, unshared.
	qt.Assert(t, len(le.Keys()), qt.Equals, 2)
	qt.Assert(t, le.keysShared, qt.IsFalse)
}

// TestCopyForApplyInto_SkipsAnonymousSlots pins the merged-`let` exemption: a
// slot minted by AppendAnonymousSlot is not transferred, and the destination
// reads it as #!void anyway because its backing array was void-filled.
//
// Both halves matter. Dropping the exemption re-introduces the per-apply copy
// the merge exists to avoid; keeping it over a merely ZEROED array hands a
// letrec forward reference a nil values.Value, which surfaces as
// "expected 1 value, got 0" rather than #!void.
func TestCopyForApplyInto_SkipsAnonymousSlots(t *testing.T) {
	le := NewLocalEnvironment(1)
	le.MaybeCreateLocalBinding(values.NewSymbol("d"), BindingTypeVariable, nil, nil)
	anon := le.AppendAnonymousSlot()

	qt.Assert(t, len(le.Bindings()), qt.Equals, 3)
	qt.Assert(t, le.CopyCount(), qt.Equals, 2)
	qt.Assert(t, anon, qt.Equals, 2)

	// A destination the pool would hand out: capacity void-filled, length 0.
	var dst EnvironmentFrame
	dst.PreAllocateBindings(4)

	copied := le.copyForApplyInto(&dst.local)
	qt.Assert(t, copied, qt.Equals, 2)
	qt.Assert(t, len(dst.local.Bindings()), qt.Equals, 3)
	qt.Assert(t, dst.local.Bindings()[anon].Value(), valuestest.SchemeEquals, values.Void)

	// An apply frame has no exempt tail of its own — every slot is live.
	qt.Assert(t, dst.local.CopyCount(), qt.Equals, 3)
}

// TestResetForPool_RestoresVoidNotNil is the recycling half of the invariant
// above: a slot the previous activation wrote must come back as #!void, because
// the next copyForApplyInto reslices onto it without writing.
func TestResetForPool_RestoresVoidNotNil(t *testing.T) {
	le := NewLocalEnvironment(1)
	anon := le.AppendAnonymousSlot()

	var f EnvironmentFrame
	f.PreAllocateBindings(4)
	le.copyForApplyInto(&f.local)
	f.local.Bindings()[anon].SetValue(values.NewInteger(7))

	f.ResetForPool()
	full := f.LocalBindingsSlice()[:cap(f.LocalBindingsSlice())]
	for i := range full {
		qt.Assert(t, full[i].Value(), valuestest.SchemeEquals, values.Void)
		qt.Assert(t, full[i].BindingType(), qt.Equals, BindingTypeUnknown)
		qt.Assert(t, full[i].Meta(), qt.IsNil)
	}

	// And the reslice after the reset sees void, not the stale 7.
	le.copyForApplyInto(&f.local)
	qt.Assert(t, f.local.Bindings()[anon].Value(), valuestest.SchemeEquals, values.Void)
}
