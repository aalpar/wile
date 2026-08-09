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

// Go Interop Primitives for Scheme
// Exposes Go's atomic-box concurrency primitive.

package gointerop

import (
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// =============================================================================
// AtomicBox Primitives
// =============================================================================

// PrimMakeAtomic creates a new AtomicBox value
// (make-atomic initial) -> atomic
func PrimMakeAtomic(mc machine.CallContext) error {
	initial := mc.Arg(0)

	a := values.NewAtomicBox(initial)
	mc.SetValue(a)
	return nil
}

// PrimAtomicQ tests if an object is an AtomicBox
// (atomic? obj) -> boolean
var PrimAtomicQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.AtomicBox)
	return ok
})

// PrimAtomicLoad atomically loads the value
// (atomic-load a) -> value
var PrimAtomicLoad = helpers.MakeUnaryAccessor(werr.ErrNotAnAtomic, "atomic-load", func(a *values.AtomicBox) values.Value {
	return values.ValueOrVoid(a.Load())
})

// PrimAtomicStore atomically stores a value
// (atomic-store! a value) -> void
var PrimAtomicStore = helpers.MakeBinarySetter(werr.ErrNotAnAtomic, "atomic-store!", func(a *values.AtomicBox, val values.Value) {
	a.Store(val)
})

// PrimAtomicSwap atomically swaps and returns the old value
// (atomic-swap! a new) -> old
func PrimAtomicSwap(mc machine.CallContext) error {
	a, err := helpers.RequireArg[*values.AtomicBox](mc, 0, werr.ErrNotAnAtomic, "atomic-swap!")
	if err != nil {
		return err
	}
	newVal := mc.Arg(1)

	mc.SetValue(values.ValueOrVoid(a.Swap(newVal)))
	return nil
}

// PrimAtomicCompareAndSwap atomically compares and swaps
// (atomic-compare-and-swap! a old new) -> boolean
func PrimAtomicCompareAndSwap(mc machine.CallContext) error {
	a, err := helpers.RequireArg[*values.AtomicBox](mc, 0, werr.ErrNotAnAtomic, "atomic-compare-and-swap!")
	if err != nil {
		return err
	}
	oldVal := mc.Arg(1)
	newVal := mc.Arg(2)

	mc.SetValue(values.BoolToBoolean(a.CompareAndSwap(oldVal, newVal)))
	return nil
}
