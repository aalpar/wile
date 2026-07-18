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
// Exposes Go's concurrency primitives: RWMutex, Once, AtomicBox

package gointerop

import (
	"context"
	"errors"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// =============================================================================
// Blocking-lock cancellation
// =============================================================================

// finishBlockingSync completes a blocking lock acquisition whose acquire returned
// `acquired`. On success it sets Void. On cancellation it raises
// ErrOperationCancelled, EXCEPT when the ctx cause is ErrTimerExpired
// (with-timeout), where it returns Void so callForeignCached's eager recheck can
// raise ErrTimerInterrupt and run the handler: the recheck fires only on the
// error-free return, so raising on the timer source would bypass it and defeat
// the handler. A cancelled acquire never holds the lock, so returning here leaves
// nothing half-held.
func finishBlockingSync(mc machine.CallContext, acquired bool, site string) error {
	if acquired {
		mc.SetValue(values.Void)
		return nil
	}
	if errors.Is(context.Cause(mc.Context()), machine.ErrTimerExpired) {
		mc.SetValue(values.Void)
		return nil
	}
	return werr.WrapForeignErrorf(werr.ErrOperationCancelled, "%s", site)
}

// =============================================================================
// RWMutex Primitives
// =============================================================================

// PrimMakeRWMutex creates a new RWMutex
// (make-rw-mutex [name]) -> rw-mutex
func PrimMakeRWMutex(mc machine.CallContext) error {
	name := helpers.OptionalName(mc.Arg(0))

	rwm := values.NewRWMutex(name)
	mc.SetValue(rwm)
	return nil
}

// PrimRWMutexQ tests if an object is an RWMutex
// (rw-mutex? obj) -> boolean
var PrimRWMutexQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.RWMutex)
	return ok
})

// PrimRWMutexReadLock acquires the read lock, blocking until granted or ctx is
// cancelled. On cancellation it raises ErrOperationCancelled (with the
// ErrTimerExpired carve-out); a cancelled acquire never holds the lock.
// (rw-mutex-read-lock! rwm) -> void
func PrimRWMutexReadLock(mc machine.CallContext) error {
	rwm, err := helpers.RequireArg[*values.RWMutex](mc, 0, werr.ErrNotARWMutex, "rw-mutex-read-lock!")
	if err != nil {
		return err
	}
	return finishBlockingSync(mc, rwm.RLockContext(mc.Context()), "rw-mutex-read-lock!")
}

// PrimRWMutexReadUnlock releases the read lock
// (rw-mutex-read-unlock! rwm) -> void
var PrimRWMutexReadUnlock = helpers.MakeUnarySideEffect(werr.ErrNotARWMutex, "rw-mutex-read-unlock!", func(rwm *values.RWMutex) {
	rwm.RUnlock()
})

// PrimRWMutexWriteLock acquires the write lock, blocking until granted or ctx is
// cancelled. On cancellation it raises ErrOperationCancelled (with the
// ErrTimerExpired carve-out); a cancelled acquire never holds the lock.
// (rw-mutex-write-lock! rwm) -> void
func PrimRWMutexWriteLock(mc machine.CallContext) error {
	rwm, err := helpers.RequireArg[*values.RWMutex](mc, 0, werr.ErrNotARWMutex, "rw-mutex-write-lock!")
	if err != nil {
		return err
	}
	return finishBlockingSync(mc, rwm.LockContext(mc.Context()), "rw-mutex-write-lock!")
}

// PrimRWMutexWriteUnlock releases the write lock
// (rw-mutex-write-unlock! rwm) -> void
var PrimRWMutexWriteUnlock = helpers.MakeUnarySideEffect(werr.ErrNotARWMutex, "rw-mutex-write-unlock!", func(rwm *values.RWMutex) {
	rwm.Unlock()
})

// PrimRWMutexTryReadLock tries to acquire the read lock
// (rw-mutex-try-read-lock! rwm) -> boolean
var PrimRWMutexTryReadLock = helpers.MakeUnaryAccessor(werr.ErrNotARWMutex, "rw-mutex-try-read-lock!", func(rwm *values.RWMutex) values.Value {
	return values.BoolToBoolean(rwm.TryRLock())
})

// PrimRWMutexTryWriteLock tries to acquire the write lock
// (rw-mutex-try-write-lock! rwm) -> boolean
var PrimRWMutexTryWriteLock = helpers.MakeUnaryAccessor(werr.ErrNotARWMutex, "rw-mutex-try-write-lock!", func(rwm *values.RWMutex) values.Value {
	return values.BoolToBoolean(rwm.TryLock())
})

// =============================================================================
// Once Primitives
// =============================================================================

// PrimMakeOnce creates a new Once
// (make-once) -> once
func PrimMakeOnce(mc machine.CallContext) error {
	once := values.NewOnce()
	mc.SetValue(once)
	return nil
}

// PrimOnceQ tests if an object is a Once
// (once? obj) -> boolean
var PrimOnceQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Once)
	return ok
})

// PrimOnceDo executes the thunk only once
// (once-do! once thunk) -> boolean (true if executed, false if already done)
func PrimOnceDo(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "once-do!")
	if err != nil {
		return err
	}
	once, err := helpers.RequireArg[*values.Once](mc, 0, werr.ErrNotAOnce, "once-do!")
	if err != nil {
		return err
	}
	thunk := mc.Arg(1)

	var thunkErr error
	executed := once.Do(func() {
		// Execute the thunk in a sub-context
		cls, ok := thunk.(machine.Closure)
		if !ok {
			return // Can't execute non-closure
		}

		sub := mc.NewSubContext()
		defer machine.ReleaseSubContext(sub)
		_, err := sub.ApplyCallable(cls)
		if err != nil {
			thunkErr = err
			return
		}
		err = sub.RunWithinBoundary()
		if err != nil {
			thunkErr = err
			return
		}
	})

	if thunkErr != nil {
		return thunkErr
	}

	mc.SetValue(values.BoolToBoolean(executed))
	return nil
}

// PrimOnceDoneQ tests if the Once has been executed
// (once-done? once) -> boolean
var PrimOnceDoneQ = helpers.MakeUnaryAccessor(werr.ErrNotAOnce, "once-done?", func(once *values.Once) values.Value {
	return values.BoolToBoolean(once.Done())
})

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
