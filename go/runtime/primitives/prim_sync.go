// Copyright 2025 Aaron Alpar
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

// Go Sync Package Primitives for Scheme
// Exposes Go's sync package primitives: WaitGroup, RWMutex, Once, Atomic

package primitives

import (
	"context"
	"errors"

	"wile/machine"
	"wile/values"
)

// ============================================================================
// WaitGroup Primitives
// ============================================================================

// PrimMakeWaitGroup creates a new WaitGroup
// (make-wait-group) -> wait-group
func PrimMakeWaitGroup(_ context.Context, mc *machine.MachineContext) error {
	wg := values.NewWaitGroup()
	mc.SetValue(wg)
	return nil
}

// PrimWaitGroupQ tests if an object is a WaitGroup
// (wait-group? obj) -> boolean
func PrimWaitGroupQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.WaitGroup)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimWaitGroupAdd adds to the WaitGroup counter
// (wait-group-add! wg n) -> void
func PrimWaitGroupAdd(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	nVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	wg, ok := o.(*values.WaitGroup)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAWaitGroup, "wait-group-add!: expected wait-group, got %T", o)
	}

	n, ok := nVal.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "wait-group-add!: expected integer, got %T", nVal)
	}

	wg.Add(int(n.Value))
	mc.SetValue(values.Void)
	return nil
}

// PrimWaitGroupDone decrements the WaitGroup counter
// (wait-group-done! wg) -> void
func PrimWaitGroupDone(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	wg, ok := o.(*values.WaitGroup)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAWaitGroup, "wait-group-done!: expected wait-group, got %T", o)
	}

	wg.Done()
	mc.SetValue(values.Void)
	return nil
}

// PrimWaitGroupWait waits for the WaitGroup counter to reach zero
// (wait-group-wait! wg) -> void
func PrimWaitGroupWait(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	wg, ok := o.(*values.WaitGroup)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAWaitGroup, "wait-group-wait!: expected wait-group, got %T", o)
	}

	wg.Wait()
	mc.SetValue(values.Void)
	return nil
}

// ============================================================================
// RWMutex Primitives
// ============================================================================

// PrimMakeRWMutex creates a new RWMutex
// (make-rw-mutex [name]) -> rw-mutex
func PrimMakeRWMutex(_ context.Context, mc *machine.MachineContext) error {
	restVal := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	name := ""
	// Parse optional name from rest list
	if !values.IsEmptyList(restVal) {
		if restList, ok := restVal.(*values.Pair); ok {
			nameVal := restList.Car()
			if s, ok := nameVal.(*values.String); ok {
				name = s.Value
			} else if sym, ok := nameVal.(*values.Symbol); ok {
				name = sym.Key
			}
		}
	}

	rwm := values.NewRWMutex(name)
	mc.SetValue(rwm)
	return nil
}

// PrimRWMutexQ tests if an object is an RWMutex
// (rw-mutex? obj) -> boolean
func PrimRWMutexQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.RWMutex)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimRWMutexReadLock acquires the read lock
// (rw-mutex-read-lock! rwm) -> void
func PrimRWMutexReadLock(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rwm, ok := o.(*values.RWMutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotARWMutex, "rw-mutex-read-lock!: expected rw-mutex, got %T", o)
	}

	rwm.RLock()
	mc.SetValue(values.Void)
	return nil
}

// PrimRWMutexReadUnlock releases the read lock
// (rw-mutex-read-unlock! rwm) -> void
func PrimRWMutexReadUnlock(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rwm, ok := o.(*values.RWMutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotARWMutex, "rw-mutex-read-unlock!: expected rw-mutex, got %T", o)
	}

	rwm.RUnlock()
	mc.SetValue(values.Void)
	return nil
}

// PrimRWMutexWriteLock acquires the write lock
// (rw-mutex-write-lock! rwm) -> void
func PrimRWMutexWriteLock(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rwm, ok := o.(*values.RWMutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotARWMutex, "rw-mutex-write-lock!: expected rw-mutex, got %T", o)
	}

	rwm.Lock()
	mc.SetValue(values.Void)
	return nil
}

// PrimRWMutexWriteUnlock releases the write lock
// (rw-mutex-write-unlock! rwm) -> void
func PrimRWMutexWriteUnlock(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rwm, ok := o.(*values.RWMutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotARWMutex, "rw-mutex-write-unlock!: expected rw-mutex, got %T", o)
	}

	rwm.Unlock()
	mc.SetValue(values.Void)
	return nil
}

// PrimRWMutexTryReadLock tries to acquire the read lock
// (rw-mutex-try-read-lock! rwm) -> boolean
func PrimRWMutexTryReadLock(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rwm, ok := o.(*values.RWMutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotARWMutex, "rw-mutex-try-read-lock!: expected rw-mutex, got %T", o)
	}

	if rwm.TryRLock() {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimRWMutexTryWriteLock tries to acquire the write lock
// (rw-mutex-try-write-lock! rwm) -> boolean
func PrimRWMutexTryWriteLock(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rwm, ok := o.(*values.RWMutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotARWMutex, "rw-mutex-try-write-lock!: expected rw-mutex, got %T", o)
	}

	if rwm.TryLock() {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// ============================================================================
// Once Primitives
// ============================================================================

// PrimMakeOnce creates a new Once
// (make-once) -> once
func PrimMakeOnce(_ context.Context, mc *machine.MachineContext) error {
	once := values.NewOnce()
	mc.SetValue(once)
	return nil
}

// PrimOnceQ tests if an object is a Once
// (once? obj) -> boolean
func PrimOnceQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.Once)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimOnceDo executes the thunk only once
// (once-do! once thunk) -> boolean (true if executed, false if already done)
func PrimOnceDo(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	thunk := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	once, ok := o.(*values.Once)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAOnce, "once-do!: expected once, got %T", o)
	}

	executed := once.Do(func() {
		// Execute the thunk in a sub-context
		cls, ok := thunk.(*machine.MachineClosure)
		if !ok {
			return // Can't execute non-closure
		}

		sub := mc.NewSubContext()
		if _, err := sub.Apply(cls); err != nil {
			return
		}
		err := sub.Run(context.Background())
		if err != nil && !errors.Is(err, machine.ErrMachineHalt) {
			return
		}
	})

	if executed {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimOnceDoneQ tests if the Once has been executed
// (once-done? once) -> boolean
func PrimOnceDoneQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	once, ok := o.(*values.Once)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAOnce, "once-done?: expected once, got %T", o)
	}

	if once.Done() {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// ============================================================================
// Atomic Primitives
// ============================================================================

// PrimMakeAtomic creates a new Atomic value
// (make-atomic initial) -> atomic
func PrimMakeAtomic(_ context.Context, mc *machine.MachineContext) error {
	initial := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	a := values.NewAtomic(initial)
	mc.SetValue(a)
	return nil
}

// PrimAtomicQ tests if an object is an Atomic
// (atomic? obj) -> boolean
func PrimAtomicQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.Atomic)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimAtomicLoad atomically loads the value
// (atomic-load a) -> value
func PrimAtomicLoad(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	a, ok := o.(*values.Atomic)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnAtomic, "atomic-load: expected atomic, got %T", o)
	}

	v := a.Load()
	if v == nil {
		mc.SetValue(values.Void)
	} else {
		mc.SetValue(v)
	}
	return nil
}

// PrimAtomicStore atomically stores a value
// (atomic-store! a value) -> void
func PrimAtomicStore(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	val := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	a, ok := o.(*values.Atomic)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnAtomic, "atomic-store!: expected atomic, got %T", o)
	}

	a.Store(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimAtomicSwap atomically swaps and returns the old value
// (atomic-swap! a new) -> old
func PrimAtomicSwap(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	newVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	a, ok := o.(*values.Atomic)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnAtomic, "atomic-swap!: expected atomic, got %T", o)
	}

	old := a.Swap(newVal)
	if old == nil {
		mc.SetValue(values.Void)
	} else {
		mc.SetValue(old)
	}
	return nil
}

// PrimAtomicCompareAndSwap atomically compares and swaps
// (atomic-compare-and-swap! a old new) -> boolean
func PrimAtomicCompareAndSwap(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	oldVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	newVal := mc.EnvironmentFrame().GetLocalBindingByIndex(2).Value()

	a, ok := o.(*values.Atomic)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnAtomic, "atomic-compare-and-swap!: expected atomic, got %T", o)
	}

	if a.CompareAndSwap(oldVal, newVal) {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}
