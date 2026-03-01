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
// Exposes Go's concurrency primitives: channels, WaitGroup, RWMutex, Once, AtomicBox

package gointerop

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// =============================================================================
// Channel Primitives
// =============================================================================

// PrimMakeChannel creates a new channel
// (make-channel [buffer-size]) -> channel
func PrimMakeChannel(mc *machine.MachineContext) error {
	restVal := mc.Arg(0)

	bufferSize := 0
	// Parse optional buffer-size from rest list
	if !values.IsEmptyList(restVal) {
		restList, ok := restVal.(values.Tuple)
		if ok {
			n, ok := restList.Car().(*values.Integer)
			if ok {
				bufferSize = max(int(n.Value), 0)
			}
		}
	}

	ch := values.NewChannel(bufferSize)
	mc.SetValue(ch)
	return nil
}

// PrimChannelQ tests if an object is a channel
// (channel? obj) -> boolean
func PrimChannelQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Channel)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimChannelSend sends a value on the channel (blocking)
// (channel-send! ch value) -> void
func PrimChannelSend(mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Channel](mc, 0, werr.ErrNotAChannel, "channel-send!")
	if err != nil {
		return err
	}
	val := mc.Arg(1)

	err = ch.Send(val)
	if err != nil {
		return werr.WrapForeignErrorf(err, "channel-send!")
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimChannelReceive receives a value from the channel (blocking)
// (channel-receive ch) -> value
func PrimChannelReceive(mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Channel](mc, 0, werr.ErrNotAChannel, "channel-receive")
	if err != nil {
		return err
	}

	v, ok := ch.Receive()
	switch {
	case !ok:
		// Channel is closed
		mc.SetValue(values.Void)
	case v == nil:
		mc.SetValue(values.Void)
	default:
		mc.SetValue(v)
	}
	return nil
}

// PrimChannelTrySend attempts to send without blocking
// (channel-try-send! ch value) -> boolean
func PrimChannelTrySend(mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Channel](mc, 0, werr.ErrNotAChannel, "channel-try-send!")
	if err != nil {
		return err
	}
	val := mc.Arg(1)

	sent, err := ch.TrySend(val)
	if err != nil {
		return werr.WrapForeignErrorf(err, "channel-try-send!")
	}

	mc.SetValue(values.BoolToBoolean(sent))
	return nil
}

// PrimChannelTryReceive attempts to receive without blocking
// (channel-try-receive ch) -> (values value received? open?)
func PrimChannelTryReceive(mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Channel](mc, 0, werr.ErrNotAChannel, "channel-try-receive")
	if err != nil {
		return err
	}

	v, received, open := ch.TryReceive()

	// Return multiple values
	var val values.Value
	if v == nil {
		val = values.FalseValue
	} else {
		val = v
	}

	var receivedVal values.Value
	if received {
		receivedVal = values.TrueValue
	} else {
		receivedVal = values.FalseValue
	}

	var openVal values.Value
	if open {
		openVal = values.TrueValue
	} else {
		openVal = values.FalseValue
	}

	mc.SetValues(val, receivedVal, openVal)
	return nil
}

// PrimChannelClose closes the channel
// (channel-close! ch) -> void
func PrimChannelClose(mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Channel](mc, 0, werr.ErrNotAChannel, "channel-close!")
	if err != nil {
		return err
	}

	err = ch.Close()
	if err != nil {
		return werr.WrapForeignErrorf(err, "channel-close!")
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimChannelClosedQ tests if a channel is closed
// (channel-closed? ch) -> boolean
func PrimChannelClosedQ(mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Channel](mc, 0, werr.ErrNotAChannel, "channel-closed?")
	if err != nil {
		return err
	}

	mc.SetValue(values.BoolToBoolean(ch.IsClosed()))
	return nil
}

// PrimChannelLength returns the number of elements in the channel buffer
// (channel-length ch) -> integer
func PrimChannelLength(mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Channel](mc, 0, werr.ErrNotAChannel, "channel-length")
	if err != nil {
		return err
	}

	mc.SetValue(values.NewInteger(int64(ch.Len())))
	return nil
}

// PrimChannelCapacity returns the channel's buffer capacity
// (channel-capacity ch) -> integer
func PrimChannelCapacity(mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Channel](mc, 0, werr.ErrNotAChannel, "channel-capacity")
	if err != nil {
		return err
	}

	mc.SetValue(values.NewInteger(int64(ch.Cap())))
	return nil
}

// =============================================================================
// WaitGroup Primitives
// =============================================================================

// PrimMakeWaitGroup creates a new WaitGroup
// (make-wait-group) -> wait-group
func PrimMakeWaitGroup(mc *machine.MachineContext) error {
	wg := values.NewWaitGroup()
	mc.SetValue(wg)
	return nil
}

// PrimWaitGroupQ tests if an object is a WaitGroup
// (wait-group? obj) -> boolean
func PrimWaitGroupQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.WaitGroup)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimWaitGroupAdd adds to the WaitGroup counter
// (wait-group-add! wg n) -> void
func PrimWaitGroupAdd(mc *machine.MachineContext) error {
	wg, err := helpers.RequireArg[*values.WaitGroup](mc, 0, werr.ErrNotAWaitGroup, "wait-group-add!")
	if err != nil {
		return err
	}

	n, err := helpers.RequireArg[*values.Integer](mc, 1, werr.ErrNotAnInteger, "wait-group-add!")
	if err != nil {
		return err
	}

	wg.Add(int(n.Value))
	mc.SetValue(values.Void)
	return nil
}

// PrimWaitGroupDone decrements the WaitGroup counter
// (wait-group-done! wg) -> void
func PrimWaitGroupDone(mc *machine.MachineContext) error {
	wg, err := helpers.RequireArg[*values.WaitGroup](mc, 0, werr.ErrNotAWaitGroup, "wait-group-done!")
	if err != nil {
		return err
	}

	wg.Done()
	mc.SetValue(values.Void)
	return nil
}

// PrimWaitGroupWait waits for the WaitGroup counter to reach zero
// (wait-group-wait! wg) -> void
func PrimWaitGroupWait(mc *machine.MachineContext) error {
	wg, err := helpers.RequireArg[*values.WaitGroup](mc, 0, werr.ErrNotAWaitGroup, "wait-group-wait!")
	if err != nil {
		return err
	}

	wg.Wait()
	mc.SetValue(values.Void)
	return nil
}

// =============================================================================
// RWMutex Primitives
// =============================================================================

// PrimMakeRWMutex creates a new RWMutex
// (make-rw-mutex [name]) -> rw-mutex
func PrimMakeRWMutex(mc *machine.MachineContext) error {
	restVal := mc.Arg(0)

	name := ""
	// Parse optional name from rest list
	if !values.IsEmptyList(restVal) {
		restList, ok := restVal.(values.Tuple)
		if ok {
			nameVal := restList.Car()
			s, ok := nameVal.(*values.String)
			if ok {
				name = s.Value
			} else {
				sym, ok := nameVal.(*values.Symbol)
				if ok {
					name = sym.Key
				}
			}
		}
	}

	rwm := values.NewRWMutex(name)
	mc.SetValue(rwm)
	return nil
}

// PrimRWMutexQ tests if an object is an RWMutex
// (rw-mutex? obj) -> boolean
func PrimRWMutexQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.RWMutex)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimRWMutexReadLock acquires the read lock
// (rw-mutex-read-lock! rwm) -> void
func PrimRWMutexReadLock(mc *machine.MachineContext) error {
	rwm, err := helpers.RequireArg[*values.RWMutex](mc, 0, werr.ErrNotARWMutex, "rw-mutex-read-lock!")
	if err != nil {
		return err
	}

	rwm.RLock()
	mc.SetValue(values.Void)
	return nil
}

// PrimRWMutexReadUnlock releases the read lock
// (rw-mutex-read-unlock! rwm) -> void
func PrimRWMutexReadUnlock(mc *machine.MachineContext) error {
	rwm, err := helpers.RequireArg[*values.RWMutex](mc, 0, werr.ErrNotARWMutex, "rw-mutex-read-unlock!")
	if err != nil {
		return err
	}

	rwm.RUnlock()
	mc.SetValue(values.Void)
	return nil
}

// PrimRWMutexWriteLock acquires the write lock
// (rw-mutex-write-lock! rwm) -> void
func PrimRWMutexWriteLock(mc *machine.MachineContext) error {
	rwm, err := helpers.RequireArg[*values.RWMutex](mc, 0, werr.ErrNotARWMutex, "rw-mutex-write-lock!")
	if err != nil {
		return err
	}

	rwm.Lock()
	mc.SetValue(values.Void)
	return nil
}

// PrimRWMutexWriteUnlock releases the write lock
// (rw-mutex-write-unlock! rwm) -> void
func PrimRWMutexWriteUnlock(mc *machine.MachineContext) error {
	rwm, err := helpers.RequireArg[*values.RWMutex](mc, 0, werr.ErrNotARWMutex, "rw-mutex-write-unlock!")
	if err != nil {
		return err
	}

	rwm.Unlock()
	mc.SetValue(values.Void)
	return nil
}

// PrimRWMutexTryReadLock tries to acquire the read lock
// (rw-mutex-try-read-lock! rwm) -> boolean
func PrimRWMutexTryReadLock(mc *machine.MachineContext) error {
	rwm, err := helpers.RequireArg[*values.RWMutex](mc, 0, werr.ErrNotARWMutex, "rw-mutex-try-read-lock!")
	if err != nil {
		return err
	}

	mc.SetValue(values.BoolToBoolean(rwm.TryRLock()))
	return nil
}

// PrimRWMutexTryWriteLock tries to acquire the write lock
// (rw-mutex-try-write-lock! rwm) -> boolean
func PrimRWMutexTryWriteLock(mc *machine.MachineContext) error {
	rwm, err := helpers.RequireArg[*values.RWMutex](mc, 0, werr.ErrNotARWMutex, "rw-mutex-try-write-lock!")
	if err != nil {
		return err
	}

	mc.SetValue(values.BoolToBoolean(rwm.TryLock()))
	return nil
}

// =============================================================================
// Once Primitives
// =============================================================================

// PrimMakeOnce creates a new Once
// (make-once) -> once
func PrimMakeOnce(mc *machine.MachineContext) error {
	once := values.NewOnce()
	mc.SetValue(once)
	return nil
}

// PrimOnceQ tests if an object is a Once
// (once? obj) -> boolean
func PrimOnceQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Once)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimOnceDo executes the thunk only once
// (once-do! once thunk) -> boolean (true if executed, false if already done)
func PrimOnceDo(mc *machine.MachineContext) error {
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
		err = sub.Run()
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
func PrimOnceDoneQ(mc *machine.MachineContext) error {
	once, err := helpers.RequireArg[*values.Once](mc, 0, werr.ErrNotAOnce, "once-done?")
	if err != nil {
		return err
	}

	mc.SetValue(values.BoolToBoolean(once.Done()))
	return nil
}

// =============================================================================
// AtomicBox Primitives
// =============================================================================

// PrimMakeAtomic creates a new AtomicBox value
// (make-atomic initial) -> atomic
func PrimMakeAtomic(mc *machine.MachineContext) error {
	initial := mc.Arg(0)

	a := values.NewAtomicBox(initial)
	mc.SetValue(a)
	return nil
}

// PrimAtomicQ tests if an object is an AtomicBox
// (atomic? obj) -> boolean
func PrimAtomicQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.AtomicBox)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimAtomicLoad atomically loads the value
// (atomic-load a) -> value
func PrimAtomicLoad(mc *machine.MachineContext) error {
	a, err := helpers.RequireArg[*values.AtomicBox](mc, 0, werr.ErrNotAnAtomic, "atomic-load")
	if err != nil {
		return err
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
func PrimAtomicStore(mc *machine.MachineContext) error {
	a, err := helpers.RequireArg[*values.AtomicBox](mc, 0, werr.ErrNotAnAtomic, "atomic-store!")
	if err != nil {
		return err
	}
	val := mc.Arg(1)

	a.Store(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimAtomicSwap atomically swaps and returns the old value
// (atomic-swap! a new) -> old
func PrimAtomicSwap(mc *machine.MachineContext) error {
	a, err := helpers.RequireArg[*values.AtomicBox](mc, 0, werr.ErrNotAnAtomic, "atomic-swap!")
	if err != nil {
		return err
	}
	newVal := mc.Arg(1)

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
func PrimAtomicCompareAndSwap(mc *machine.MachineContext) error {
	a, err := helpers.RequireArg[*values.AtomicBox](mc, 0, werr.ErrNotAnAtomic, "atomic-compare-and-swap!")
	if err != nil {
		return err
	}
	oldVal := mc.Arg(1)
	newVal := mc.Arg(2)

	mc.SetValue(values.BoolToBoolean(a.CompareAndSwap(oldVal, newVal)))
	return nil
}
