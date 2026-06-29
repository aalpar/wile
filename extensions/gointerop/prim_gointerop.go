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
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// =============================================================================
// Channel Primitives
// =============================================================================

// PrimMakeChannel creates a new channel
// (make-channel [buffer-size]) -> channel
func PrimMakeChannel(mc machine.CallContext) error {
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
var PrimChannelQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Channel)
	return ok
})

// PrimChannelSend sends a value on the channel (blocking)
// (channel-send! ch value) -> void
func PrimChannelSend(mc machine.CallContext) error {
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
func PrimChannelReceive(mc machine.CallContext) error {
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
func PrimChannelTrySend(mc machine.CallContext) error {
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
func PrimChannelTryReceive(mc machine.CallContext) error {
	ch, err := helpers.RequireArg[*values.Channel](mc, 0, werr.ErrNotAChannel, "channel-try-receive")
	if err != nil {
		return err
	}

	v, received, open := ch.TryReceive()

	// Return three values: the received value (#f when none, not Void — per the
	// channel-try-receive contract), whether a value was received, and whether
	// the channel is still open.
	val := v
	if val == nil {
		val = values.FalseValue
	}
	mc.SetValues(val, values.BoolToBoolean(received), values.BoolToBoolean(open))
	return nil
}

// PrimChannelClose closes the channel
// (channel-close! ch) -> void
func PrimChannelClose(mc machine.CallContext) error {
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
var PrimChannelClosedQ = helpers.MakeUnaryAccessor(werr.ErrNotAChannel, "channel-closed?", func(ch *values.Channel) values.Value {
	return values.BoolToBoolean(ch.IsClosed())
})

// PrimChannelLength returns the number of elements in the channel buffer
// (channel-length ch) -> integer
var PrimChannelLength = helpers.MakeUnaryAccessor(werr.ErrNotAChannel, "channel-length", func(ch *values.Channel) values.Value {
	return values.NewInteger(int64(ch.Len()))
})

// PrimChannelCapacity returns the channel's buffer capacity
// (channel-capacity ch) -> integer
var PrimChannelCapacity = helpers.MakeUnaryAccessor(werr.ErrNotAChannel, "channel-capacity", func(ch *values.Channel) values.Value {
	return values.NewInteger(int64(ch.Cap()))
})

// =============================================================================
// WaitGroup Primitives
// =============================================================================

// PrimMakeWaitGroup creates a new WaitGroup
// (make-wait-group) -> wait-group
func PrimMakeWaitGroup(mc machine.CallContext) error {
	wg := values.NewWaitGroup()
	mc.SetValue(wg)
	return nil
}

// PrimWaitGroupQ tests if an object is a WaitGroup
// (wait-group? obj) -> boolean
var PrimWaitGroupQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.WaitGroup)
	return ok
})

// PrimWaitGroupAdd adds to the WaitGroup counter
// (wait-group-add! wg n) -> void
func PrimWaitGroupAdd(mc machine.CallContext) error {
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
var PrimWaitGroupDone = helpers.MakeUnarySideEffect(werr.ErrNotAWaitGroup, "wait-group-done!", func(wg *values.WaitGroup) {
	wg.Done()
})

// PrimWaitGroupWait waits for the WaitGroup counter to reach zero
// (wait-group-wait! wg) -> void
var PrimWaitGroupWait = helpers.MakeUnarySideEffect(werr.ErrNotAWaitGroup, "wait-group-wait!", func(wg *values.WaitGroup) {
	wg.Wait()
})

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

// PrimRWMutexReadLock acquires the read lock
// (rw-mutex-read-lock! rwm) -> void
var PrimRWMutexReadLock = helpers.MakeUnarySideEffect(werr.ErrNotARWMutex, "rw-mutex-read-lock!", func(rwm *values.RWMutex) {
	rwm.RLock()
})

// PrimRWMutexReadUnlock releases the read lock
// (rw-mutex-read-unlock! rwm) -> void
var PrimRWMutexReadUnlock = helpers.MakeUnarySideEffect(werr.ErrNotARWMutex, "rw-mutex-read-unlock!", func(rwm *values.RWMutex) {
	rwm.RUnlock()
})

// PrimRWMutexWriteLock acquires the write lock
// (rw-mutex-write-lock! rwm) -> void
var PrimRWMutexWriteLock = helpers.MakeUnarySideEffect(werr.ErrNotARWMutex, "rw-mutex-write-lock!", func(rwm *values.RWMutex) {
	rwm.Lock()
})

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
