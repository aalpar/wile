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

// Package gointerop provides Go-specific concurrency primitives.
package gointerop

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the Go interop extension.
var Extension = registry.NewExtension("gointerop", AddToRegistry)

// Builder aggregates all Go interop registration functions.
var Builder = registry.NewRegistryBuilder(addChannels, addWaitGroup, addRWMutex, addOnce, addAtomic)

// AddToRegistry registers all Go interop primitives.
var AddToRegistry = Builder.AddToRegistry

func addChannels(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-channel", ParamCount: 1, IsVariadic: true, Impl: PrimMakeChannel,
			Doc: "Creates a new channel with optional buffer size.", ParamNames: []string{"size"}, Category: "channels"},
		{Name: "channel?", ParamCount: 1, Impl: PrimChannelQ,
			Doc: "Returns #t if obj is a channel.", ParamNames: []string{"obj"}, Category: "channels"},
		{Name: "channel-send!", ParamCount: 2, Impl: PrimChannelSend,
			Doc: "Sends a value to a channel, blocking if full.", ParamNames: []string{"channel", "value"}, Category: "channels"},
		{Name: "channel-receive", ParamCount: 1, Impl: PrimChannelReceive,
			Doc: "Receives a value from a channel, blocking if empty.", ParamNames: []string{"channel"}, Category: "channels"},
		{Name: "channel-try-send!", ParamCount: 2, Impl: PrimChannelTrySend,
			Doc: "Attempts a non-blocking send, returns #t on success.", ParamNames: []string{"channel", "value"}, Category: "channels"},
		{Name: "channel-try-receive", ParamCount: 1, Impl: PrimChannelTryReceive,
			Doc: "Attempts a non-blocking receive, returns three values.", ParamNames: []string{"channel"}, Category: "channels"},
		{Name: "channel-close!", ParamCount: 1, Impl: PrimChannelClose,
			Doc: "Closes a channel.", ParamNames: []string{"channel"}, Category: "channels"},
		{Name: "channel-closed?", ParamCount: 1, Impl: PrimChannelClosedQ,
			Doc: "Returns #t if channel is closed.", ParamNames: []string{"channel"}, Category: "channels"},
		{Name: "channel-length", ParamCount: 1, Impl: PrimChannelLength,
			Doc: "Returns the number of buffered elements.", ParamNames: []string{"channel"}, Category: "channels"},
		{Name: "channel-capacity", ParamCount: 1, Impl: PrimChannelCapacity,
			Doc: "Returns the buffer capacity of a channel.", ParamNames: []string{"channel"}, Category: "channels"},
	}, registry.PhaseRuntime)
	return nil
}

func addWaitGroup(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-wait-group", Impl: PrimMakeWaitGroup,
			Doc: "Creates a new wait group.", Category: "waitgroups"},
		{Name: "wait-group?", ParamCount: 1, Impl: PrimWaitGroupQ,
			Doc: "Returns #t if obj is a wait group.", ParamNames: []string{"obj"}, Category: "waitgroups"},
		{Name: "wait-group-add!", ParamCount: 2, Impl: PrimWaitGroupAdd,
			Doc: "Adds delta to the wait group counter.", ParamNames: []string{"wg", "delta"}, Category: "waitgroups"},
		{Name: "wait-group-done!", ParamCount: 1, Impl: PrimWaitGroupDone,
			Doc: "Decrements the wait group counter by one.", ParamNames: []string{"wg"}, Category: "waitgroups"},
		{Name: "wait-group-wait!", ParamCount: 1, Impl: PrimWaitGroupWait,
			Doc: "Blocks until the wait group counter is zero.", ParamNames: []string{"wg"}, Category: "waitgroups"},
	}, registry.PhaseRuntime)
	return nil
}

func addRWMutex(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-rw-mutex", ParamCount: 1, IsVariadic: true, Impl: PrimMakeRWMutex,
			Doc: "Creates a new read-write mutex with optional name.", ParamNames: []string{"name"}, Category: "rwmutex"},
		{Name: "rw-mutex?", ParamCount: 1, Impl: PrimRWMutexQ,
			Doc: "Returns #t if obj is a read-write mutex.", ParamNames: []string{"obj"}, Category: "rwmutex"},
		{Name: "rw-mutex-read-lock!", ParamCount: 1, Impl: PrimRWMutexReadLock,
			Doc: "Acquires a read lock.", ParamNames: []string{"rwm"}, Category: "rwmutex"},
		{Name: "rw-mutex-read-unlock!", ParamCount: 1, Impl: PrimRWMutexReadUnlock,
			Doc: "Releases a read lock.", ParamNames: []string{"rwm"}, Category: "rwmutex"},
		{Name: "rw-mutex-write-lock!", ParamCount: 1, Impl: PrimRWMutexWriteLock,
			Doc: "Acquires a write lock.", ParamNames: []string{"rwm"}, Category: "rwmutex"},
		{Name: "rw-mutex-write-unlock!", ParamCount: 1, Impl: PrimRWMutexWriteUnlock,
			Doc: "Releases a write lock.", ParamNames: []string{"rwm"}, Category: "rwmutex"},
		{Name: "rw-mutex-try-read-lock!", ParamCount: 1, Impl: PrimRWMutexTryReadLock,
			Doc: "Attempts a non-blocking read lock, returns #t on success.", ParamNames: []string{"rwm"}, Category: "rwmutex"},
		{Name: "rw-mutex-try-write-lock!", ParamCount: 1, Impl: PrimRWMutexTryWriteLock,
			Doc: "Attempts a non-blocking write lock, returns #t on success.", ParamNames: []string{"rwm"}, Category: "rwmutex"},
	}, registry.PhaseRuntime)
	return nil
}

func addOnce(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-once", Impl: PrimMakeOnce,
			Doc: "Creates a new once object.", Category: "once"},
		{Name: "once?", ParamCount: 1, Impl: PrimOnceQ,
			Doc: "Returns #t if obj is a once object.", ParamNames: []string{"obj"}, Category: "once"},
		{Name: "once-do!", ParamCount: 2, Impl: PrimOnceDo,
			Doc: "Executes thunk exactly once, returns #t if executed.", ParamNames: []string{"once", "thunk"}, Category: "once"},
		{Name: "once-done?", ParamCount: 1, Impl: PrimOnceDoneQ,
			Doc: "Returns #t if the once has already been executed.", ParamNames: []string{"once"}, Category: "once"},
	}, registry.PhaseRuntime)
	return nil
}

func addAtomic(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-atomic", ParamCount: 1, Impl: PrimMakeAtomic,
			Doc: "Creates an atomic value with initial value.", ParamNames: []string{"value"}, Category: "atomic"},
		{Name: "atomic?", ParamCount: 1, Impl: PrimAtomicQ,
			Doc: "Returns #t if obj is an atomic value.", ParamNames: []string{"obj"}, Category: "atomic"},
		{Name: "atomic-load", ParamCount: 1, Impl: PrimAtomicLoad,
			Doc: "Atomically loads and returns the value.", ParamNames: []string{"atomic"}, Category: "atomic"},
		{Name: "atomic-store!", ParamCount: 2, Impl: PrimAtomicStore,
			Doc: "Atomically stores a new value.", ParamNames: []string{"atomic", "value"}, Category: "atomic"},
		{Name: "atomic-swap!", ParamCount: 2, Impl: PrimAtomicSwap,
			Doc: "Atomically swaps and returns the old value.", ParamNames: []string{"atomic", "value"}, Category: "atomic"},
		{Name: "atomic-compare-and-swap!", ParamCount: 3, Impl: PrimAtomicCompareAndSwap,
			Doc: "Atomically compares and swaps, returns #t on success.", ParamNames: []string{"atomic", "old", "new"}, Category: "atomic"},
	}, registry.PhaseRuntime)
	return nil
}
