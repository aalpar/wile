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
//
//nolint:govet // Using unkeyed struct fields for concise primitive specs
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
		{"make-channel", 1, true, PrimMakeChannel},
		{"channel?", 1, false, PrimChannelQ},
		{"channel-send!", 2, false, PrimChannelSend},
		{"channel-receive", 1, false, PrimChannelReceive},
		{"channel-try-send!", 2, false, PrimChannelTrySend},
		{"channel-try-receive", 1, false, PrimChannelTryReceive},
		{"channel-close!", 1, false, PrimChannelClose},
		{"channel-closed?", 1, false, PrimChannelClosedQ},
		{"channel-length", 1, false, PrimChannelLength},
		{"channel-capacity", 1, false, PrimChannelCapacity},
	}, registry.PhaseRuntime)
	return nil
}

func addWaitGroup(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-wait-group", 0, false, PrimMakeWaitGroup},
		{"wait-group?", 1, false, PrimWaitGroupQ},
		{"wait-group-add!", 2, false, PrimWaitGroupAdd},
		{"wait-group-done!", 1, false, PrimWaitGroupDone},
		{"wait-group-wait!", 1, false, PrimWaitGroupWait},
	}, registry.PhaseRuntime)
	return nil
}

func addRWMutex(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-rw-mutex", 1, true, PrimMakeRWMutex},
		{"rw-mutex?", 1, false, PrimRWMutexQ},
		{"rw-mutex-read-lock!", 1, false, PrimRWMutexReadLock},
		{"rw-mutex-read-unlock!", 1, false, PrimRWMutexReadUnlock},
		{"rw-mutex-write-lock!", 1, false, PrimRWMutexWriteLock},
		{"rw-mutex-write-unlock!", 1, false, PrimRWMutexWriteUnlock},
		{"rw-mutex-try-read-lock!", 1, false, PrimRWMutexTryReadLock},
		{"rw-mutex-try-write-lock!", 1, false, PrimRWMutexTryWriteLock},
	}, registry.PhaseRuntime)
	return nil
}

func addOnce(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-once", 0, false, PrimMakeOnce},
		{"once?", 1, false, PrimOnceQ},
		{"once-do!", 2, false, PrimOnceDo},
		{"once-done?", 1, false, PrimOnceDoneQ},
	}, registry.PhaseRuntime)
	return nil
}

func addAtomic(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-atomic", 1, false, PrimMakeAtomic},
		{"atomic?", 1, false, PrimAtomicQ},
		{"atomic-load", 1, false, PrimAtomicLoad},
		{"atomic-store!", 2, false, PrimAtomicStore},
		{"atomic-swap!", 2, false, PrimAtomicSwap},
		{"atomic-compare-and-swap!", 3, false, PrimAtomicCompareAndSwap},
	}, registry.PhaseRuntime)
	return nil
}
