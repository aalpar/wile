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
	"wile/registry"
	"wile/runtime/primitives"
)

// Extension is the Go interop extension.
var Extension = registry.NewExtension("gointerop", AddToRegistry)

// Builder aggregates all Go interop registration functions.
var Builder = registry.NewRegistryBuilder(addChannels, addWaitGroup, addRWMutex, addOnce, addAtomic)

// AddToRegistry registers all Go interop primitives.
var AddToRegistry = Builder.AddToRegistry

func addChannels(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-channel", 1, true, primitives.PrimMakeChannel},
		{"channel?", 1, false, primitives.PrimChannelQ},
		{"channel-send!", 2, false, primitives.PrimChannelSend},
		{"channel-receive", 1, false, primitives.PrimChannelReceive},
		{"channel-try-send!", 2, false, primitives.PrimChannelTrySend},
		{"channel-try-receive", 1, false, primitives.PrimChannelTryReceive},
		{"channel-close!", 1, false, primitives.PrimChannelClose},
		{"channel-closed?", 1, false, primitives.PrimChannelClosedQ},
		{"channel-length", 1, false, primitives.PrimChannelLength},
		{"channel-capacity", 1, false, primitives.PrimChannelCapacity},
	}, registry.PhaseRuntime)
	return nil
}

func addWaitGroup(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-wait-group", 0, false, primitives.PrimMakeWaitGroup},
		{"wait-group?", 1, false, primitives.PrimWaitGroupQ},
		{"wait-group-add!", 2, false, primitives.PrimWaitGroupAdd},
		{"wait-group-done!", 1, false, primitives.PrimWaitGroupDone},
		{"wait-group-wait!", 1, false, primitives.PrimWaitGroupWait},
	}, registry.PhaseRuntime)
	return nil
}

func addRWMutex(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-rw-mutex", 1, true, primitives.PrimMakeRWMutex},
		{"rw-mutex?", 1, false, primitives.PrimRWMutexQ},
		{"rw-mutex-read-lock!", 1, false, primitives.PrimRWMutexReadLock},
		{"rw-mutex-read-unlock!", 1, false, primitives.PrimRWMutexReadUnlock},
		{"rw-mutex-write-lock!", 1, false, primitives.PrimRWMutexWriteLock},
		{"rw-mutex-write-unlock!", 1, false, primitives.PrimRWMutexWriteUnlock},
		{"rw-mutex-try-read-lock!", 1, false, primitives.PrimRWMutexTryReadLock},
		{"rw-mutex-try-write-lock!", 1, false, primitives.PrimRWMutexTryWriteLock},
	}, registry.PhaseRuntime)
	return nil
}

func addOnce(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-once", 0, false, primitives.PrimMakeOnce},
		{"once?", 1, false, primitives.PrimOnceQ},
		{"once-do!", 2, false, primitives.PrimOnceDo},
		{"once-done?", 1, false, primitives.PrimOnceDoneQ},
	}, registry.PhaseRuntime)
	return nil
}

func addAtomic(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-atomic", 1, false, primitives.PrimMakeAtomic},
		{"atomic?", 1, false, primitives.PrimAtomicQ},
		{"atomic-load", 1, false, primitives.PrimAtomicLoad},
		{"atomic-store!", 2, false, primitives.PrimAtomicStore},
		{"atomic-swap!", 2, false, primitives.PrimAtomicSwap},
		{"atomic-compare-and-swap!", 3, false, primitives.PrimAtomicCompareAndSwap},
	}, registry.PhaseRuntime)
	return nil
}
