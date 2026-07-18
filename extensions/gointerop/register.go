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
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

// Extension is the Go interop extension.
var Extension = registry.NewDescribedExtension("gointerop",
	"Go interop: Go value wrapping, struct access, method calls.",
	AddToRegistry)

// Builder aggregates all Go interop registration functions.
var Builder = registry.NewRegistryBuilder(addRWMutex, addOnce, addAtomic)

// AddToRegistry registers all Go interop primitives.
var AddToRegistry = Builder.AddToRegistry

func addRWMutex(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-rw-mutex", ParamCount: 1, IsVariadic: true, Impl: PrimMakeRWMutex,
			Doc: "Creates a new read-write mutex. Optional NAME for debugging.", ParamNames: []string{"name"}, Category: "rwmutex",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "rw-mutex?", ParamCount: 1, Impl: PrimRWMutexQ,
			Doc: "Returns #t if OBJ is a read-write mutex.", ParamNames: []string{"obj"}, Category: "rwmutex",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "rw-mutex-read-lock!", ParamCount: 1, Impl: PrimRWMutexReadLock,
			Doc: "Acquires a shared read lock. Multiple readers may hold the lock concurrently.", ParamNames: []string{"rwm"}, Category: "rwmutex",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeVoid},
		{Name: "rw-mutex-read-unlock!", ParamCount: 1, Impl: PrimRWMutexReadUnlock,
			Doc: "Releases a shared read lock.", ParamNames: []string{"rwm"}, Category: "rwmutex",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeVoid},
		{Name: "rw-mutex-write-lock!", ParamCount: 1, Impl: PrimRWMutexWriteLock,
			Doc: "Acquires an exclusive write lock. Blocks until all readers and writers release.", ParamNames: []string{"rwm"}, Category: "rwmutex",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeVoid},
		{Name: "rw-mutex-write-unlock!", ParamCount: 1, Impl: PrimRWMutexWriteUnlock,
			Doc: "Releases an exclusive write lock.", ParamNames: []string{"rwm"}, Category: "rwmutex",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeVoid},
		{Name: "rw-mutex-try-read-lock!", ParamCount: 1, Impl: PrimRWMutexTryReadLock,
			Doc: "Attempts a non-blocking read lock. Returns #t if acquired, #f otherwise.", ParamNames: []string{"rwm"}, Category: "rwmutex",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "rw-mutex-try-write-lock!", ParamCount: 1, Impl: PrimRWMutexTryWriteLock,
			Doc: "Attempts a non-blocking write lock. Returns #t if acquired, #f otherwise.", ParamNames: []string{"rwm"}, Category: "rwmutex",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
	}, registry.PhaseSetRuntime)
	return nil
}

func addOnce(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-once", Impl: PrimMakeOnce,
			Doc: "Creates a new once object for one-time initialization.", Category: "once",
			ReturnType: values.TypeAny},
		{Name: "once?", ParamCount: 1, Impl: PrimOnceQ,
			Doc: "Returns #t if OBJ is a once object.", ParamNames: []string{"obj"}, Category: "once",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "once-do!", InvokesProcedure: true, ParamCount: 2, Impl: PrimOnceDo,
			Doc: "Executes THUNK exactly once across all calls. Returns #t if this call executed THUNK.", ParamNames: []string{"once", "thunk"}, Category: "once",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeProcedure},
			ReturnType: values.TypeBoolean},
		{Name: "once-done?", ParamCount: 1, Impl: PrimOnceDoneQ,
			Doc: "Returns #t if ONCE's thunk has already been executed.", ParamNames: []string{"once"}, Category: "once",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
	}, registry.PhaseSetRuntime)
	return nil
}

func addAtomic(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-atomic", ParamCount: 1, Impl: PrimMakeAtomic,
			Doc: "Creates a new atomic value box initialized with VALUE. Provides lock-free concurrent access.", ParamNames: []string{"value"}, Category: "atomic",
			Keywords:   []string{"lock-free", "CAS", "compare-and-swap", "concurrent"},
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "atomic?", ParamCount: 1, Impl: PrimAtomicQ,
			Doc: "Returns #t if OBJ is an atomic value box.", ParamNames: []string{"obj"}, Category: "atomic",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "atomic-load", ParamCount: 1, Impl: PrimAtomicLoad,
			Doc: "Atomically reads and returns the current value.", ParamNames: []string{"atomic"}, Category: "atomic",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "atomic-store!", ParamCount: 2, Impl: PrimAtomicStore,
			Doc: "Atomically replaces the stored value with VALUE.", ParamNames: []string{"atomic", "value"}, Category: "atomic",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny},
			ReturnType: values.TypeVoid},
		{Name: "atomic-swap!", ParamCount: 2, Impl: PrimAtomicSwap,
			Doc: "Atomically stores VALUE and returns the previous value.", ParamNames: []string{"atomic", "value"}, Category: "atomic",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "atomic-compare-and-swap!", ParamCount: 3, Impl: PrimAtomicCompareAndSwap,
			Doc: "Atomically compares the stored value to OLD (by identity) and, if equal, replaces it with NEW. Returns #t on success.", ParamNames: []string{"atomic", "old", "new"}, Category: "atomic",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny, values.TypeAny},
			ReturnType: values.TypeBoolean},
	}, registry.PhaseSetRuntime)
	return nil
}
