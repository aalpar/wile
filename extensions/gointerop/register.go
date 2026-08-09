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
	"Go concurrency primitives: atomic boxes.",
	AddToRegistry)

// Builder aggregates all Go interop registration functions.
var Builder = registry.NewRegistryBuilder(addAtomic)

// AddToRegistry registers all Go interop primitives.
var AddToRegistry = Builder.AddToRegistry

func addAtomic(r *registry.PrimitiveRegistry) error {
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
