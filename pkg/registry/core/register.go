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

// Package core provides the core primitives required for Scheme to function.
// These primitives are always included and cannot be omitted.
package core

import (
	"github.com/aalpar/wile/pkg/registry"
)

// Extension is the core extension containing required primitives.
var Extension = registry.NewExtension("core", AddToRegistry)

// Builder aggregates all core registration functions.
//
// ADDING A NEW CORE PRIMITIVE requires updates in these locations:
//
//  1. registry/core/<category>.go     — add PrimitiveSpec to the appropriate addXxx function
//  2. registry/core/prim_<category>.go — implement the ForeignFunction
//  3. registry/core/register.go       — add addXxx to Builder (only if creating a new category)
//  4. registry/core/prim_<category>_test.go — add table-driven tests
//
// If the primitive invokes a Scheme procedure (its Impl reaches ApplyCallable or
// runs a sub-context), it MUST set InvokesProcedure:true on its PrimitiveSpec —
// see the field doc in registry/primitive_registry.go. TestInvokesProcedureStaticGuard
// (pkg/wile) fails CI if the annotation is missing.
var Builder = registry.NewRegistryBuilder(
	addSpecialForms,
	addPredicates,
	addEquality,
	addPairs,
	addLists,
	addArithmetic,
	addControl,
	addReflection,
	addVectors,
	addStrings,
	addCharacters,
	addBytevectors,
	addSyntax,
	addSyntaxLoc,
	addParameters,
	addPrompts,
	addBoxes,
	addOpaque,
	addHashes,
	addHashtables,
	addExceptions,
	addContMarks,
	addTimer,
	addBootstrapSources,
)

// AddToRegistry registers all core primitives.
var AddToRegistry = Builder.AddToRegistry
