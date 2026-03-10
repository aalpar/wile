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
	"github.com/aalpar/wile/registry"
)

// Extension is the core extension containing required primitives.
var Extension = registry.NewExtension("core", AddToRegistry)

// Builder aggregates all core registration functions.
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
	addParameters,
	addPrompts,
	addBoxes,
	addHashtables,
	addExceptions,
	addBootstrapSources,
)

// AddToRegistry registers all core primitives.
var AddToRegistry = Builder.AddToRegistry
