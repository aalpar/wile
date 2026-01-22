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

// Package all provides a convenience extension that includes all standard extensions.
//
//nolint:govet // Using unkeyed struct fields for concise primitive specs
package all

import (
	"wile/extensions/eval"
	"wile/extensions/exceptions"
	"wile/extensions/files"
	"wile/extensions/gointerop"
	"wile/extensions/io"
	"wile/extensions/math"
	"wile/extensions/system"
	"wile/extensions/threads"
	"wile/registry"
	"wile/runtime/primitives"
)

// Extension includes all standard extensions.
var Extension = registry.NewExtension("all", AddToRegistry)

// Builder aggregates all extension registration functions.
var Builder = registry.RegistryBuilder{
	io.AddToRegistry,
	files.AddToRegistry,
	system.AddToRegistry,
	math.AddToRegistry,
	exceptions.AddToRegistry,
	eval.AddToRegistry,
	threads.AddToRegistry,
	gointerop.AddToRegistry,
	addRecords,
	addPromises,
	addMoreStrings,
	addMoreChars,
}

// AddToRegistry registers all standard primitives.
var AddToRegistry = Builder.AddToRegistry

func addRecords(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-record-type", 2, false, primitives.PrimMakeRecordType},
		{"record-type?", 1, false, primitives.PrimIsRecordType},
		{"record?", 1, false, primitives.PrimIsRecord},
		{"record-type", 1, false, primitives.PrimRecordType},
		{"record-constructor", 2, false, primitives.PrimRecordConstructor},
		{"record-predicate", 1, false, primitives.PrimRecordPredicate},
		{"record-accessor", 2, false, primitives.PrimRecordAccessor},
		{"record-modifier", 2, false, primitives.PrimRecordModifier},
	}, registry.PhaseRuntime)
	return nil
}

func addPromises(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"promise?", 1, false, primitives.PrimPromiseQ},
		{"make-promise", 1, false, primitives.PrimMakePromise},
		{"force", 1, false, primitives.PrimForce},
		{"%make-lazy-promise", 1, false, primitives.PrimMakeLazyPromise},
	}, registry.PhaseRuntime)
	return nil
}

func addMoreStrings(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string-copy!", 2, true, primitives.PrimStringCopyTo},
		{"string-fill!", 2, true, primitives.PrimStringFill},
		{"string-map", 2, true, primitives.PrimStringMap},
		{"string-for-each", 2, true, primitives.PrimStringForEach},
		{"string-ci=?", 2, true, primitives.PrimStringCiEqVariadic},
		{"string-ci<?", 2, true, primitives.PrimStringCiLtVariadic},
		{"string-ci>?", 2, true, primitives.PrimStringCiGtVariadic},
		{"string-ci<=?", 2, true, primitives.PrimStringCiLeVariadic},
		{"string-ci>=?", 2, true, primitives.PrimStringCiGeVariadic},
		{"string-upcase", 1, false, primitives.PrimStringUpcase},
		{"string-downcase", 1, false, primitives.PrimStringDowncase},
		{"string-foldcase", 1, false, primitives.PrimStringFoldcase},
	}, registry.PhaseRuntime)
	return nil
}

func addMoreChars(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"char-ci=?", 2, true, primitives.PrimCharCiEqVariadic},
		{"char-ci<?", 2, true, primitives.PrimCharCiLtVariadic},
		{"char-ci>?", 2, true, primitives.PrimCharCiGtVariadic},
		{"char-ci<=?", 2, true, primitives.PrimCharCiLeVariadic},
		{"char-ci>=?", 2, true, primitives.PrimCharCiGeVariadic},
		{"char-alphabetic?", 1, false, primitives.PrimCharAlphabeticQ},
		{"char-numeric?", 1, false, primitives.PrimCharNumericQ},
		{"char-whitespace?", 1, false, primitives.PrimCharWhitespaceQ},
		{"char-upper-case?", 1, false, primitives.PrimCharUpperCaseQ},
		{"char-lower-case?", 1, false, primitives.PrimCharLowerCaseQ},
		{"char-upcase", 1, false, primitives.PrimCharUpcase},
		{"char-downcase", 1, false, primitives.PrimCharDowncase},
		{"char-foldcase", 1, false, primitives.PrimCharFoldcase},
		{"digit-value", 1, false, primitives.PrimDigitValue},
	}, registry.PhaseRuntime)
	return nil
}
