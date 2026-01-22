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
		{"make-record-type", 2, false, PrimMakeRecordType},
		{"record-type?", 1, false, PrimIsRecordType},
		{"record?", 1, false, PrimIsRecord},
		{"record-type", 1, false, PrimRecordType},
		{"record-constructor", 2, false, PrimRecordConstructor},
		{"record-predicate", 1, false, PrimRecordPredicate},
		{"record-accessor", 2, false, PrimRecordAccessor},
		{"record-modifier", 2, false, PrimRecordModifier},
	}, registry.PhaseRuntime)
	return nil
}

func addPromises(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"promise?", 1, false, PrimPromiseQ},
		{"make-promise", 1, false, PrimMakePromise},
		{"force", 1, false, PrimForce},
		{"%make-lazy-promise", 1, false, PrimMakeLazyPromise},
	}, registry.PhaseRuntime)
	return nil
}

func addMoreStrings(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string-copy!", 2, true, PrimStringCopyTo},
		{"string-fill!", 2, true, PrimStringFill},
		{"string-map", 2, true, PrimStringMap},
		{"string-for-each", 2, true, PrimStringForEach},
		{"string-ci=?", 2, true, PrimStringCiEqVariadic},
		{"string-ci<?", 2, true, PrimStringCiLtVariadic},
		{"string-ci>?", 2, true, PrimStringCiGtVariadic},
		{"string-ci<=?", 2, true, PrimStringCiLeVariadic},
		{"string-ci>=?", 2, true, PrimStringCiGeVariadic},
		{"string-upcase", 1, false, PrimStringUpcase},
		{"string-downcase", 1, false, PrimStringDowncase},
		{"string-foldcase", 1, false, PrimStringFoldcase},
	}, registry.PhaseRuntime)
	return nil
}

func addMoreChars(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"char-ci=?", 2, true, PrimCharCiEqVariadic},
		{"char-ci<?", 2, true, PrimCharCiLtVariadic},
		{"char-ci>?", 2, true, PrimCharCiGtVariadic},
		{"char-ci<=?", 2, true, PrimCharCiLeVariadic},
		{"char-ci>=?", 2, true, PrimCharCiGeVariadic},
		{"char-alphabetic?", 1, false, PrimCharAlphabeticQ},
		{"char-numeric?", 1, false, PrimCharNumericQ},
		{"char-whitespace?", 1, false, PrimCharWhitespaceQ},
		{"char-upper-case?", 1, false, PrimCharUpperCaseQ},
		{"char-lower-case?", 1, false, PrimCharLowerCaseQ},
		{"char-upcase", 1, false, PrimCharUpcase},
		{"char-downcase", 1, false, PrimCharDowncase},
		{"char-foldcase", 1, false, PrimCharFoldcase},
		{"digit-value", 1, false, PrimDigitValue},
	}, registry.PhaseRuntime)
	return nil
}
