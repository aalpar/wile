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
package all

import (
	"github.com/aalpar/wile/extensions/exceptions"
	"github.com/aalpar/wile/extensions/files"
	"github.com/aalpar/wile/extensions/gointerop"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/internal/extensions/eval"
	"github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/registry"
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

// SafeBuilder includes only the safe parts of all: records, promises, strings,
// and characters. It excludes sub-extensions that grant ambient authority
// (files, system, eval, gointerop, threads).
var SafeBuilder = registry.NewRegistryBuilder(
	addRecords,
	addPromises,
	addMoreStrings,
	addMoreChars,
)

// SafeExtension includes records, promises, and additional string/character
// operations without any privileged sub-extensions.
var SafeExtension = registry.NewExtension("all-safe", SafeBuilder.AddToRegistry)

func addRecords(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-record-type", ParamCount: 2, Impl: PrimMakeRecordType,
			Doc: "Creates a new record type from name and field names.", ParamNames: []string{"name", "field-names"}, Category: "records"},
		{Name: "record-type?", ParamCount: 1, Impl: PrimIsRecordType,
			Doc: "Returns #t if obj is a record type.", ParamNames: []string{"obj"}, Category: "records"},
		{Name: "record?", ParamCount: 1, Impl: PrimIsRecord,
			Doc: "Returns #t if obj is a record.", ParamNames: []string{"obj"}, Category: "records"},
		{Name: "record-type", ParamCount: 1, Impl: PrimRecordType,
			Doc: "Returns the record type of a record.", ParamNames: []string{"record"}, Category: "records"},
		{Name: "record-constructor", ParamCount: 2, Impl: PrimRecordConstructor,
			Doc: "Creates a constructor procedure for a record type.", ParamNames: []string{"rtd", "field-tags"}, Category: "records"},
		{Name: "record-predicate", ParamCount: 1, Impl: PrimRecordPredicate,
			Doc: "Creates a predicate procedure for a record type.", ParamNames: []string{"rtd"}, Category: "records"},
		{Name: "record-accessor", ParamCount: 2, Impl: PrimRecordAccessor,
			Doc: "Creates a field accessor for a record type.", ParamNames: []string{"rtd", "field-tag"}, Category: "records"},
		{Name: "record-modifier", ParamCount: 2, Impl: PrimRecordModifier,
			Doc: "Creates a field modifier for a record type.", ParamNames: []string{"rtd", "field-tag"}, Category: "records"},
	}, registry.PhaseRuntime)
	return nil
}

func addPromises(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "promise?", ParamCount: 1, Impl: PrimPromiseQ,
			Doc: "Returns #t if obj is a promise.", ParamNames: []string{"obj"}, Category: "promises"},
		{Name: "make-promise", ParamCount: 1, Impl: PrimMakePromise,
			Doc: "Creates an eager promise wrapping a value.", ParamNames: []string{"obj"}, Category: "promises"},
		{Name: "force", ParamCount: 1, Impl: PrimForce,
			Doc: "Forces a promise and returns its value.", ParamNames: []string{"promise"}, Category: "promises"},
		{Name: "%make-lazy-promise", ParamCount: 1, Impl: PrimMakeLazyPromise,
			Doc: "Creates a lazy promise from a thunk.", ParamNames: []string{"thunk"}, Category: "promises"},
	}, registry.PhaseRuntime)
	return nil
}

func addMoreStrings(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string-copy!", ParamCount: 2, IsVariadic: true, Impl: PrimStringCopyTo,
			Doc: "Copies characters from one string to another.", ParamNames: []string{"to", "at"}, Category: "strings"},
		{Name: "string-fill!", ParamCount: 2, IsVariadic: true, Impl: PrimStringFill,
			Doc: "Fills a string range with a character.", ParamNames: []string{"string", "char"}, Category: "strings"},
		{Name: "string-map", ParamCount: 2, IsVariadic: true, Impl: PrimStringMap,
			Doc: "Maps a procedure over string characters.", ParamNames: []string{"proc", "string"}, Category: "strings"},
		{Name: "string-for-each", ParamCount: 2, IsVariadic: true, Impl: PrimStringForEach,
			Doc: "Applies a procedure to string characters for side effects.", ParamNames: []string{"proc", "string"}, Category: "strings"},
		{Name: "string-upcase", ParamCount: 1, Impl: PrimStringUpcase,
			Doc: "Returns the uppercase version of a string.", ParamNames: []string{"string"}, Category: "strings"},
		{Name: "string-downcase", ParamCount: 1, Impl: PrimStringDowncase,
			Doc: "Returns the lowercase version of a string.", ParamNames: []string{"string"}, Category: "strings"},
		{Name: "string-foldcase", ParamCount: 1, Impl: PrimStringFoldcase,
			Doc: "Returns the case-folded version of a string.", ParamNames: []string{"string"}, Category: "strings"},
	}, registry.PhaseRuntime)

	// Case-insensitive string comparisons (generated from stringCiCompareSpecs table)
	stringCiPrims := make([]registry.PrimitiveSpec, len(stringCiCompareSpecs))
	for i, spec := range stringCiCompareSpecs {
		stringCiPrims[i] = registry.PrimitiveSpec{
			Name: spec.name, ParamCount: 2, IsVariadic: true,
			Impl:       makeStringCiComparePrimitive(spec.name, spec.cmp),
			Doc:        "Compares strings case-insensitively.",
			ParamNames: []string{"s1", "s2"}, Category: "strings",
		}
	}
	r.AddPrimitives(stringCiPrims, registry.PhaseRuntime)
	return nil
}

func addMoreChars(r *registry.Registry) error {
	// Case-insensitive character comparisons (generated from charCiCompareSpecs table)
	charCiPrims := make([]registry.PrimitiveSpec, len(charCiCompareSpecs))
	for i, spec := range charCiCompareSpecs {
		charCiPrims[i] = registry.PrimitiveSpec{
			Name: spec.name, ParamCount: 2, IsVariadic: true,
			Impl:       makeCharCiComparePrimitive(spec.name, spec.cmp),
			Doc:        "Compares characters case-insensitively.",
			ParamNames: []string{"c1", "c2"}, Category: "characters",
		}
	}
	r.AddPrimitives(charCiPrims, registry.PhaseRuntime)

	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "char-alphabetic?", ParamCount: 1, Impl: PrimCharAlphabeticQ,
			Doc: "Returns #t if char is alphabetic.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-numeric?", ParamCount: 1, Impl: PrimCharNumericQ,
			Doc: "Returns #t if char is numeric.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-whitespace?", ParamCount: 1, Impl: PrimCharWhitespaceQ,
			Doc: "Returns #t if char is whitespace.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-upper-case?", ParamCount: 1, Impl: PrimCharUpperCaseQ,
			Doc: "Returns #t if char is uppercase.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-lower-case?", ParamCount: 1, Impl: PrimCharLowerCaseQ,
			Doc: "Returns #t if char is lowercase.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-upcase", ParamCount: 1, Impl: PrimCharUpcase,
			Doc: "Returns the uppercase version of a character.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-downcase", ParamCount: 1, Impl: PrimCharDowncase,
			Doc: "Returns the lowercase version of a character.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-foldcase", ParamCount: 1, Impl: PrimCharFoldcase,
			Doc: "Returns the case-folded version of a character.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "digit-value", ParamCount: 1, Impl: PrimDigitValue,
			Doc: "Returns the numeric value of a digit character, or #f.", ParamNames: []string{"char"}, Category: "characters"},
	}, registry.PhaseRuntime)
	return nil
}
