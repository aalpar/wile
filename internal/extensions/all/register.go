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
	"github.com/aalpar/wile/extensions/files"
	"github.com/aalpar/wile/extensions/gointerop"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/extensions/process"
	"github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/internal/extensions/eval"
	"github.com/aalpar/wile/internal/extensions/io"
	nsext "github.com/aalpar/wile/internal/extensions/namespace"
	"github.com/aalpar/wile/registry"
)

// Extension includes all standard extensions.
var Extension = registry.NewExtension("all", AddToRegistry)

// Builder aggregates all extension registration functions.
var Builder = registry.NewRegistryBuilder(
	io.AddToRegistry,
	files.AddToRegistry,
	system.AddToRegistry,
	math.AddToRegistry,
	eval.AddToRegistry,
	nsext.AddToRegistry,
	threads.AddToRegistry,
	gointerop.AddToRegistry,
	process.AddToRegistry,
	addRecords,
	addPromises,
	addMoreStrings,
	addMoreChars,
)

// AddToRegistry registers all standard primitives.
var AddToRegistry = Builder.AddToRegistry

// SafeBuilder includes only the safe parts of all: records, promises, strings,
// and characters. It excludes sub-extensions that grant ambient authority
// (files, system, eval, gointerop, threads, process).
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
			Doc: "Creates a new record type descriptor with the given name (symbol) and field-names (list of symbols).", ParamNames: []string{"name", "field-names"}, Category: "records"},
		{Name: "record-type?", ParamCount: 1, Impl: PrimIsRecordType,
			Doc: "Returns #t if obj is a record type descriptor.", ParamNames: []string{"obj"}, Category: "records"},
		{Name: "record?", ParamCount: 1, Impl: PrimIsRecord,
			Doc: "Returns #t if obj is a record instance of any record type.", ParamNames: []string{"obj"}, Category: "records"},
		{Name: "record-type", ParamCount: 1, Impl: PrimRecordType,
			Doc: "Returns the record type descriptor of a record instance.", ParamNames: []string{"record"}, Category: "records"},
		{Name: "record-constructor", ParamCount: 2, Impl: PrimRecordConstructor,
			Doc: "Returns a constructor procedure for rtd that initializes the fields named in field-tags.", ParamNames: []string{"rtd", "field-tags"}, Category: "records"},
		{Name: "record-predicate", ParamCount: 1, Impl: PrimRecordPredicate,
			Doc: "Returns a predicate procedure that returns #t for instances of rtd.", ParamNames: []string{"rtd"}, Category: "records"},
		{Name: "record-accessor", ParamCount: 2, Impl: PrimRecordAccessor,
			Doc: "Returns an accessor procedure that retrieves field-tag from instances of rtd.", ParamNames: []string{"rtd", "field-tag"}, Category: "records"},
		{Name: "record-modifier", ParamCount: 2, Impl: PrimRecordModifier,
			Doc: "Returns a modifier procedure that sets field-tag on instances of rtd.", ParamNames: []string{"rtd", "field-tag"}, Category: "records"},
	}, registry.PhaseRuntime)
	return nil
}

func addPromises(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "promise?", ParamCount: 1, Impl: PrimPromiseQ,
			Doc: "Returns #t if obj is a promise (created by delay, delay-force, or make-promise).", ParamNames: []string{"obj"}, Category: "promises"},
		{Name: "make-promise", ParamCount: 1, Impl: PrimMakePromise,
			Doc: "Returns an eager (already-forced) promise wrapping obj. If obj is already a promise, returns it unchanged.", ParamNames: []string{"obj"}, Category: "promises"},
		{Name: "force", ParamCount: 1, Impl: PrimForce,
			Doc: "Forces evaluation of a promise and returns its memoized value. Non-promise arguments are returned unchanged.", ParamNames: []string{"promise"}, Category: "promises"},
		{Name: "%make-lazy-promise", ParamCount: 1, Impl: PrimMakeLazyPromise,
			Doc: "Internal: creates a lazy promise from thunk. Used by the delay macro.", ParamNames: []string{"thunk"}, Category: "promises"},
	}, registry.PhaseRuntime)
	return nil
}

func addMoreStrings(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string-copy!", ParamCount: 2, IsVariadic: true, Impl: PrimStringCopyTo,
			Doc: "Copies characters from the from string into the to string starting at position at.", ParamNames: []string{"to", "at"}, Category: "strings"},
		{Name: "string-fill!", ParamCount: 2, IsVariadic: true, Impl: PrimStringFill,
			Doc: "Fills string positions from start to end with char. Start defaults to 0, end to string length.", ParamNames: []string{"string", "char"}, Category: "strings"},
		{Name: "string-upcase", ParamCount: 1, Impl: PrimStringUpcase,
			Doc: "Returns a new string with all characters converted to uppercase using full Unicode case mapping.", ParamNames: []string{"string"}, Category: "strings"},
		{Name: "string-downcase", ParamCount: 1, Impl: PrimStringDowncase,
			Doc: "Returns a new string with all characters converted to lowercase using full Unicode case mapping.", ParamNames: []string{"string"}, Category: "strings"},
		{Name: "string-foldcase", ParamCount: 1, Impl: PrimStringFoldcase,
			Doc: "Returns a new string with full Unicode case folding applied. Useful for case-insensitive comparison.", ParamNames: []string{"string"}, Category: "strings"},
	}, registry.PhaseRuntime)

	// Case-insensitive string comparisons (generated from stringCiCompareSpecs table)
	stringCiPrims := make([]registry.PrimitiveSpec, len(stringCiCompareSpecs))
	for i, spec := range stringCiCompareSpecs {
		stringCiPrims[i] = registry.PrimitiveSpec{
			Name: spec.name, ParamCount: 2, IsVariadic: true,
			Impl:       makeStringCiComparePrimitive(spec.name, spec.cmp),
			Doc:        spec.doc,
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
			Doc:        spec.doc,
			ParamNames: []string{"c1", "c2"}, Category: "characters",
		}
	}
	r.AddPrimitives(charCiPrims, registry.PhaseRuntime)

	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "char-alphabetic?", ParamCount: 1, Impl: PrimCharAlphabeticQ,
			Doc: "Returns #t if char is a Unicode letter (Lu, Ll, Lt, Lm, Lo categories).", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-numeric?", ParamCount: 1, Impl: PrimCharNumericQ,
			Doc: "Returns #t if char is a Unicode decimal digit.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-whitespace?", ParamCount: 1, Impl: PrimCharWhitespaceQ,
			Doc: "Returns #t if char is a Unicode whitespace character.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-upper-case?", ParamCount: 1, Impl: PrimCharUpperCaseQ,
			Doc: "Returns #t if char is an uppercase Unicode letter.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-lower-case?", ParamCount: 1, Impl: PrimCharLowerCaseQ,
			Doc: "Returns #t if char is a lowercase Unicode letter.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-upcase", ParamCount: 1, Impl: PrimCharUpcase,
			Doc: "Returns the uppercase form of char. Simple (1:1) Unicode case mapping.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-downcase", ParamCount: 1, Impl: PrimCharDowncase,
			Doc: "Returns the lowercase form of char. Simple (1:1) Unicode case mapping.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-foldcase", ParamCount: 1, Impl: PrimCharFoldcase,
			Doc: "Returns the case-folded form of char for case-insensitive comparison.", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "digit-value", ParamCount: 1, Impl: PrimDigitValue,
			Doc: "Returns the numeric value (0-9) of a Unicode decimal digit character, or #f if not a digit.", ParamNames: []string{"char"}, Category: "characters"},
	}, registry.PhaseRuntime)
	return nil
}
