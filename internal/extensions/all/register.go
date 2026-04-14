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

// Extension registers the primitives defined in this package (records, promises,
// strings, characters) under the name "all". When used in AllExtensions() where
// sub-extensions are listed individually, this avoids double-registration.
// Use Builder.AddToRegistry for standalone use that includes all sub-extensions.
var Extension = registry.NewDescribedExtension("all",
	"All Wile extensions combined.",
	SafeBuilder.AddToRegistry)

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
var SafeExtension = registry.NewDescribedExtension("all-safe",
	"Safe subset: records, promises, strings, characters (no filesystem, eval, or system).",
	SafeBuilder.AddToRegistry)

func addRecords(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-record-type", ParamCount: 2, Impl: PrimMakeRecordType,
			Doc: "Creates a new record type descriptor with the given NAME (symbol) and FIELD-NAMES (list of symbols).\n\nExamples:\n  (make-record-type 'point '(x y))  => #<record-type point>", ParamNames: []string{"name", "field-names"}, Category: "records"},
		{Name: "make-opaque-record-type", ParamCount: 2, Impl: PrimMakeOpaqueRecordType,
			Doc: "Creates an opaque record type descriptor. Instances are hidden from record? and record-type.\n\nExamples:\n  (make-opaque-record-type 'stack '(items))  => #<record-type stack>", ParamNames: []string{"name", "field-names"}, Category: "records"},
		{Name: "record-type?", ParamCount: 1, Impl: PrimIsRecordType,
			Doc: "Returns #t if OBJ is a record type descriptor.\n\nExamples:\n  (record-type? (make-record-type 'point '(x y)))  => #t\n  (record-type? 42)                                 => #f", ParamNames: []string{"obj"}, Category: "records"},
		{Name: "record?", ParamCount: 1, Impl: PrimIsRecord,
			Doc: "Returns #t if OBJ is a record instance of any record type.\n\nExamples:\n  ; (define-record-type <point> (make-point x y) point? (x point-x) (y point-y))\n  ; (record? (make-point 1 2))  => #t", ParamNames: []string{"obj"}, Category: "records"},
		{Name: "record-type", ParamCount: 1, Impl: PrimRecordType,
			Doc: "Returns the record type descriptor of a record instance.\n\nExamples:\n  ; (record-type my-record)  => #<record-type point>", ParamNames: []string{"record"}, Category: "records"},
		{Name: "record-constructor", ParamCount: 2, Impl: PrimRecordConstructor,
			Doc: "Returns a constructor procedure for RTD that initializes the fields named in FIELD-TAGS.\n\nExamples:\n  (let* ((rt (make-record-type 'point '(x y)))\n         (mk (record-constructor rt '(x y))))\n    (mk 3 4))  => #<record point>", ParamNames: []string{"rtd", "field-tags"}, Category: "records"},
		{Name: "record-predicate", ParamCount: 1, Impl: PrimRecordPredicate,
			Doc: "Returns a predicate procedure that returns #t for instances of RTD.\n\nExamples:\n  (let* ((rt (make-record-type 'point '(x y)))\n         (point? (record-predicate rt)))\n    (point? 42))  => #f", ParamNames: []string{"rtd"}, Category: "records"},
		{Name: "record-accessor", ParamCount: 2, Impl: PrimRecordAccessor,
			Doc: "Returns an accessor procedure that retrieves FIELD-TAG from instances of RTD.\n\nExamples:\n  ; (let ((get-x (record-accessor rt 'x))) (get-x my-point))  => 3", ParamNames: []string{"rtd", "field-tag"}, Category: "records"},
		{Name: "record-modifier", ParamCount: 2, Impl: PrimRecordModifier,
			Doc: "Returns a modifier procedure that sets FIELD-TAG on instances of RTD.\n\nExamples:\n  ; (let ((set-x! (record-modifier rt 'x))) (set-x! my-point 10))", ParamNames: []string{"rtd", "field-tag"}, Category: "records"},
	}, registry.PhaseRuntime)
	return nil
}

func addPromises(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "promise?", ParamCount: 1, Impl: PrimPromiseQ,
			Doc: "Returns #t if OBJ is a promise (created by delay, delay-force, or make-promise).\n\nExamples:\n  (promise? (delay 42))      => #t\n  (promise? (make-promise 1)) => #t\n  (promise? 42)              => #f", ParamNames: []string{"obj"}, Category: "promises"},
		{Name: "make-promise", ParamCount: 1, Impl: PrimMakePromise,
			Doc: "Returns an eager (already-forced) promise wrapping OBJ. If OBJ is already a promise, returns it unchanged.\n\nExamples:\n  (force (make-promise 42))  => 42", ParamNames: []string{"obj"}, Category: "promises"},
		{Name: "force", ParamCount: 1, Impl: PrimForce,
			Doc: "Forces evaluation of PROMISE and returns its memoized value. Non-promise arguments are returned unchanged.\n\nExamples:\n  (force (delay (+ 1 2)))  => 3\n  (force 42)              => 42", ParamNames: []string{"promise"}, Category: "promises"},
		{Name: "%make-lazy-promise", ParamCount: 1, Impl: PrimMakeLazyPromise,
			Doc: "Internal: creates a lazy promise from THUNK. Used by the delay macro.\n\nExamples:\n  ; (%make-lazy-promise (lambda () 42))  ; used internally by (delay 42)", ParamNames: []string{"thunk"}, Category: "promises"},
	}, registry.PhaseRuntime)
	return nil
}

func addMoreStrings(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string-copy!", ParamCount: 2, IsVariadic: true, Impl: PrimStringCopyTo,
			Doc: "Copies characters from the from string into TO starting at position AT.\n\nExamples:\n  (let ((s (string-copy \"abcde\"))) (string-copy! s 1 \"xy\") s)  => \"axyde\"", ParamNames: []string{"to", "at"}, Category: "strings"},
		{Name: "string-fill!", ParamCount: 2, IsVariadic: true, Impl: PrimStringFill,
			Doc: "Fills STRING positions from start to end with CHAR. Start defaults to 0, end to STRING length.\n\nExamples:\n  (let ((s (string-copy \"hello\"))) (string-fill! s #\\x) s)  => \"xxxxx\"", ParamNames: []string{"string", "char"}, Category: "strings"},
		{Name: "string-upcase", ParamCount: 1, Impl: PrimStringUpcase,
			Doc: "Returns a new string with all characters converted to uppercase using full Unicode case mapping.\n\nExamples:\n  (string-upcase \"hello\")  => \"HELLO\"\n  (string-upcase \"straße\") => \"STRASSE\"", ParamNames: []string{"string"}, Category: "strings",
			Keywords: []string{"uppercase", "toupper", "capitalize"}},
		{Name: "string-downcase", ParamCount: 1, Impl: PrimStringDowncase,
			Doc: "Returns a new string with all characters converted to lowercase using full Unicode case mapping.\n\nExamples:\n  (string-downcase \"HELLO\")  => \"hello\"", ParamNames: []string{"string"}, Category: "strings",
			Keywords: []string{"lowercase", "tolower"}},
		{Name: "string-foldcase", ParamCount: 1, Impl: PrimStringFoldcase,
			Doc: "Returns a new string with full Unicode case folding applied. Useful for case-insensitive comparison.\n\nExamples:\n  (string-foldcase \"HELLO\")  => \"hello\"\n  (string-foldcase \"Straße\") => \"strasse\"", ParamNames: []string{"string"}, Category: "strings"},
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
			Doc: "Returns #t if CHAR is a Unicode letter (Lu, Ll, Lt, Lm, Lo categories).\n\nExamples:\n  (char-alphabetic? #\\a)  => #t\n  (char-alphabetic? #\\1)  => #f", ParamNames: []string{"char"}, Category: "characters",
			Keywords: []string{"letter", "alpha", "is-letter"}},
		{Name: "char-numeric?", ParamCount: 1, Impl: PrimCharNumericQ,
			Doc: "Returns #t if CHAR is a Unicode decimal digit.\n\nExamples:\n  (char-numeric? #\\5)  => #t\n  (char-numeric? #\\a)  => #f", ParamNames: []string{"char"}, Category: "characters",
			Keywords: []string{"digit", "is-digit", "number character"}},
		{Name: "char-whitespace?", ParamCount: 1, Impl: PrimCharWhitespaceQ,
			Doc: "Returns #t if CHAR is a Unicode whitespace character.\n\nExamples:\n  (char-whitespace? #\\space)  => #t\n  (char-whitespace? #\\a)      => #f", ParamNames: []string{"char"}, Category: "characters",
			Keywords: []string{"space", "blank", "is-space"}},
		{Name: "char-upper-case?", ParamCount: 1, Impl: PrimCharUpperCaseQ,
			Doc: "Returns #t if CHAR is an uppercase Unicode letter.\n\nExamples:\n  (char-upper-case? #\\A)  => #t\n  (char-upper-case? #\\a)  => #f", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-lower-case?", ParamCount: 1, Impl: PrimCharLowerCaseQ,
			Doc: "Returns #t if CHAR is a lowercase Unicode letter.\n\nExamples:\n  (char-lower-case? #\\a)  => #t\n  (char-lower-case? #\\A)  => #f", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "char-upcase", ParamCount: 1, Impl: PrimCharUpcase,
			Doc: "Returns the uppercase form of CHAR. Simple (1:1) Unicode case mapping.\n\nExamples:\n  (char-upcase #\\a)  => #\\A", ParamNames: []string{"char"}, Category: "characters",
			Keywords: []string{"uppercase", "toupper"}},
		{Name: "char-downcase", ParamCount: 1, Impl: PrimCharDowncase,
			Doc: "Returns the lowercase form of CHAR. Simple (1:1) Unicode case mapping.\n\nExamples:\n  (char-downcase #\\A)  => #\\a", ParamNames: []string{"char"}, Category: "characters",
			Keywords: []string{"lowercase", "tolower"}},
		{Name: "char-foldcase", ParamCount: 1, Impl: PrimCharFoldcase,
			Doc: "Returns the case-folded form of CHAR for case-insensitive comparison.\n\nExamples:\n  (char-foldcase #\\A)  => #\\a", ParamNames: []string{"char"}, Category: "characters"},
		{Name: "digit-value", ParamCount: 1, Impl: PrimDigitValue,
			Doc: "Returns the numeric value (0-9) of a Unicode decimal digit character, or #f if not a digit.\n\nExamples:\n  (digit-value #\\3)  => 3\n  (digit-value #\\a)  => #f", ParamNames: []string{"char"}, Category: "characters"},
	}, registry.PhaseRuntime)
	return nil
}
