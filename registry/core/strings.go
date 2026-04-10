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

package core

import (
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)

func addStrings(r *registry.Registry) error {
	// String construction
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string", ParamCount: 1, IsVariadic: true, Impl: PrimString,
			Doc: "Returns a string composed of its character arguments. All arguments must be characters.\n\nExamples:\n  (string #\\a #\\b #\\c)  => \"abc\"", ParamNames: []string{"char"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeCharacter}, ReturnType: values.TypeString},
		{Name: "make-string", ParamCount: 2, IsVariadic: true, Impl: PrimMakeString,
			Doc: "Returns a string of length K. If CHAR is given, each position is filled with CHAR; otherwise unspecified.\n\nExamples:\n  (make-string 3 #\\a)    => \"aaa\"", ParamNames: []string{"k", "char"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeExactInteger, values.TypeCharacter}, ReturnType: values.TypeString},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String access
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string-length", ParamCount: 1, Impl: PrimStringLength,
			Doc: "Returns the number of characters in STRING.\n\nExamples:\n  (string-length \"hello\")  => 5\n  (string-length \"\")       => 0", ParamNames: []string{"string"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeString}, ReturnType: values.TypeExactInteger},
		{Name: "string-ref", ParamCount: 2, Impl: PrimStringRef,
			Doc: "Returns the character at 0-based index K in STRING. Raises an error if K is out of range.\n\nExamples:\n  (string-ref \"hello\" 0)  => #\\h\n  (string-ref \"hello\" 4)  => #\\o", ParamNames: []string{"string", "k"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeExactInteger}, ReturnType: values.TypeCharacter},
		{Name: "string-set!", ParamCount: 3, Impl: PrimStringSet,
			Doc: "Stores CHAR at 0-based index K in STRING. STRING must be mutable.\n\nExamples:\n  (let ((s (string-copy \"hello\"))) (string-set! s 0 #\\H) s)  => \"Hello\"", ParamNames: []string{"string", "k", "char"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeExactInteger, values.TypeCharacter}, ReturnType: values.TypeVoid},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string->list", ParamCount: 2, IsVariadic: true, Impl: PrimStringToList,
			Doc: "Returns a list of the characters in STRING from START to end. START defaults to 0, end to string length.\n\nExamples:\n  (string->list \"abc\")  => (#\\a #\\b #\\c)", ParamNames: []string{"string", "start"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeExactInteger}, ReturnType: values.TypeList,
			Keywords: []string{"split", "explode", "characters"}},
		{Name: "list->string", ParamCount: 1, Impl: PrimListToString,
			Doc: "Returns a string formed from LIST. All elements must be characters.\n\nExamples:\n  (list->string '(#\\a #\\b #\\c))  => \"abc\"", ParamNames: []string{"list"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeList}, ReturnType: values.TypeString,
			Keywords: []string{"join", "implode", "from characters"}},
		{Name: "symbol->string", ParamCount: 1, Impl: PrimSymbolToString,
			Doc: "Returns the name of SYMBOL as an immutable string.\n\nExamples:\n  (symbol->string 'hello)  => \"hello\"", ParamNames: []string{"symbol"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeSymbol}, ReturnType: values.TypeString,
			Keywords: []string{"symbol name", "identifier name"}},
		{Name: "string->symbol", ParamCount: 1, Impl: PrimStringToSymbol,
			Doc: "Returns the symbol whose name is STRING. Symbols with the same name are always eq?.\n\nExamples:\n  (string->symbol \"hello\")  => hello", ParamNames: []string{"string"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeString}, ReturnType: values.TypeSymbol,
			Keywords: []string{"intern", "make symbol"}},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string-append", ParamCount: 1, IsVariadic: true, Impl: PrimStringAppend,
			Doc: "Returns a newly allocated string formed by concatenating its arguments.\n\nExamples:\n  (string-append \"hello\" \" \" \"world\")  => \"hello world\"", ParamNames: []string{"string"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeString}, ReturnType: values.TypeString,
			Keywords: []string{"concat", "concatenate", "join"}},
		{Name: "substring", ParamCount: 3, Impl: PrimSubstring,
			Doc: "Returns a newly allocated string containing characters of STRING from START (inclusive) to END (exclusive).\n\nExamples:\n  (substring \"hello\" 1 3)  => \"el\"", ParamNames: []string{"string", "start", "end"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeExactInteger, values.TypeExactInteger}, ReturnType: values.TypeString,
			Keywords: []string{"slice", "substr", "extract", "range"}},
		{Name: "string-copy", ParamCount: 2, IsVariadic: true, Impl: PrimStringCopy,
			Doc: "Returns a mutable copy of STRING from START to end. START defaults to 0, end to string length.\n\nExamples:\n  (string-copy \"hello\")      => \"hello\"\n  (string-copy \"hello\" 1 3)  => \"el\"", ParamNames: []string{"string", "start"}, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeExactInteger}, ReturnType: values.TypeString,
			Keywords: []string{"clone", "duplicate"}},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String comparison (generated from stringCompareSpecs table)
	stringCmpPrims := make([]registry.PrimitiveSpec, len(stringCompareSpecs))
	for i, spec := range stringCompareSpecs {
		stringCmpPrims[i] = registry.PrimitiveSpec{
			Name: spec.name, ParamCount: 2, IsVariadic: true,
			Impl: makeStringComparePrimitive(spec.name, spec.cmp),
			Doc:  spec.doc, Category: "strings",
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeString},
			ReturnType: values.TypeBoolean,
		}
	}
	r.AddPrimitives(stringCmpPrims, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
