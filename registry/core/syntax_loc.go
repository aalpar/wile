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

func addSyntaxLoc(r *registry.Registry) error {
	// Every accessor below returns a concrete value (string, integer, or list)
	// OR #f when location information is unavailable. The union is not
	// representable by a single ValueType, so ReturnType is TypeAny.
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "syntax-source", ParamCount: 1, Impl: PrimSyntaxSource,
			Doc: "Returns the source file name of STX as a string, or #f if unavailable.\n\nExamples:\n  ;; (syntax-source #'foo)  => \"test.scm\" or #f", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "syntax-line", ParamCount: 1, Impl: PrimSyntaxLine,
			Doc: "Returns the 1-based line number of STX, or #f if unavailable.\n\nExamples:\n  ;; (syntax-line #'foo)  => 5 or #f", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "syntax-column", ParamCount: 1, Impl: PrimSyntaxColumn,
			Doc: "Returns the 0-based column number of STX, or #f if unavailable.\n\nExamples:\n  ;; (syntax-column #'foo)  => 3 or #f", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "syntax-position", ParamCount: 1, Impl: PrimSyntaxPosition,
			Doc: "Returns the 0-based byte offset of STX in its source, or #f if unavailable.\n\nExamples:\n  ;; (syntax-position #'foo)  => 42 or #f", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "syntax-span", ParamCount: 1, Impl: PrimSyntaxSpan,
			Doc: "Returns the byte span (length) of STX in its source, or #f if unavailable.\n\nExamples:\n  ;; (syntax-span #'foo)  => 3 or #f", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "syntax->list", ParamCount: 1, Impl: PrimSyntaxToList,
			Doc: "Converts STX from a syntax pair chain to a list of individual syntax objects. Returns #f if not a proper syntax list.\n\nExamples:\n  ;; (length (syntax->list #'(a b c)))  => 3", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
