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
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "syntax-source", ParamCount: 1, Impl: PrimSyntaxSource,
			Doc: "Returns the source file name of a syntax object as a string, or #f if unavailable.", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "syntax-line", ParamCount: 1, Impl: PrimSyntaxLine,
			Doc: "Returns the 1-based line number of a syntax object, or #f if unavailable.", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "syntax-column", ParamCount: 1, Impl: PrimSyntaxColumn,
			Doc: "Returns the 0-based column number of a syntax object, or #f if unavailable.", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "syntax-position", ParamCount: 1, Impl: PrimSyntaxPosition,
			Doc: "Returns the 0-based byte offset of a syntax object in its source, or #f if unavailable.", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "syntax-span", ParamCount: 1, Impl: PrimSyntaxSpan,
			Doc: "Returns the byte span (length) of a syntax object in its source, or #f if unavailable.", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "syntax->list", ParamCount: 1, Impl: PrimSyntaxToList,
			Doc: "Converts a syntax pair chain to a list of individual syntax objects. Returns #f if not a proper syntax list.", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
