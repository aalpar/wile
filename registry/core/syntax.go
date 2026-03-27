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

func addSyntax(r *registry.Registry) error {
	// Syntax objects (R6RS syntax-case support)
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "identifier?", ParamCount: 1, Impl: PrimIdentifierQ,
			Doc: "Returns #t if obj is an identifier.", ParamNames: []string{"obj"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "syntax->datum", ParamCount: 1, Impl: PrimSyntaxToDatum,
			Doc: "Strips syntax information from a syntax object.", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "datum->syntax", ParamCount: 2, Impl: PrimDatumToSyntax,
			Doc: "Converts a datum to a syntax object with the given context.", ParamNames: []string{"template-id", "datum"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}},
		{Name: "generate-temporaries", ParamCount: 1, Impl: PrimGenerateTemporaries,
			Doc: "Generates a list of unique temporary identifiers.", ParamNames: []string{"stx-list"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeList},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Identifier comparison
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "bound-identifier=?", ParamCount: 2, Impl: PrimBoundIdentifierEqualQ,
			Doc: "Returns #t if two identifiers have the same binding.", ParamNames: []string{"id1", "id2"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "free-identifier=?", ParamCount: 2, Impl: PrimFreeIdentifierEqualQ,
			Doc: "Returns #t if two identifiers refer to the same binding.", ParamNames: []string{"id1", "id2"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
