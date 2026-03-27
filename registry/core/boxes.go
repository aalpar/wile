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

func addBoxes(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "box", ParamCount: 1, Impl: PrimBox,
			Doc: "Wraps a value in a mutable box.", ParamNames: []string{"obj"}, Category: "boxes",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "box?", ParamCount: 1, Impl: PrimBoxQ,
			Doc: "Returns #t if obj is a box.", ParamNames: []string{"obj"}, Category: "boxes",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "unbox", ParamCount: 1, Impl: PrimUnbox,
			Doc: "Returns the value stored in a box.", ParamNames: []string{"box"}, Category: "boxes",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "set-box!", ParamCount: 2, Impl: PrimSetBox,
			Doc: "Stores a new value in a box.", ParamNames: []string{"box", "obj"}, Category: "boxes",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypeVoid},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
