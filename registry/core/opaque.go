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

func addOpaque(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "opaque?", ParamCount: 1, Impl: PrimOpaqueQ,
			Doc: "Returns #t if obj is an opaque value.", ParamNames: []string{"obj"}, Category: "opaque",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "opaque-tag", ParamCount: 1, Impl: PrimOpaqueTag,
			Doc: "Returns the tag of an opaque value as a symbol.", ParamNames: []string{"obj"}, Category: "opaque",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeSymbol},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
