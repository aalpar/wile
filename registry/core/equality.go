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
)

func addEquality(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "eq?", ParamCount: 2, Impl: PrimEqQ,
			Doc: "Returns #t if obj1 and obj2 are the same object.", ParamNames: []string{"obj1", "obj2"}, Category: "equality"},
		{Name: "eqv?", ParamCount: 2, Impl: PrimEqvQ,
			Doc: "Returns #t if obj1 and obj2 are equivalent.", ParamNames: []string{"obj1", "obj2"}, Category: "equality"},
		{Name: "equal?", ParamCount: 2, Impl: PrimEqualQ,
			Doc: "Returns #t if obj1 and obj2 have the same structure and contents.", ParamNames: []string{"obj1", "obj2"}, Category: "equality"},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
