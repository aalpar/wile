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

func addPairs(r *registry.Registry) error {
	// Basic pair operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "cons", ParamCount: 2, Impl: PrimCons,
			Doc: "Creates a new pair.", ParamNames: []string{"obj1", "obj2"}, Category: "pairs"},
		{Name: "car", ParamCount: 1, Impl: PrimCar,
			Doc: "Returns the car of a pair.", ParamNames: []string{"pair"}, Category: "pairs"},
		{Name: "cdr", ParamCount: 1, Impl: PrimCdr,
			Doc: "Returns the cdr of a pair.", ParamNames: []string{"pair"}, Category: "pairs"},
		{Name: "set-car!", ParamCount: 2, Impl: PrimSetCar,
			Doc: "Sets the car of a pair.", ParamNames: []string{"pair", "obj"}, Category: "pairs"},
		{Name: "set-cdr!", ParamCount: 2, Impl: PrimSetCdr,
			Doc: "Sets the cdr of a pair.", ParamNames: []string{"pair", "obj"}, Category: "pairs"},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
