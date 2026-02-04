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

//nolint:govet // Using unkeyed struct fields for concise primitive specs
package core

import (
	"github.com/aalpar/wile/registry"
)

func addBoxes(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"box", 1, false, PrimBox},
		{"box?", 1, false, PrimBoxQ},
		{"unbox", 1, false, PrimUnbox},
		{"set-box!", 2, false, PrimSetBox},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
