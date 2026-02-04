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

func addBytevectors(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-bytevector", 2, true, PrimMakeBytevector},
		{"bytevector", 1, true, PrimBytevector},
		{"bytevector-length", 1, false, PrimBytevectorLength},
		{"bytevector-u8-ref", 2, false, PrimBytevectorU8Ref},
		{"bytevector-u8-set!", 3, false, PrimBytevectorU8Set},
		{"bytevector-copy", 2, true, PrimBytevectorCopy},
		{"bytevector-copy!", 4, true, PrimBytevectorCopyBang},
		{"bytevector-append", 1, true, PrimBytevectorAppend},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// UTF-8 conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"utf8->string", 2, true, PrimUtf8ToString},
		{"string->utf8", 2, true, PrimStringToUtf8},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
