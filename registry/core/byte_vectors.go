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

func addBytevectors(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-bytevector", ParamCount: 2, IsVariadic: true, Impl: PrimMakeBytevector,
			Doc: "Creates a bytevector of length k, optionally filled with byte.", ParamNames: []string{"k", "byte"}, Category: "bytevectors"},
		{Name: "bytevector", ParamCount: 1, IsVariadic: true, Impl: PrimBytevector,
			Doc: "Creates a bytevector from its arguments.", ParamNames: []string{"byte"}, Category: "bytevectors"},
		{Name: "bytevector-length", ParamCount: 1, Impl: PrimBytevectorLength,
			Doc: "Returns the length of bytevector.", ParamNames: []string{"bytevector"}, Category: "bytevectors"},
		{Name: "bytevector-u8-ref", ParamCount: 2, Impl: PrimBytevectorU8Ref,
			Doc: "Returns the byte at index k.", ParamNames: []string{"bytevector", "k"}, Category: "bytevectors"},
		{Name: "bytevector-u8-set!", ParamCount: 3, Impl: PrimBytevectorU8Set,
			Doc: "Sets the byte at index k.", ParamNames: []string{"bytevector", "k", "byte"}, Category: "bytevectors"},
		{Name: "bytevector-copy", ParamCount: 2, IsVariadic: true, Impl: PrimBytevectorCopy,
			Doc: "Returns a copy of bytevector, optionally from start to end.", ParamNames: []string{"bytevector", "start"}, Category: "bytevectors"},
		{Name: "bytevector-copy!", ParamCount: 4, IsVariadic: true, Impl: PrimBytevectorCopyBang,
			Doc: "Copies bytes from source to destination bytevector.", ParamNames: []string{"to", "at", "from", "start"}, Category: "bytevectors"},
		{Name: "bytevector-append", ParamCount: 1, IsVariadic: true, Impl: PrimBytevectorAppend,
			Doc: "Appends bytevectors together.", ParamNames: []string{"bytevector"}, Category: "bytevectors"},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// UTF-8 conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "utf8->string", ParamCount: 2, IsVariadic: true, Impl: PrimUtf8ToString,
			Doc: "Decodes a UTF-8 bytevector to a string.", ParamNames: []string{"bytevector", "start"}, Category: "bytevectors"},
		{Name: "string->utf8", ParamCount: 2, IsVariadic: true, Impl: PrimStringToUtf8,
			Doc: "Encodes a string to a UTF-8 bytevector.", ParamNames: []string{"string", "start"}, Category: "bytevectors"},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
