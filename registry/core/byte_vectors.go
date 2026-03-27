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

func addBytevectors(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-bytevector", ParamCount: 2, IsVariadic: true, Impl: PrimMakeBytevector,
			Doc: "Returns a bytevector of length k. If byte is given, each element is byte (0-255); otherwise 0.", ParamNames: []string{"k", "byte"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeExactInteger, values.TypeByte}, ReturnType: values.TypeByteVector},
		{Name: "bytevector", ParamCount: 1, IsVariadic: true, Impl: PrimBytevector,
			Doc: "Returns a bytevector whose elements are its arguments. Each argument must be an exact integer 0-255.", ParamNames: []string{"byte"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByte}, ReturnType: values.TypeByteVector},
		{Name: "bytevector-length", ParamCount: 1, Impl: PrimBytevectorLength,
			Doc: "Returns the number of bytes in bytevector.", ParamNames: []string{"bytevector"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector}, ReturnType: values.TypeExactInteger},
		{Name: "bytevector-u8-ref", ParamCount: 2, Impl: PrimBytevectorU8Ref,
			Doc: "Returns the byte at 0-based index k as an exact integer (0-255). Raises an error if k is out of range.", ParamNames: []string{"bytevector", "k"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector, values.TypeExactInteger}, ReturnType: values.TypeByte},
		{Name: "bytevector-u8-set!", ParamCount: 3, Impl: PrimBytevectorU8Set,
			Doc: "Stores byte (0-255) at 0-based index k in bytevector. Raises an error if k is out of range.", ParamNames: []string{"bytevector", "k", "byte"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector, values.TypeExactInteger, values.TypeByte}, ReturnType: values.TypeVoid},
		{Name: "bytevector-copy", ParamCount: 2, IsVariadic: true, Impl: PrimBytevectorCopy,
			Doc: "Returns a fresh copy of bytevector bytes from start to end. Start defaults to 0, end to bytevector length.", ParamNames: []string{"bytevector", "start"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector, values.TypeExactInteger}, ReturnType: values.TypeByteVector},
		{Name: "bytevector-copy!", ParamCount: 4, IsVariadic: true, Impl: PrimBytevectorCopyBang,
			Doc: "Copies bytes from the from bytevector into the to bytevector starting at index at.", ParamNames: []string{"to", "at", "from", "start"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector, values.TypeExactInteger, values.TypeByteVector, values.TypeExactInteger}, ReturnType: values.TypeVoid},
		{Name: "bytevector-append", ParamCount: 1, IsVariadic: true, Impl: PrimBytevectorAppend,
			Doc: "Returns a newly allocated bytevector whose bytes are the concatenation of the argument bytevectors.", ParamNames: []string{"bytevector"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector}, ReturnType: values.TypeByteVector},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// UTF-8 conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "utf8->string", ParamCount: 2, IsVariadic: true, Impl: PrimUtf8ToString,
			Doc: "Decodes a UTF-8 bytevector to a string from start to end. Raises an error on invalid UTF-8.", ParamNames: []string{"bytevector", "start"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector, values.TypeExactInteger}, ReturnType: values.TypeString},
		{Name: "string->utf8", ParamCount: 2, IsVariadic: true, Impl: PrimStringToUtf8,
			Doc: "Encodes the characters of string from start to end as a UTF-8 bytevector.", ParamNames: []string{"string", "start"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger}, ReturnType: values.TypeByteVector},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
