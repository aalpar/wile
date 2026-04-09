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
			Doc: "Returns a bytevector of length K. If BYTE is given, each element is BYTE (0-255); otherwise 0.\n\nExamples:\n  (make-bytevector 3 0)    => #u8(0 0 0)\n  (make-bytevector 3 255)  => #u8(255 255 255)", ParamNames: []string{"k", "byte"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeExactInteger, values.TypeByte}, ReturnType: values.TypeByteVector},
		{Name: "bytevector", ParamCount: 1, IsVariadic: true, Impl: PrimBytevector,
			Doc: "Returns a bytevector whose elements are its arguments. Each argument must be an exact integer 0-255.\n\nExamples:\n  (bytevector 1 2 3)     => #u8(1 2 3)", ParamNames: []string{"byte"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByte}, ReturnType: values.TypeByteVector},
		{Name: "bytevector-length", ParamCount: 1, Impl: PrimBytevectorLength,
			Doc: "Returns the number of bytes in BYTEVECTOR.\n\nExamples:\n  (bytevector-length #u8(1 2 3))  => 3", ParamNames: []string{"bytevector"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector}, ReturnType: values.TypeExactInteger},
		{Name: "bytevector-u8-ref", ParamCount: 2, Impl: PrimBytevectorU8Ref,
			Doc: "Returns the byte at 0-based index K in BYTEVECTOR as an exact integer (0-255). Raises an error if K is out of range.\n\nExamples:\n  (bytevector-u8-ref #u8(10 20 30) 1)  => 20", ParamNames: []string{"bytevector", "k"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector, values.TypeExactInteger}, ReturnType: values.TypeByte},
		{Name: "bytevector-u8-set!", ParamCount: 3, Impl: PrimBytevectorU8Set,
			Doc: "Stores BYTE (0-255) at 0-based index K in BYTEVECTOR. Raises an error if K is out of range.\n\nExamples:\n  (let ((bv (bytevector 1 2 3))) (bytevector-u8-set! bv 1 99) bv)  => #u8(1 99 3)", ParamNames: []string{"bytevector", "k", "byte"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector, values.TypeExactInteger, values.TypeByte}, ReturnType: values.TypeVoid},
		{Name: "bytevector-copy", ParamCount: 2, IsVariadic: true, Impl: PrimBytevectorCopy,
			Doc: "Returns a fresh copy of BYTEVECTOR bytes from START to end. START defaults to 0, end to bytevector length.\n\nExamples:\n  (bytevector-copy #u8(1 2 3))  => #u8(1 2 3)", ParamNames: []string{"bytevector", "start"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector, values.TypeExactInteger}, ReturnType: values.TypeByteVector},
		{Name: "bytevector-copy!", ParamCount: 4, IsVariadic: true, Impl: PrimBytevectorCopyBang,
			Doc: "Copies bytes from FROM into TO starting at index AT.\n\nExamples:\n  (let ((bv (bytevector 0 0 0))) (bytevector-copy! bv 0 #u8(1 2) 0 2) bv)  => #u8(1 2 0)", ParamNames: []string{"to", "at", "from", "start"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector, values.TypeExactInteger, values.TypeByteVector, values.TypeExactInteger}, ReturnType: values.TypeVoid},
		{Name: "bytevector-append", ParamCount: 1, IsVariadic: true, Impl: PrimBytevectorAppend,
			Doc: "Returns a newly allocated bytevector whose bytes are the concatenation of the argument bytevectors.\n\nExamples:\n  (bytevector-append #u8(1 2) #u8(3 4))  => #u8(1 2 3 4)", ParamNames: []string{"bytevector"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector}, ReturnType: values.TypeByteVector},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// UTF-8 conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "utf8->string", ParamCount: 2, IsVariadic: true, Impl: PrimUtf8ToString,
			Doc: "Decodes BYTEVECTOR as UTF-8 to a string from START to end. Raises an error on invalid UTF-8.\n\nExamples:\n  (utf8->string #u8(104 105))  => \"hi\"", ParamNames: []string{"bytevector", "start"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeByteVector, values.TypeExactInteger}, ReturnType: values.TypeString,
			Keywords: []string{"decode", "bytes to text", "UTF-8 decode"}},
		{Name: "string->utf8", ParamCount: 2, IsVariadic: true, Impl: PrimStringToUtf8,
			Doc: "Encodes the characters of STRING from START to end as a UTF-8 bytevector.\n\nExamples:\n  (string->utf8 \"hi\")  => #u8(104 105)", ParamNames: []string{"string", "start"}, Category: "bytevectors",
			ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger}, ReturnType: values.TypeByteVector,
			Keywords: []string{"encode", "text to bytes", "UTF-8 encode"}},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
