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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
)

// TestBytevectors_ArityErrors verifies that bytevector primitives reject
// wrong argument counts. The VM enforces arity automatically via
// machine_context.go ("expected %d arguments, got %d").
func TestBytevectors_ArityErrors(t *testing.T) {
	fixedArityErrors := []struct {
		name string
		code string
	}{
		// bytevector-length: exactly 1 arg
		{name: "bytevector-length zero args", code: `(bytevector-length)`},
		{name: "bytevector-length two args", code: `(bytevector-length #u8() #u8())`},
		// bytevector-u8-ref: exactly 2 args
		{name: "bytevector-u8-ref zero args", code: `(bytevector-u8-ref)`},
		{name: "bytevector-u8-ref one arg", code: `(bytevector-u8-ref #u8(1))`},
		{name: "bytevector-u8-ref three args", code: `(bytevector-u8-ref #u8(1) 0 0)`},
		// bytevector-u8-set!: exactly 3 args
		{name: "bytevector-u8-set! zero args", code: `(bytevector-u8-set!)`},
		{name: "bytevector-u8-set! one arg", code: `(bytevector-u8-set! #u8(1))`},
		{name: "bytevector-u8-set! two args", code: `(bytevector-u8-set! #u8(1) 0)`},
		{name: "bytevector-u8-set! four args", code: `(bytevector-u8-set! #u8(1) 0 2 3)`},
	}
	for _, tc := range fixedArityErrors {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}

	// Variadic with minimum 1 arg: too few args
	variadicMin1 := []string{
		"make-bytevector",
		"bytevector-copy",
		"utf8->string",
		"string->utf8",
	}
	for _, name := range variadicMin1 {
		t.Run(name+" zero args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+")")
		})
	}

	// Variadic with minimum 3 args: too few args
	variadicMin3Errors := []struct {
		name string
		code string
	}{
		{name: "bytevector-copy! zero args", code: `(bytevector-copy!)`},
		{name: "bytevector-copy! one arg", code: `(bytevector-copy! #u8(1))`},
		{name: "bytevector-copy! two args", code: `(bytevector-copy! #u8(1) 0)`},
	}
	for _, tc := range variadicMin3Errors {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}
