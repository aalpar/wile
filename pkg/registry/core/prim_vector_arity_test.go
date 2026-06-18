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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// TestVectors_ArityErrors verifies that vector primitives reject wrong
// argument counts. The VM enforces arity automatically via
// machine_context.go ("expected %d arguments, got %d").
func TestVectors_ArityErrors(t *testing.T) {
	fixedArityErrors := []struct {
		name string
		code string
	}{
		// vector-length: exactly 1 arg
		{name: "vector-length zero args", code: `(vector-length)`},
		{name: "vector-length two args", code: `(vector-length #() #())`},
		// vector-ref: exactly 2 args
		{name: "vector-ref zero args", code: `(vector-ref)`},
		{name: "vector-ref one arg", code: `(vector-ref #())`},
		{name: "vector-ref three args", code: `(vector-ref #() 0 1)`},
		// vector-set!: exactly 3 args
		{name: "vector-set! zero args", code: `(vector-set!)`},
		{name: "vector-set! one arg", code: `(vector-set! #(1))`},
		{name: "vector-set! two args", code: `(vector-set! #(1) 0)`},
		{name: "vector-set! four args", code: `(vector-set! #(1) 0 2 3)`},
		// list->vector: exactly 1 arg
		{name: "list->vector zero args", code: `(list->vector)`},
		{name: "list->vector two args", code: `(list->vector '() '())`},
	}
	for _, tc := range fixedArityErrors {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}

	// Variadic with minimum 1 arg: too few args
	variadicMin1 := []string{
		"make-vector",
		"vector->list",
		"vector-copy",
		"vector-map",
		"vector-for-each",
		"vector->string",
		"string->vector",
	}
	for _, name := range variadicMin1 {
		t.Run(name+" zero args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+")")
		})
	}

	// Variadic with minimum 2 args: too few args
	variadicMin2Errors := []struct {
		name string
		code string
	}{
		{name: "vector-copy! zero args", code: `(vector-copy!)`},
		{name: "vector-copy! one arg", code: `(vector-copy! #(1))`},
		{name: "vector-fill! zero args", code: `(vector-fill!)`},
		{name: "vector-fill! one arg", code: `(vector-fill! #(1))`},
	}
	for _, tc := range variadicMin2Errors {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}
