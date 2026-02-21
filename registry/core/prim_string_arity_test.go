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

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestStrings_ArityErrors verifies that fixed-arity and minimum-arity string
// primitives reject wrong argument counts.
func TestStrings_ArityErrors(t *testing.T) {
	// Fixed-arity: too few and too many args
	fixedArityErrors := []struct {
		name string
		code string
	}{
		// string-length: exactly 1 arg
		{name: "string-length zero args", code: `(string-length)`},
		{name: "string-length two args", code: `(string-length "a" "b")`},
		// string-ref: exactly 2 args
		{name: "string-ref zero args", code: `(string-ref)`},
		{name: "string-ref one arg", code: `(string-ref "a")`},
		{name: "string-ref three args", code: `(string-ref "a" 0 1)`},
		// string-set!: exactly 3 args
		{name: "string-set! zero args", code: `(string-set!)`},
		{name: "string-set! one arg", code: `(string-set! "a")`},
		{name: "string-set! two args", code: `(string-set! "a" 0)`},
		{name: "string-set! four args", code: `(string-set! "a" 0 #\b #\c)`},
		// list->string: exactly 1 arg
		{name: "list->string zero args", code: `(list->string)`},
		{name: "list->string two args", code: `(list->string '() '())`},
		// symbol->string: exactly 1 arg
		{name: "symbol->string zero args", code: `(symbol->string)`},
		{name: "symbol->string two args", code: `(symbol->string 'a 'b)`},
		// string->symbol: exactly 1 arg
		{name: "string->symbol zero args", code: `(string->symbol)`},
		{name: "string->symbol two args", code: `(string->symbol "a" "b")`},
		// substring: exactly 3 args
		{name: "substring zero args", code: `(substring)`},
		{name: "substring one arg", code: `(substring "a")`},
		{name: "substring two args", code: `(substring "a" 0)`},
		{name: "substring four args", code: `(substring "a" 0 1 2)`},
	}
	for _, tc := range fixedArityErrors {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}

	// Variadic with minimum: too few args
	variadicMinErrors := []struct {
		name string
		code string
	}{
		// string->list: min 1 arg (string [start [end]])
		{name: "string->list zero args", code: `(string->list)`},
		// string-copy: min 1 arg (string [start [end]])
		{name: "string-copy zero args", code: `(string-copy)`},
	}
	for _, tc := range variadicMinErrors {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// TestStrings_VariadicZeroArgs verifies that variadic string primitives
// accepting zero arguments produce the correct results per R7RS.
func TestStrings_VariadicZeroArgs(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected values.Value
	}{
		// R7RS §6.7: (string) returns ""
		{name: "string zero args", code: `(string)`, expected: values.NewString("")},
		// R7RS §6.7: (string-append) returns ""
		{name: "string-append zero args", code: `(string-append)`, expected: values.NewString("")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}
