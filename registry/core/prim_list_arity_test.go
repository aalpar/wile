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

// TestLists_ArityErrors verifies that list and pair primitives reject
// wrong argument counts. The VM enforces arity automatically via
// machine_context.go ("expected %d arguments, got %d").
func TestLists_ArityErrors(t *testing.T) {
	// Fixed 1 arg: car, cdr, reverse, length, list-copy
	fixedArity1 := []string{
		"car",
		"cdr",
		"reverse",
		"length",
		"list-copy",
	}
	for _, name := range fixedArity1 {
		t.Run(name+" zero args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+")")
		})
		t.Run(name+" two args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+" '(1) '(2))")
		})
	}

	// Fixed 2 args: cons, memq, memv, assq, assv, list-ref, list-tail
	fixedArity2 := []string{
		"cons",
		"memq",
		"memv",
		"assq",
		"assv",
		"list-ref",
		"list-tail",
	}
	for _, name := range fixedArity2 {
		t.Run(name+" zero args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+")")
		})
		t.Run(name+" one arg", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+" 1)")
		})
		t.Run(name+" three args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+" 1 2 3)")
		})
	}

	// Fixed 3 args: list-set!
	listSetErrors := []struct {
		name string
		code string
	}{
		{name: "list-set! zero args", code: `(list-set!)`},
		{name: "list-set! one arg", code: `(list-set! '(1))`},
		{name: "list-set! two args", code: `(list-set! '(1) 0)`},
		{name: "list-set! four args", code: `(list-set! '(1) 0 2 3)`},
	}
	for _, tc := range listSetErrors {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}

	// Variadic with minimum 2 args: member, assoc
	variadicMin2Errors := []struct {
		name string
		code string
	}{
		{name: "member zero args", code: `(member)`},
		{name: "member one arg", code: `(member 1)`},
		{name: "assoc zero args", code: `(assoc)`},
		{name: "assoc one arg", code: `(assoc 1)`},
	}
	for _, tc := range variadicMin2Errors {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}

	// Variadic with minimum 1 arg: make-list
	t.Run("make-list zero args", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, "(make-list)")
	})
}

// TestCxR_ArityErrors verifies that all 28 CxR accessors (2/3/4-level
// compositions of car and cdr) reject wrong argument counts.
func TestCxR_ArityErrors(t *testing.T) {
	cxrs := []string{
		// 2-level (4)
		"caar", "cadr", "cdar", "cddr",
		// 3-level (8)
		"caaar", "caadr", "cadar", "caddr",
		"cdaar", "cdadr", "cddar", "cdddr",
		// 4-level (16)
		"caaaar", "caaadr", "caadar", "caaddr",
		"cadaar", "cadadr", "caddar", "cadddr",
		"cdaaar", "cdaadr", "cdadar", "cdaddr",
		"cddaar", "cddadr", "cdddar", "cddddr",
	}

	for _, name := range cxrs {
		t.Run(name+" zero args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+")")
		})
		t.Run(name+" two args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+" '(1) '(2))")
		})
	}
}
